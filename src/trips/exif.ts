import fs from "node:fs";
import path from "node:path";
import exifr from "exifr";

export type PhotoExif = {
  capturedAt: Date;
  displayCapturedAt: string;
  /** WGS84 latitude, or null when GPS EXIF is absent (allowed for loose photos). */
  lat: number | null;
  /** WGS84 longitude, or null when GPS EXIF is absent (allowed for loose photos). */
  lon: number | null;
  /** Multi-line hover tooltip: date/time, exposure, optional GPS. */
  tooltip: string;
  /** Source file size in bytes (from the same stat used for the cache key). */
  sourceSize: number;
};

type CachedExifPayload = {
  capturedAt: string;
  displayCapturedAt: string;
  lat: number | null;
  lon: number | null;
  tooltip: string;
};

export type ReadPhotoExifOptions = {
  /** When true (default), missing GPS fails the read. Loose photos pass false. */
  requireGps?: boolean;
};

export function sourceKey(srcPath: string): { key: string; size: number } {
  const st = fs.statSync(srcPath);
  return { key: `${st.mtimeMs}_${st.size}`, size: st.size };
}

function formatExifLocal(dt: Date): string {
  const pad = (n: number) => String(n).padStart(2, "0");
  return `${dt.getFullYear()}-${pad(dt.getMonth() + 1)}-${pad(dt.getDate())} ${pad(dt.getHours())}:${pad(dt.getMinutes())}:${pad(dt.getSeconds())}`;
}

function formatAperture(fNumber: unknown): string | null {
  if (typeof fNumber !== "number" || !Number.isFinite(fNumber) || fNumber <= 0) {
    return null;
  }
  const rounded = Math.round(fNumber * 10) / 10;
  const text = Number.isInteger(rounded) ? String(rounded) : rounded.toFixed(1);
  return `f/${text}`;
}

function formatFocalLength(focalLength: unknown): string | null {
  if (
    typeof focalLength !== "number" ||
    !Number.isFinite(focalLength) ||
    !(focalLength > 0)
  ) {
    return null;
  }
  const rounded = Math.round(focalLength * 10) / 10;
  const text = Number.isInteger(rounded) ? String(rounded) : rounded.toFixed(1);
  return `${text}mm`;
}

function formatShutter(exposureTime: unknown): string | null {
  if (
    typeof exposureTime !== "number" ||
    !Number.isFinite(exposureTime) ||
    !(exposureTime > 0)
  ) {
    return null;
  }
  if (exposureTime >= 1) {
    const text = Number.isInteger(exposureTime)
      ? String(exposureTime)
      : exposureTime.toFixed(1);
    return `${text}s`;
  }
  return `1/${Math.round(1 / exposureTime)}`;
}

function formatIso(iso: unknown): string | null {
  const raw = Array.isArray(iso) ? iso[0] : iso;
  if (typeof raw !== "number" || !Number.isFinite(raw) || raw <= 0) return null;
  return `ISO ${Math.round(raw)}`;
}

function formatGps(lat: number, lon: number): string {
  return `${lat.toFixed(5)}, ${lon.toFixed(5)}`;
}

/** Pretty labels for known EXIF Model values; unknown models fall back to raw Model. */
const DEVICE_NAMES: Record<string, string> = {
  "ILCE-7M3": "Sony A7 III",
  "iPhone 12 Pro Max": "iPhone 12 Pro Max",
  "E-M10 Mark III": "Olympus E-M10 Mark III",
  "E-M10": "Olympus E-M10",
  "Mi A2 Lite": "Xiaomi Mi A2 Lite",
};

function formatDevice(model: unknown): string | null {
  if (typeof model !== "string") return null;
  const trimmed = model.trim();
  if (!trimmed) return null;
  return DEVICE_NAMES[trimmed] ?? trimmed;
}

/** Bump when tooltip/format fields change so cached EXIF payloads regenerate. */
const EXIF_CACHE_VERSION = "exif-v4";

export function formatExifTooltip(parts: {
  displayCapturedAt: string;
  device: string | null;
  focalLength: string | null;
  aperture: string | null;
  shutter: string | null;
  iso: string | null;
  lat: number | null;
  lon: number | null;
}): string {
  const lines = [parts.displayCapturedAt];
  const exposure = [
    parts.device,
    parts.focalLength,
    parts.aperture,
    parts.shutter,
    parts.iso,
  ]
    .filter(Boolean)
    .join(" · ");
  if (exposure) lines.push(exposure);
  if (
    typeof parts.lat === "number" &&
    typeof parts.lon === "number" &&
    Number.isFinite(parts.lat) &&
    Number.isFinite(parts.lon)
  ) {
    lines.push(formatGps(parts.lat, parts.lon));
  }
  return lines.join("\n");
}

function readCachedExif(
  cacheJsonPath: string,
  cacheKeyPath: string,
  key: string,
): CachedExifPayload | null {
  if (!fs.existsSync(cacheJsonPath) || !fs.existsSync(cacheKeyPath)) return null;
  if (fs.readFileSync(cacheKeyPath, "utf8") !== key) return null;
  try {
    const raw = JSON.parse(
      fs.readFileSync(cacheJsonPath, "utf8"),
    ) as Partial<CachedExifPayload>;
    if (
      typeof raw.capturedAt !== "string" ||
      typeof raw.displayCapturedAt !== "string" ||
      typeof raw.tooltip !== "string" ||
      !(
        (typeof raw.lat === "number" && typeof raw.lon === "number") ||
        (raw.lat === null && raw.lon === null)
      )
    ) {
      return null;
    }
    return {
      capturedAt: raw.capturedAt,
      displayCapturedAt: raw.displayCapturedAt,
      lat: raw.lat as number | null,
      lon: raw.lon as number | null,
      tooltip: raw.tooltip,
    };
  } catch {
    return null;
  }
}

async function parsePhotoExif(
  filePath: string,
  options: { requireGps: boolean },
): Promise<Omit<PhotoExif, "sourceSize">> {
  const label = path.relative(process.cwd(), filePath) || filePath;
  const buf = await fs.promises.readFile(filePath);

  let data: Record<string, unknown> | undefined;
  try {
    data = await exifr.parse(buf, {
      pick: [
        "DateTimeOriginal",
        "Model",
        "FocalLength",
        "FNumber",
        "ExposureTime",
        "ISO",
        "ISOSpeedRatings",
        "PhotographicSensitivity",
      ],
    });
  } catch (err) {
    const detail = err instanceof Error ? err.message : String(err);
    throw new Error(
      `Failed to read EXIF from ${label}: ${detail}. Photos must include DateTimeOriginal.`,
    );
  }

  const dt = data?.DateTimeOriginal;
  if (!(dt instanceof Date) || Number.isNaN(+dt)) {
    throw new Error(
      `Missing EXIF DateTimeOriginal in ${label}. Add a valid DateTimeOriginal tag (CreateDate/filename are not used).`,
    );
  }

  let lat: number | null = null;
  let lon: number | null = null;
  try {
    const gps = await exifr.gps(buf);
    if (
      gps &&
      typeof gps.latitude === "number" &&
      typeof gps.longitude === "number" &&
      Number.isFinite(gps.latitude) &&
      Number.isFinite(gps.longitude)
    ) {
      lat = gps.latitude;
      lon = gps.longitude;
    }
  } catch (err) {
    if (options.requireGps) {
      const detail = err instanceof Error ? err.message : String(err);
      throw new Error(
        `Failed to read GPS EXIF from ${label}: ${detail}. Each trip photo must include GPS latitude/longitude for the map.`,
      );
    }
  }

  if (options.requireGps && (lat == null || lon == null)) {
    throw new Error(
      `Missing GPS coordinates in ${label}. Each trip photo must include EXIF GPS latitude/longitude to appear on the trip map.`,
    );
  }

  const displayCapturedAt = formatExifLocal(dt);
  const device = formatDevice(data?.Model);
  const focalLength = formatFocalLength(data?.FocalLength);
  const aperture = formatAperture(data?.FNumber);
  const shutter = formatShutter(data?.ExposureTime);
  const iso = formatIso(
    data?.ISO ?? data?.ISOSpeedRatings ?? data?.PhotographicSensitivity,
  );

  return {
    capturedAt: dt,
    displayCapturedAt,
    lat,
    lon,
    tooltip: formatExifTooltip({
      displayCapturedAt,
      device,
      focalLength,
      aperture,
      shutter,
      iso,
      lat,
      lon,
    }),
  };
}

export async function readPhotoExif(
  filePath: string,
  cacheTripDir: string,
  options: ReadPhotoExifOptions = {},
): Promise<PhotoExif> {
  const requireGps = options.requireGps !== false;
  const { key: source, size } = sourceKey(filePath);
  const key = `${EXIF_CACHE_VERSION}_gps${requireGps ? "1" : "0"}_${source}`;
  const base = path.basename(filePath);
  const exifDir = path.join(cacheTripDir, "exif");
  const cacheJsonPath = path.join(exifDir, `${base}.json`);
  const cacheKeyPath = path.join(exifDir, `${base}.key`);

  const cached = readCachedExif(cacheJsonPath, cacheKeyPath, key);
  if (cached) {
    const capturedAt = new Date(cached.capturedAt);
    if (!Number.isNaN(+capturedAt)) {
      if (
        requireGps &&
        (cached.lat == null || cached.lon == null)
      ) {
        // Stale cache from a non-GPS read; re-parse.
      } else {
        return {
          capturedAt,
          displayCapturedAt: cached.displayCapturedAt,
          lat: cached.lat,
          lon: cached.lon,
          tooltip: cached.tooltip,
          sourceSize: size,
        };
      }
    }
  }

  const parsed = await parsePhotoExif(filePath, { requireGps });
  fs.mkdirSync(exifDir, { recursive: true });
  const payload: CachedExifPayload = {
    capturedAt: parsed.capturedAt.toISOString(),
    displayCapturedAt: parsed.displayCapturedAt,
    lat: parsed.lat,
    lon: parsed.lon,
    tooltip: parsed.tooltip,
  };
  fs.writeFileSync(cacheJsonPath, JSON.stringify(payload));
  fs.writeFileSync(cacheKeyPath, key);

  return { ...parsed, sourceSize: size };
}

export function formatIsoCapturedAt(date: Date): string {
  if (Number.isNaN(+date)) return "";
  return date.toISOString();
}

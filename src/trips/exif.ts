import fs from "node:fs";
import path from "node:path";
import exifr from "exifr";

export type PhotoExif = {
  capturedAt: Date;
  displayCapturedAt: string;
  lat: number | null;
  lon: number | null;
};

function formatExifLocal(dt: Date): string {
  const pad = (n: number) => String(n).padStart(2, "0");
  return `${dt.getFullYear()}-${pad(dt.getMonth() + 1)}-${pad(dt.getDate())} ${pad(dt.getHours())}:${pad(dt.getMinutes())}:${pad(dt.getSeconds())}`;
}

export async function readPhotoExif(filePath: string): Promise<PhotoExif> {
  const label = path.relative(process.cwd(), filePath) || filePath;
  const buf = await fs.promises.readFile(filePath);

  let data: Record<string, unknown> | undefined;
  try {
    data = await exifr.parse(buf, {
      pick: ["DateTimeOriginal"],
    });
  } catch (err) {
    const detail = err instanceof Error ? err.message : String(err);
    throw new Error(
      `Failed to read EXIF from ${label}: ${detail}. Each trip photo must include DateTimeOriginal.`,
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
      typeof gps.longitude === "number"
    ) {
      lat = gps.latitude;
      lon = gps.longitude;
    }
  } catch {
    // GPS remains optional for map markers
  }

  return {
    capturedAt: dt,
    displayCapturedAt: formatExifLocal(dt),
    lat,
    lon,
  };
}

export function formatIsoCapturedAt(date: Date): string {
  if (Number.isNaN(+date)) return "";
  return date.toISOString();
}

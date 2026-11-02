import fs from "node:fs";
import path from "node:path";
import sharp from "sharp";
import { latToTileY, lonToTileX } from "./geo.ts";
import type { LatLon } from "./types.ts";

/** Bump when changing DEM source/zoom so caches invalidate. */
export const ELEVATION_SOURCE_ID = "mapterhorn";
/**
 * Preferred Mapterhorn sample zoom. Regional lidar (e.g. UK 1 m) is available
 * here; areas with only the global layer fall back to `ELEVATION_ZOOM_MIN`.
 */
export const ELEVATION_ZOOM = 14;
/** Global planet coverage max zoom on Mapterhorn (no regional tiles). */
export const ELEVATION_ZOOM_MIN = 12;
/** Mapterhorn Terrarium tiles are 512×512 (vs Mapzen’s 256). */
const ELEVATION_TILE_SIZE = 512;
/**
 * Minimum elevation change (metres) counted toward ascent after smoothing.
 * Smaller zigzags / DEM noise are ignored via hysteresis around local extrema.
 */
export const ASCENT_THRESHOLD_M = 15;
/** Centered moving-average window applied before thresholded ascent (odd size). */
const ASCENT_SMOOTH_WINDOW = 11;

const USER_AGENT =
  "dzhus.org-blog-static-map/1.0 (https://dzhus.org; personal static site build)";

const TILE_URL = (z: number, x: number, y: number) =>
  `https://tiles.mapterhorn.com/${z}/${x}/${y}.webp`;

/** Min gap between outbound Mapterhorn HTTP requests. */
const FETCH_GAP_MS = 80;

/** In-flight tile fetches keyed by `z/x/y` (null = confirmed missing / 404). */
const tileInflight = new Map<string, Promise<Buffer | null>>();

/** Decoded raw RGBA tile buffers keyed by `z/x/y` for the current process. */
const tilePixels = new Map<string, Buffer>();

/** Confirmed 404 tiles for this process (`z/x/y`). */
const tileMissing = new Set<string>();

let lastFetchAt = 0;
let fetchChain: Promise<void> = Promise.resolve();

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}

function scheduleTileFetch<T>(fn: () => Promise<T>): Promise<T> {
  const run = fetchChain.then(async () => {
    const wait = lastFetchAt + FETCH_GAP_MS - Date.now();
    if (wait > 0) await sleep(wait);
    lastFetchAt = Date.now();
    return fn();
  });
  fetchChain = run.then(
    () => undefined,
    () => undefined,
  );
  return run;
}

function writeFileAtomic(destPath: string, data: Buffer): void {
  fs.mkdirSync(path.dirname(destPath), { recursive: true });
  const tmp = path.join(
    path.dirname(destPath),
    `.${path.basename(destPath)}.${process.pid}.${Date.now()}.${Math.random().toString(36).slice(2)}.tmp`,
  );
  try {
    fs.writeFileSync(tmp, data);
    fs.renameSync(tmp, destPath);
  } catch (err) {
    try {
      fs.unlinkSync(tmp);
    } catch {
      // ignore
    }
    throw err;
  }
}

function cachePath(cacheDir: string, z: number, x: number, y: number): string {
  return path.join(cacheDir, String(z), String(x), `${y}.webp`);
}

function missingMarkerPath(
  cacheDir: string,
  z: number,
  x: number,
  y: number,
): string {
  return path.join(cacheDir, String(z), String(x), `${y}.missing`);
}

function decodeTerrarium(r: number, g: number, b: number): number {
  return r * 256 + g + b / 256 - 32768;
}

function markTileMissing(cacheDir: string, z: number, x: number, y: number): void {
  const key = `${z}/${x}/${y}`;
  tileMissing.add(key);
  writeFileAtomic(missingMarkerPath(cacheDir, z, x, y), Buffer.alloc(0));
}

async function fetchTileWebp(
  cacheDir: string,
  z: number,
  x: number,
  y: number,
): Promise<Buffer | null> {
  const dest = cachePath(cacheDir, z, x, y);
  const miss = missingMarkerPath(cacheDir, z, x, y);
  const key = `${z}/${x}/${y}`;
  if (tileMissing.has(key) || fs.existsSync(miss)) {
    tileMissing.add(key);
    return null;
  }
  if (fs.existsSync(dest)) {
    return fs.readFileSync(dest);
  }

  const existing = tileInflight.get(key);
  if (existing) return existing;

  const promise = scheduleTileFetch(async () => {
    if (tileMissing.has(key) || fs.existsSync(miss)) {
      tileMissing.add(key);
      return null;
    }
    if (fs.existsSync(dest)) {
      return fs.readFileSync(dest);
    }
    const url = TILE_URL(z, x, y);
    const res = await fetch(url, {
      headers: { "User-Agent": USER_AGENT },
    });
    if (res.status === 404) {
      markTileMissing(cacheDir, z, x, y);
      return null;
    }
    if (!res.ok) {
      throw new Error(
        `Mapterhorn tile fetch failed ${z}/${x}/${y}: HTTP ${res.status}`,
      );
    }
    const data = Buffer.from(await res.arrayBuffer());
    writeFileAtomic(dest, data);
    return data;
  }).finally(() => {
    tileInflight.delete(key);
  });

  tileInflight.set(key, promise);
  return promise;
}

async function loadTilePixels(
  cacheDir: string,
  z: number,
  x: number,
  y: number,
): Promise<Buffer | null> {
  const key = `${z}/${x}/${y}`;
  if (tileMissing.has(key)) return null;
  const cached = tilePixels.get(key);
  if (cached) return cached;

  const webp = await fetchTileWebp(cacheDir, z, x, y);
  if (webp == null) return null;
  const { data, info } = await sharp(webp)
    .ensureAlpha()
    .raw()
    .toBuffer({ resolveWithObject: true });
  if (info.width !== ELEVATION_TILE_SIZE || info.height !== ELEVATION_TILE_SIZE) {
    throw new Error(
      `Unexpected Mapterhorn tile size ${info.width}x${info.height} for ${key}`,
    );
  }
  tilePixels.set(key, data);
  return data;
}

function pixelElevation(pixels: Buffer, px: number, py: number): number {
  const i = (py * ELEVATION_TILE_SIZE + px) * 4;
  return decodeTerrarium(pixels[i]!, pixels[i + 1]!, pixels[i + 2]!);
}

/**
 * Bilinear-sample at an exact zoom. Returns null if any required tile is
 * missing (Mapterhorn 404 outside regional coverage).
 */
async function elevationAtExactZoom(
  lat: number,
  lon: number,
  cacheDir: string,
  zoom: number,
): Promise<number | null> {
  const max = Math.pow(2, zoom);
  const fx = lonToTileX(lon, zoom);
  const fy = latToTileY(lat, zoom);
  const px = fx * ELEVATION_TILE_SIZE;
  const py = fy * ELEVATION_TILE_SIZE;

  const x0 = Math.floor(px);
  const y0 = Math.floor(py);
  const x1 = x0 + 1;
  const y1 = y0 + 1;
  const tx = px - x0;
  const ty = py - y0;

  async function sampleWorldPixel(
    wx: number,
    wy: number,
  ): Promise<number | null> {
    let tileX = Math.floor(wx / ELEVATION_TILE_SIZE);
    let tileY = Math.floor(wy / ELEVATION_TILE_SIZE);
    const localX =
      ((wx - tileX * ELEVATION_TILE_SIZE) % ELEVATION_TILE_SIZE +
        ELEVATION_TILE_SIZE) %
      ELEVATION_TILE_SIZE;
    const localY = Math.max(
      0,
      Math.min(ELEVATION_TILE_SIZE - 1, wy - tileY * ELEVATION_TILE_SIZE),
    );
    tileX = ((tileX % max) + max) % max;
    tileY = Math.max(0, Math.min(max - 1, tileY));
    const pixels = await loadTilePixels(cacheDir, zoom, tileX, tileY);
    if (pixels == null) return null;
    return pixelElevation(pixels, localX, localY);
  }

  const e00 = await sampleWorldPixel(x0, y0);
  if (e00 == null) return null;
  const e10 = await sampleWorldPixel(x1, y0);
  if (e10 == null) return null;
  const e01 = await sampleWorldPixel(x0, y1);
  if (e01 == null) return null;
  const e11 = await sampleWorldPixel(x1, y1);
  if (e11 == null) return null;

  const e0 = e00 * (1 - tx) + e10 * tx;
  const e1 = e01 * (1 - tx) + e11 * tx;
  return e0 * (1 - ty) + e1 * ty;
}

/**
 * Bilinear-sample elevation (metres) from Mapterhorn tiles, trying preferred
 * zoom then stepping down to global coverage.
 */
export async function elevationAt(
  lat: number,
  lon: number,
  cacheDir: string,
  zoom = ELEVATION_ZOOM,
): Promise<number> {
  const maxZoom = Math.max(zoom, ELEVATION_ZOOM_MIN);
  for (let z = maxZoom; z >= ELEVATION_ZOOM_MIN; z--) {
    const elev = await elevationAtExactZoom(lat, lon, cacheDir, z);
    if (elev != null) return elev;
  }
  throw new Error(
    `Mapterhorn has no elevation tiles for ${lat.toFixed(5)},${lon.toFixed(5)} (tried z${ELEVATION_ZOOM_MIN}–${maxZoom})`,
  );
}

/**
 * Prefetch tiles for `points`, trying preferred zoom first. Points whose
 * bilinear neighbourhood is incomplete fall through to coarser zooms.
 */
async function ensureTilesForPoints(
  points: LatLon[],
  cacheDir: string,
): Promise<void> {
  let remaining: LatLon[] = points;
  for (let zoom = ELEVATION_ZOOM; zoom >= ELEVATION_ZOOM_MIN; zoom--) {
    if (remaining.length === 0) break;
    const max = Math.pow(2, zoom);
    const needed = new Set<string>();
    for (const p of remaining) {
      const tx = Math.floor(lonToTileX(p.lon, zoom));
      const ty = Math.floor(latToTileY(p.lat, zoom));
      for (let dx = -1; dx <= 1; dx++) {
        for (let dy = -1; dy <= 1; dy++) {
          let x = tx + dx;
          let y = ty + dy;
          x = ((x % max) + max) % max;
          if (y < 0 || y >= max) continue;
          needed.add(`${x}/${y}`);
        }
      }
    }
    for (const key of needed) {
      const [xs, ys] = key.split("/");
      await loadTilePixels(cacheDir, zoom, Number(xs), Number(ys));
    }

    const stillNeed: LatLon[] = [];
    for (const p of remaining) {
      const tx = Math.floor(lonToTileX(p.lon, zoom));
      const ty = Math.floor(latToTileY(p.lat, zoom));
      let covered = true;
      for (let dx = -1; dx <= 1 && covered; dx++) {
        for (let dy = -1; dy <= 1; dy++) {
          let x = tx + dx;
          let y = ty + dy;
          x = ((x % max) + max) % max;
          if (y < 0 || y >= max) continue;
          if (tileMissing.has(`${zoom}/${x}/${y}`)) {
            covered = false;
            break;
          }
        }
      }
      if (!covered) stillNeed.push(p);
    }
    remaining = stillNeed;
  }
}


/**
 * Centered moving-average smooth of an elevation series (reduces DEM noise).
 * Window is clamped at track ends.
 */
export function smoothElevations(
  elevations: number[],
  windowSize: number,
): number[] {
  if (elevations.length === 0) return [];
  const half = Math.floor(windowSize / 2);
  const out = new Array<number>(elevations.length);
  for (let i = 0; i < elevations.length; i++) {
    let sum = 0;
    let n = 0;
    for (
      let j = Math.max(0, i - half);
      j <= Math.min(elevations.length - 1, i + half);
      j++
    ) {
      sum += elevations[j]!;
      n++;
    }
    out[i] = sum / n;
  }
  return out;
}

/**
 * Cumulative elevation gain along a track on a (typically smoothed) elevation
 * series, ignoring deltas smaller than `thresholdM` relative to the last
 * confirmed extreme (hysteresis).
 */
export function ascentFromElevations(
  elevations: number[],
  thresholdM = ASCENT_THRESHOLD_M,
): number {
  if (elevations.length < 2) return 0;
  let ascent = 0;
  let last = elevations[0]!;
  for (let i = 1; i < elevations.length; i++) {
    const elev = elevations[i]!;
    const d = elev - last;
    if (d >= thresholdM) {
      ascent += d;
      last = elev;
    } else if (d <= -thresholdM) {
      last = elev;
    }
  }
  return ascent;
}

export type TrackElevationStats = {
  ascentMeters: number;
  /** Highest DEM sample along the track (metres), or null if empty. */
  maxElevationMeters: number | null;
};

/** Ascent and max elevation for one track using Mapterhorn DEM samples. */
export async function elevationStatsForTrack(
  points: LatLon[],
  cacheDir: string,
): Promise<TrackElevationStats> {
  if (points.length === 0) {
    return { ascentMeters: 0, maxElevationMeters: null };
  }
  await ensureTilesForPoints(points, cacheDir);
  const elevations: number[] = [];
  for (const p of points) {
    elevations.push(await elevationAt(p.lat, p.lon, cacheDir));
  }
  let maxElevationMeters = elevations[0]!;
  for (let i = 1; i < elevations.length; i++) {
    const elev = elevations[i]!;
    if (elev > maxElevationMeters) maxElevationMeters = elev;
  }
  if (elevations.length < 2) {
    return { ascentMeters: 0, maxElevationMeters };
  }
  const smoothed = smoothElevations(elevations, ASCENT_SMOOTH_WINDOW);
  return {
    ascentMeters: ascentFromElevations(smoothed),
    maxElevationMeters,
  };
}

/**
 * Sum ascent across separate tracks (do not join end-to-start); take the
 * highest max elevation among them.
 */
export async function elevationStatsForTracks(
  trackPointLists: LatLon[][],
  cacheDir: string,
): Promise<TrackElevationStats> {
  let ascentMeters = 0;
  let maxElevationMeters: number | null = null;
  for (const points of trackPointLists) {
    const stats = await elevationStatsForTrack(points, cacheDir);
    ascentMeters += stats.ascentMeters;
    if (
      typeof stats.maxElevationMeters === "number" &&
      (maxElevationMeters === null ||
        stats.maxElevationMeters > maxElevationMeters)
    ) {
      maxElevationMeters = stats.maxElevationMeters;
    }
  }
  return { ascentMeters, maxElevationMeters };
}

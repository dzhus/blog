import fs from "node:fs";
import path from "node:path";
import sharp from "sharp";
import {
  TILE_SIZE,
  latToTileY,
  lonToTileX,
} from "./geo.ts";
import type { LatLon } from "./types.ts";

/** Bump when changing DEM source/zoom so caches invalidate. */
export const ELEVATION_SOURCE_ID = "terrarium";
/** Terrarium sample zoom (~30 m-class DEM). */
export const ELEVATION_ZOOM = 12;
/** Ignore elevation deltas smaller than this when summing ascent (metres). */
export const ASCENT_THRESHOLD_M = 5;

const USER_AGENT =
  "dzhus.org-blog-static-map/1.0 (https://dzhus.org; personal static site build)";

const TILE_URL = (z: number, x: number, y: number) =>
  `https://s3.amazonaws.com/elevation-tiles-prod/terrarium/${z}/${x}/${y}.png`;

/** Min gap between outbound Terrarium HTTP requests. */
const FETCH_GAP_MS = 80;

/** In-flight tile fetches keyed by `z/x/y`. */
const tileInflight = new Map<string, Promise<Buffer>>();

/** Decoded raw RGBA tile buffers keyed by `z/x/y` for the current process. */
const tilePixels = new Map<string, Buffer>();

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
  return path.join(cacheDir, String(z), String(x), `${y}.png`);
}

function decodeTerrarium(r: number, g: number, b: number): number {
  return r * 256 + g + b / 256 - 32768;
}

async function fetchTilePng(
  cacheDir: string,
  z: number,
  x: number,
  y: number,
): Promise<Buffer> {
  const dest = cachePath(cacheDir, z, x, y);
  if (fs.existsSync(dest)) {
    return fs.readFileSync(dest);
  }

  const key = `${z}/${x}/${y}`;
  const existing = tileInflight.get(key);
  if (existing) return existing;

  const promise = scheduleTileFetch(async () => {
    if (fs.existsSync(dest)) {
      return fs.readFileSync(dest);
    }
    const url = TILE_URL(z, x, y);
    const res = await fetch(url, {
      headers: { "User-Agent": USER_AGENT },
    });
    if (!res.ok) {
      throw new Error(
        `Terrarium tile fetch failed ${z}/${x}/${y}: HTTP ${res.status}`,
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
): Promise<Buffer> {
  const key = `${z}/${x}/${y}`;
  const cached = tilePixels.get(key);
  if (cached) return cached;

  const png = await fetchTilePng(cacheDir, z, x, y);
  const { data, info } = await sharp(png)
    .ensureAlpha()
    .raw()
    .toBuffer({ resolveWithObject: true });
  if (info.width !== TILE_SIZE || info.height !== TILE_SIZE) {
    throw new Error(
      `Unexpected Terrarium tile size ${info.width}x${info.height} for ${key}`,
    );
  }
  tilePixels.set(key, data);
  return data;
}

function pixelElevation(pixels: Buffer, px: number, py: number): number {
  const i = (py * TILE_SIZE + px) * 4;
  return decodeTerrarium(pixels[i]!, pixels[i + 1]!, pixels[i + 2]!);
}

/** Bilinear-sample elevation (metres) at a WGS84 point from cached Terrarium tiles. */
export async function elevationAt(
  lat: number,
  lon: number,
  cacheDir: string,
  zoom = ELEVATION_ZOOM,
): Promise<number> {
  const max = Math.pow(2, zoom);
  const fx = lonToTileX(lon, zoom);
  const fy = latToTileY(lat, zoom);
  const px = fx * TILE_SIZE;
  const py = fy * TILE_SIZE;

  const x0 = Math.floor(px);
  const y0 = Math.floor(py);
  const x1 = x0 + 1;
  const y1 = y0 + 1;
  const tx = px - x0;
  const ty = py - y0;

  async function sampleWorldPixel(wx: number, wy: number): Promise<number> {
    let tileX = Math.floor(wx / TILE_SIZE);
    let tileY = Math.floor(wy / TILE_SIZE);
    const localX = ((wx - tileX * TILE_SIZE) % TILE_SIZE + TILE_SIZE) % TILE_SIZE;
    const localY = Math.max(0, Math.min(TILE_SIZE - 1, wy - tileY * TILE_SIZE));
    tileX = ((tileX % max) + max) % max;
    tileY = Math.max(0, Math.min(max - 1, tileY));
    const pixels = await loadTilePixels(cacheDir, zoom, tileX, tileY);
    return pixelElevation(pixels, localX, localY);
  }

  const e00 = await sampleWorldPixel(x0, y0);
  const e10 = await sampleWorldPixel(x1, y0);
  const e01 = await sampleWorldPixel(x0, y1);
  const e11 = await sampleWorldPixel(x1, y1);

  const e0 = e00 * (1 - tx) + e10 * tx;
  const e1 = e01 * (1 - tx) + e11 * tx;
  return e0 * (1 - ty) + e1 * ty;
}

/** Prefetch every Terrarium tile covering the given points. */
async function ensureTilesForPoints(
  points: LatLon[],
  cacheDir: string,
  zoom = ELEVATION_ZOOM,
): Promise<void> {
  const max = Math.pow(2, zoom);
  const needed = new Set<string>();
  for (const p of points) {
    const tx = Math.floor(lonToTileX(p.lon, zoom));
    const ty = Math.floor(latToTileY(p.lat, zoom));
    // Also neighbouring tiles for bilinear edge samples.
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
}

/**
 * Cumulative elevation gain along a track, ignoring deltas smaller than
 * ASCENT_THRESHOLD_M relative to the last confirmed extreme.
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

/** Total ascent (metres) for one track using Terrarium DEM elevations. */
export async function totalAscentMeters(
  points: LatLon[],
  cacheDir: string,
): Promise<number> {
  if (points.length < 2) return 0;
  await ensureTilesForPoints(points, cacheDir);
  const elevations: number[] = [];
  for (const p of points) {
    elevations.push(await elevationAt(p.lat, p.lon, cacheDir));
  }
  return ascentFromElevations(elevations);
}

/** Sum total ascent across separate tracks (do not join end-to-start). */
export async function totalAscentMetersForTracks(
  trackPointLists: LatLon[][],
  cacheDir: string,
): Promise<number> {
  let total = 0;
  for (const points of trackPointLists) {
    total += await totalAscentMeters(points, cacheDir);
  }
  return total;
}

import fs from "node:fs";
import path from "node:path";
import sharp, { type OverlayOptions } from "sharp";
import type { BBox } from "./types.ts";

const TILE_SIZE = 256;
/** Bump when changing tile provider so basemap caches invalidate. */
export const TILE_SOURCE_ID = "opentopomap";
const USER_AGENT =
  "dzhus.org-blog-static-map/1.0 (https://dzhus.org; personal static site build)";
/** OpenTopoMap includes contour lines (CC-BY-SA). */
const TILE_URL = (z: number, x: number, y: number) =>
  `https://tile.opentopomap.org/${z}/${x}/${y}.png`;

const MAX_EDGE = 2048;
const MIN_EDGE = 640;

function lonToTileX(lon: number, z: number): number {
  return ((lon + 180) / 360) * Math.pow(2, z);
}

function latToTileY(lat: number, z: number): number {
  const rad = (lat * Math.PI) / 180;
  return (
    ((1 -
      Math.log(Math.tan(rad) + 1 / Math.cos(rad)) / Math.PI) /
      2) *
    Math.pow(2, z)
  );
}

function chooseZoom(bounds: BBox): number {
  for (let z = 16; z >= 6; z--) {
    const x0 = lonToTileX(bounds.west, z);
    const x1 = lonToTileX(bounds.east, z);
    const y0 = latToTileY(bounds.north, z);
    const y1 = latToTileY(bounds.south, z);
    const width = Math.abs(x1 - x0) * TILE_SIZE;
    const height = Math.abs(y1 - y0) * TILE_SIZE;
    if (Math.max(width, height) <= MAX_EDGE) {
      return z;
    }
  }
  return 6;
}

async function fetchTile(
  z: number,
  x: number,
  y: number,
  cacheDir: string,
): Promise<Buffer> {
  const max = Math.pow(2, z);
  const tx = ((x % max) + max) % max;
  const ty = y;
  if (ty < 0 || ty >= max) {
    // empty tile
    return sharp({
      create: {
        width: TILE_SIZE,
        height: TILE_SIZE,
        channels: 3,
        background: { r: 238, g: 238, b: 236 },
      },
    })
      .png()
      .toBuffer();
  }

  const cachePath = path.join(cacheDir, String(z), String(tx), `${ty}.png`);
  if (fs.existsSync(cachePath)) {
    return fs.readFileSync(cachePath);
  }

  fs.mkdirSync(path.dirname(cachePath), { recursive: true });
  const url = TILE_URL(z, tx, ty);
  const res = await fetch(url, {
    headers: { "User-Agent": USER_AGENT, Accept: "image/png" },
  });
  if (!res.ok) {
    throw new Error(`Map tile fetch failed ${res.status} ${url}`);
  }
  const buf = Buffer.from(await res.arrayBuffer());
  fs.writeFileSync(cachePath, buf);
  // Be polite between requests
  await new Promise((r) => setTimeout(r, 100));
  return buf;
}

export type BasemapResult = {
  bounds: BBox;
  width: number;
  height: number;
  zoom: number;
};

export async function renderBasemap(
  bounds: BBox,
  outPath: string,
  tileCacheDir: string,
): Promise<BasemapResult> {
  const z = chooseZoom(bounds);
  const x0f = lonToTileX(bounds.west, z);
  const x1f = lonToTileX(bounds.east, z);
  const y0f = latToTileY(bounds.north, z);
  const y1f = latToTileY(bounds.south, z);

  const xStart = Math.floor(x0f);
  const xEnd = Math.ceil(x1f);
  const yStart = Math.floor(y0f);
  const yEnd = Math.ceil(y1f);

  const tilesX = xEnd - xStart;
  const tilesY = yEnd - yStart;

  if (tilesX <= 0 || tilesY <= 0) {
    throw new Error("Invalid basemap tile range");
  }

  const composites: OverlayOptions[] = [];
  for (let ty = yStart; ty < yEnd; ty++) {
    for (let tx = xStart; tx < xEnd; tx++) {
      const buf = await fetchTile(z, tx, ty, tileCacheDir);
      composites.push({
        input: buf,
        left: (tx - xStart) * TILE_SIZE,
        top: (ty - yStart) * TILE_SIZE,
      });
    }
  }

  const fullW = tilesX * TILE_SIZE;
  const fullH = tilesY * TILE_SIZE;

  // Crop to exact geographic bounds within the tile mosaic
  const cropLeft = Math.floor((x0f - xStart) * TILE_SIZE);
  const cropTop = Math.floor((y0f - yStart) * TILE_SIZE);
  const cropWidth = Math.max(
    MIN_EDGE,
    Math.ceil((x1f - x0f) * TILE_SIZE),
  );
  const cropHeight = Math.max(
    MIN_EDGE,
    Math.ceil((y1f - y0f) * TILE_SIZE),
  );

  const safeWidth = Math.min(cropWidth, fullW - cropLeft);
  const safeHeight = Math.min(cropHeight, fullH - cropTop);

  fs.mkdirSync(path.dirname(outPath), { recursive: true });

  const mosaic = await sharp({
    create: {
      width: fullW,
      height: fullH,
      channels: 3,
      background: { r: 238, g: 238, b: 236 },
    },
  })
    .composite(composites)
    .extract({
      left: Math.max(0, cropLeft),
      top: Math.max(0, cropTop),
      width: Math.max(1, safeWidth),
      height: Math.max(1, safeHeight),
    })
    .png()
    .toBuffer();

  const meta = await sharp(mosaic)
    .greyscale()
    .jpeg({ quality: 85, mozjpeg: true })
    .toFile(outPath);

  return {
    bounds,
    width: meta.width,
    height: meta.height,
    zoom: z,
  };
}

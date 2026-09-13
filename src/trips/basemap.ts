import fs from "node:fs";
import path from "node:path";
import sharp from "sharp";
import {
  TILE_SIZE,
  latToTileY,
  lonToTileX,
  tileXToLon,
  tileYToLat,
} from "./geo.ts";
import type { BBox } from "./types.ts";

/** Bump when changing tile provider so caches invalidate. */
export const TILE_SOURCE_ID = "opentopomap";
const USER_AGENT =
  "dzhus.org-blog-static-map/1.0 (https://dzhus.org; personal static site build)";
/** OpenTopoMap includes contour lines (CC-BY-SA). */
const TILE_URL = (z: number, x: number, y: number) =>
  `https://tile.opentopomap.org/${z}/${x}/${y}.png`;

/** Soft cap on mosaic pixel span used only to pick zoom. */
const MAX_EDGE = 2048;
/**
 * Assumed map viewport width/height. fitBounds shows more than the content
 * bbox along the unconstrained axis; tiles must cover that overscan.
 */
const MAP_ASPECT = 1.5;
/** Extra tile ring beyond the aspect-expanded mosaic (maxBounds pad, etc.). */
const TILE_EDGE_MARGIN = 1;

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

/** Grow tile index range so fitBounds into MAP_ASPECT leaves no empty sides,
 * then top up the shorter axis so the fetched mosaic is square. */
function expandTileRange(
  z: number,
  xMin: number,
  xMaxExcl: number,
  yMin: number,
  yMaxExcl: number,
): { xMin: number; xMaxExcl: number; yMin: number; yMaxExcl: number } {
  const max = Math.pow(2, z);
  const w = Math.max(1, xMaxExcl - xMin);
  const h = Math.max(1, yMaxExcl - yMin);
  const minW = Math.max(w, Math.ceil(h * MAP_ASPECT));
  const minH = Math.max(h, Math.ceil(w / MAP_ASPECT));

  let x0 = xMin;
  let x1 = xMaxExcl;
  let y0 = yMin;
  let y1 = yMaxExcl;

  if (minW > w) {
    const add = minW - w;
    const left = Math.floor(add / 2);
    x0 -= left;
    x1 += add - left;
  }
  if (minH > h) {
    const add = minH - h;
    const top = Math.floor(add / 2);
    y0 -= top;
    y1 += add - top;
  }

  x0 -= TILE_EDGE_MARGIN;
  x1 += TILE_EDGE_MARGIN;
  y0 -= TILE_EDGE_MARGIN;
  y1 += TILE_EDGE_MARGIN;

  x0 = Math.max(0, x0);
  y0 = Math.max(0, y0);
  x1 = Math.min(max, x1);
  y1 = Math.min(max, y1);

  if (x1 <= x0) {
    x0 = Math.max(0, Math.min(xMin, max - 1));
    x1 = Math.min(max, x0 + 1);
  }
  if (y1 <= y0) {
    y0 = Math.max(0, Math.min(yMin, max - 1));
    y1 = Math.min(max, y0 + 1);
  }

  // Top up the shorter axis so the mosaic is square (equal tile counts).
  const side = Math.max(x1 - x0, y1 - y0);
  ({ lo: x0, hi: x1 } = expandAxisToSize(x0, x1, side, max));
  ({ lo: y0, hi: y1 } = expandAxisToSize(y0, y1, side, max));

  return { xMin: x0, xMaxExcl: x1, yMin: y0, yMaxExcl: y1 };
}

/** Grow [lo, hi) to `target` length, centered; spill leftover onto the free side after clamp. */
function expandAxisToSize(
  lo: number,
  hi: number,
  target: number,
  max: number,
): { lo: number; hi: number } {
  const cur = hi - lo;
  if (cur >= target) return { lo, hi };

  const add = target - cur;
  const before = Math.floor(add / 2);
  let nextLo = lo - before;
  let nextHi = hi + (add - before);

  if (nextLo < 0) {
    nextHi = Math.min(max, nextHi - nextLo);
    nextLo = 0;
  }
  if (nextHi > max) {
    nextLo = Math.max(0, nextLo - (nextHi - max));
    nextHi = max;
  }

  return { lo: nextLo, hi: nextHi };
}

async function fetchColorTile(
  z: number,
  x: number,
  y: number,
  colorCacheDir: string,
): Promise<Buffer> {
  const max = Math.pow(2, z);
  const tx = ((x % max) + max) % max;
  const ty = y;
  if (ty < 0 || ty >= max) {
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

  const cachePath = path.join(colorCacheDir, String(z), String(tx), `${ty}.png`);
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
  await new Promise((r) => setTimeout(r, 100));
  return buf;
}

async function greyscaleTile(
  colorPng: Buffer,
  greyCachePath: string,
): Promise<Buffer> {
  if (fs.existsSync(greyCachePath)) {
    return fs.readFileSync(greyCachePath);
  }
  fs.mkdirSync(path.dirname(greyCachePath), { recursive: true });
  const out = await sharp(colorPng)
    .greyscale()
    .png({ compressionLevel: 9 })
    .toBuffer();
  fs.writeFileSync(greyCachePath, out);
  return out;
}

export type TripTileSet = {
  zoom: number;
  /** Inclusive tile index range. */
  xMin: number;
  xMax: number;
  yMin: number;
  yMax: number;
  /** Exact WGS84 coverage of the exported tile set. */
  bounds: BBox;
  tileUrlTemplate: string;
};

export async function renderTripTiles(
  bounds: BBox,
  slug: string,
  siteMapDir: string,
  colorCacheDir: string,
  greyCacheDir: string,
): Promise<TripTileSet> {
  const z = chooseZoom(bounds);
  let xMin = Math.floor(lonToTileX(bounds.west, z));
  let xMaxExcl = Math.ceil(lonToTileX(bounds.east, z));
  let yMin = Math.floor(latToTileY(bounds.north, z));
  let yMaxExcl = Math.ceil(latToTileY(bounds.south, z));

  if (xMaxExcl <= xMin || yMaxExcl <= yMin) {
    throw new Error("Invalid tile range for trip bounds");
  }

  ({ xMin, xMaxExcl, yMin, yMaxExcl } = expandTileRange(
    z,
    xMin,
    xMaxExcl,
    yMin,
    yMaxExcl,
  ));

  const siteTilesDir = path.join(siteMapDir, "tiles", String(z));
  fs.mkdirSync(siteTilesDir, { recursive: true });

  for (let ty = yMin; ty < yMaxExcl; ty++) {
    for (let tx = xMin; tx < xMaxExcl; tx++) {
      const color = await fetchColorTile(z, tx, ty, colorCacheDir);
      const greyCachePath = path.join(
        greyCacheDir,
        String(z),
        String(tx),
        `${ty}.png`,
      );
      const grey = await greyscaleTile(color, greyCachePath);
      const destDir = path.join(siteTilesDir, String(tx));
      fs.mkdirSync(destDir, { recursive: true });
      fs.writeFileSync(path.join(destDir, `${ty}.png`), grey);
    }
  }

  const tileBounds: BBox = {
    west: tileXToLon(xMin, z),
    east: tileXToLon(xMaxExcl, z),
    north: tileYToLat(yMin, z),
    south: tileYToLat(yMaxExcl, z),
  };

  return {
    zoom: z,
    xMin,
    xMax: xMaxExcl - 1,
    yMin,
    yMax: yMaxExcl - 1,
    bounds: tileBounds,
    tileUrlTemplate: `/trips/${slug}/map/tiles/{z}/{x}/{y}.png`,
  };
}

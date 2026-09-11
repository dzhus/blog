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
 * Map pane aspect ratios (width/height) covered at build time.
 * Keep in sync with `.trip-map` heights in `css/trips.css`
 * (`min(70vh, 32rem)` / `min(80vh, 40rem)` when photos hidden) and fluid column width.
 */
const ASPECT_MIN = 0.9;
const ASPECT_MAX = 2.2;

/**
 * Grow the data-bbox tile range so fitBounds viewports from ASPECT_MIN..ASPECT_MAX
 * stay inside fetched tiles (no grey edges).
 */
export function expandTileRangeForViewport(
  xMin: number,
  xMaxExcl: number,
  yMin: number,
  yMaxExcl: number,
  z: number,
): { xMin: number; xMaxExcl: number; yMin: number; yMaxExcl: number } {
  const max = Math.pow(2, z);
  const w = xMaxExcl - xMin;
  const h = yMaxExcl - yMin;
  const needW = Math.max(w, Math.ceil(h * ASPECT_MAX));
  const needH = Math.max(h, Math.ceil(w / ASPECT_MIN));

  const extraW = needW - w;
  const extraH = needH - h;
  let nx0 = xMin - Math.floor(extraW / 2);
  let nx1 = xMaxExcl + (extraW - Math.floor(extraW / 2));
  let ny0 = yMin - Math.floor(extraH / 2);
  let ny1 = yMaxExcl + (extraH - Math.floor(extraH / 2));

  if (nx0 < 0) {
    nx1 = Math.min(max, nx1 - nx0);
    nx0 = 0;
  }
  if (nx1 > max) {
    nx0 = Math.max(0, nx0 - (nx1 - max));
    nx1 = max;
  }
  if (ny0 < 0) {
    ny1 = Math.min(max, ny1 - ny0);
    ny0 = 0;
  }
  if (ny1 > max) {
    ny0 = Math.max(0, ny0 - (ny1 - max));
    ny1 = max;
  }

  return { xMin: nx0, xMaxExcl: nx1, yMin: ny0, yMaxExcl: ny1 };
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

  ({ xMin, xMaxExcl, yMin, yMaxExcl } = expandTileRangeForViewport(
    xMin,
    xMaxExcl,
    yMin,
    yMaxExcl,
    z,
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

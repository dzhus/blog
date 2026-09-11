/** Web Mercator helpers matching OSM/OpenTopoMap XYZ tiles. */

export const TILE_SIZE = 256;

export function lonToTileX(lon: number, z: number): number {
  return ((lon + 180) / 360) * Math.pow(2, z);
}

export function latToTileY(lat: number, z: number): number {
  const rad = (lat * Math.PI) / 180;
  return (
    ((1 -
      Math.log(Math.tan(rad) + 1 / Math.cos(rad)) / Math.PI) /
      2) *
    Math.pow(2, z)
  );
}

export function tileXToLon(x: number, z: number): number {
  return (x / Math.pow(2, z)) * 360 - 180;
}

export function tileYToLat(y: number, z: number): number {
  const n = Math.PI - (2 * Math.PI * y) / Math.pow(2, z);
  return (180 / Math.PI) * Math.atan(0.5 * (Math.exp(n) - Math.exp(-n)));
}

/** Georeference of a stitched basemap image in tile/pixel space. */
export type BasemapGeoref = {
  z: number;
  /** Fractional tile X of the left edge of pixel (0,0). */
  x0: number;
  /** Fractional tile Y of the top edge of pixel (0,0). */
  y0: number;
  width: number;
  height: number;
};

export function georefFromCrop(
  z: number,
  mosaicTileX0: number,
  mosaicTileY0: number,
  cropLeft: number,
  cropTop: number,
  width: number,
  height: number,
): BasemapGeoref {
  return {
    z,
    x0: mosaicTileX0 + cropLeft / TILE_SIZE,
    y0: mosaicTileY0 + cropTop / TILE_SIZE,
    width,
    height,
  };
}

export function georefToBBox(g: BasemapGeoref): {
  west: number;
  east: number;
  north: number;
  south: number;
} {
  return {
    west: tileXToLon(g.x0, g.z),
    east: tileXToLon(g.x0 + g.width / TILE_SIZE, g.z),
    north: tileYToLat(g.y0, g.z),
    south: tileYToLat(g.y0 + g.height / TILE_SIZE, g.z),
  };
}

/** Pixel coords on the basemap image (origin top-left, y down). */
export function latLonToImagePixel(
  lat: number,
  lon: number,
  g: BasemapGeoref,
): { x: number; y: number } {
  const tx = lonToTileX(lon, g.z);
  const ty = latToTileY(lat, g.z);
  return {
    x: (tx - g.x0) * TILE_SIZE,
    y: (ty - g.y0) * TILE_SIZE,
  };
}

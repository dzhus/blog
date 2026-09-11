import type { BBox, LatLon } from "./types.ts";

export function emptyBBox(): BBox {
  return { south: 90, west: 180, north: -90, east: -180 };
}

export function expandBBox(box: BBox, lat: number, lon: number): void {
  if (lat < box.south) box.south = lat;
  if (lat > box.north) box.north = lat;
  if (lon < box.west) box.west = lon;
  if (lon > box.east) box.east = lon;
}

export function mergeBBox(a: BBox, b: BBox): BBox {
  return {
    south: Math.min(a.south, b.south),
    west: Math.min(a.west, b.west),
    north: Math.max(a.north, b.north),
    east: Math.max(a.east, b.east),
  };
}

export function isValidBBox(box: BBox): boolean {
  return box.south <= box.north && box.west <= box.east;
}

export function padBBox(box: BBox, fraction = 0.08): BBox {
  const latPad = Math.max((box.north - box.south) * fraction, 0.002);
  const lonPad = Math.max((box.east - box.west) * fraction, 0.002);
  return {
    south: box.south - latPad,
    west: box.west - lonPad,
    north: box.north + latPad,
    east: box.east + lonPad,
  };
}

/** Squared distance from point to segment AB (lon/lat as x/y). */
function distToSegmentSq(
  p: LatLon,
  a: LatLon,
  b: LatLon,
): number {
  const x = p.lon;
  const y = p.lat;
  const x1 = a.lon;
  const y1 = a.lat;
  const x2 = b.lon;
  const y2 = b.lat;
  const dx = x2 - x1;
  const dy = y2 - y1;
  if (dx === 0 && dy === 0) {
    const ex = x - x1;
    const ey = y - y1;
    return ex * ex + ey * ey;
  }
  let t = ((x - x1) * dx + (y - y1) * dy) / (dx * dx + dy * dy);
  t = Math.max(0, Math.min(1, t));
  const px = x1 + t * dx;
  const py = y1 + t * dy;
  const ex = x - px;
  const ey = y - py;
  return ex * ex + ey * ey;
}

/**
 * Douglas–Peucker simplification. `tolerance` is in degrees.
 * Returns [lat, lon] pairs for Leaflet.
 */
export function simplifyTrack(
  points: LatLon[],
  tolerance = 0.00008,
  maxPoints = 800,
): [number, number][] {
  if (points.length <= 2) {
    return points.map((p) => [p.lat, p.lon] as [number, number]);
  }

  const sqTol = tolerance * tolerance;
  const keep = new Uint8Array(points.length);
  keep[0] = 1;
  keep[points.length - 1] = 1;

  const stack: Array<[number, number]> = [[0, points.length - 1]];
  while (stack.length) {
    const [start, end] = stack.pop()!;
    let maxSq = 0;
    let index = -1;
    const a = points[start]!;
    const b = points[end]!;
    for (let i = start + 1; i < end; i++) {
      const d = distToSegmentSq(points[i]!, a, b);
      if (d > maxSq) {
        maxSq = d;
        index = i;
      }
    }
    if (index >= 0 && maxSq > sqTol) {
      keep[index] = 1;
      stack.push([start, index], [index, end]);
    }
  }

  let result: [number, number][] = [];
  for (let i = 0; i < points.length; i++) {
    if (keep[i]) {
      const p = points[i]!;
      result.push([p.lat, p.lon]);
    }
  }

  if (result.length > maxPoints) {
    const step = result.length / maxPoints;
    const sampled: [number, number][] = [];
    for (let i = 0; i < maxPoints - 1; i++) {
      sampled.push(result[Math.floor(i * step)]!);
    }
    sampled.push(result[result.length - 1]!);
    result = sampled;
  }

  return result;
}

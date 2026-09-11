import fs from "node:fs";
import path from "node:path";
import { XMLParser } from "fast-xml-parser";
import { trackColor } from "./colors.ts";
import {
  emptyBBox,
  expandBBox,
  isValidBBox,
  simplifyTrack,
} from "./geo.ts";
import type { BBox, LatLon, TripTrack } from "./types.ts";

const parser = new XMLParser({
  ignoreAttributes: false,
  attributeNamePrefix: "@_",
  isArray: (name) =>
    ["trk", "trkseg", "trkpt", "rte", "rtept", "wpt"].includes(name),
});

function asArray<T>(v: T | T[] | undefined | null): T[] {
  if (v == null) return [];
  return Array.isArray(v) ? v : [v];
}

type GpxParseResult = {
  points: LatLon[];
  /** Epoch ms of the first track/route point with a usable `<time>`, else null. */
  startedAtMs: number | null;
};

const EARTH_RADIUS_M = 6371000;

function haversineMeters(a: LatLon, b: LatLon): number {
  const toRad = (d: number) => (d * Math.PI) / 180;
  const dLat = toRad(b.lat - a.lat);
  const dLon = toRad(b.lon - a.lon);
  const lat1 = toRad(a.lat);
  const lat2 = toRad(b.lat);
  const h =
    Math.sin(dLat / 2) ** 2 +
    Math.cos(lat1) * Math.cos(lat2) * Math.sin(dLon / 2) ** 2;
  return 2 * EARTH_RADIUS_M * Math.asin(Math.min(1, Math.sqrt(h)));
}

/** Path length along consecutive points (metres). */
export function pathLengthMeters(points: LatLon[]): number {
  let total = 0;
  for (let i = 1; i < points.length; i++) {
    total += haversineMeters(points[i - 1]!, points[i]!);
  }
  return total;
}

/** Format kilometres for trip UI (e.g. "142 км"). */
export function formatDistanceKm(meters: number): string | null {
  if (!(meters > 0)) return null;
  const km = meters / 1000;
  const rounded = km >= 100 ? Math.round(km) : Math.round(km * 10) / 10;
  const text =
    Number.isInteger(rounded) || rounded >= 100
      ? String(Math.round(rounded))
      : rounded.toFixed(1);
  return `${text} км`;
}

function parsePointTime(raw: unknown): number | null {
  if (typeof raw !== "string" || raw.trim() === "") return null;
  const ms = Date.parse(raw);
  return Number.isFinite(ms) ? ms : null;
}

function readGpx(filePath: string): GpxParseResult {
  const xml = fs.readFileSync(filePath, "utf8");
  const doc = parser.parse(xml);
  const gpx = doc.gpx;
  if (!gpx) return { points: [], startedAtMs: null };

  const points: LatLon[] = [];
  let startedAtMs: number | null = null;

  const considerPoint = (pt: Record<string, unknown>) => {
    const lat = Number(pt["@_lat"]);
    const lon = Number(pt["@_lon"]);
    if (!Number.isFinite(lat) || !Number.isFinite(lon)) return;
    points.push({ lat, lon });
    if (startedAtMs == null) {
      startedAtMs = parsePointTime(pt.time);
    }
  };

  for (const trk of asArray(gpx.trk)) {
    for (const seg of asArray(trk.trkseg)) {
      for (const pt of asArray(seg.trkpt)) {
        considerPoint(pt);
      }
    }
  }

  if (points.length === 0) {
    for (const rte of asArray(gpx.rte)) {
      for (const pt of asArray(rte.rtept)) {
        considerPoint(pt);
      }
    }
  }

  return { points, startedAtMs };
}

export function processGpxFiles(
  gpxFiles: string[],
  slug: string,
  siteTripDir: string,
): { tracks: TripTrack[]; bounds: BBox; distanceMeters: number } {
  const gpxOutDir = path.join(siteTripDir, "gpx");
  fs.mkdirSync(gpxOutDir, { recursive: true });

  const bounds = emptyBBox();
  let distanceMeters = 0;

  const parsed = gpxFiles.map((filePath) => {
    const filename = path.basename(filePath);
    const dest = path.join(gpxOutDir, filename);
    fs.copyFileSync(filePath, dest);

    const { points, startedAtMs } = readGpx(filePath);
    for (const p of points) expandBBox(bounds, p.lat, p.lon);
    distanceMeters += pathLengthMeters(points);

    if (startedAtMs == null) {
      const label = path.relative(process.cwd(), filePath) || filePath;
      throw new Error(
        `Missing <time> on the first track point in ${label}. GPX tracks are ordered by that timestamp.`,
      );
    }

    return {
      filename,
      startedAtMs,
      coordinates: simplifyTrack(points),
      id: path.basename(filename, path.extname(filename)),
    };
  });

  parsed.sort((a, b) => {
    if (a.startedAtMs !== b.startedAtMs) return a.startedAtMs - b.startedAtMs;
    return a.filename.localeCompare(b.filename, "en");
  });

  const tracks: TripTrack[] = parsed.map((t, index) => ({
    id: t.id,
    filename: t.filename,
    color: trackColor(index),
    gpxUrl: `/trips/${slug}/gpx/${t.filename}`,
    coordinates: t.coordinates,
  }));

  return {
    tracks,
    bounds: isValidBBox(bounds) ? bounds : emptyBBox(),
    distanceMeters,
  };
}

export function writeTracksJson(
  tracks: TripTrack[],
  outPath: string,
): void {
  const payload = {
    type: "FeatureCollection",
    features: tracks.map((t) => ({
      type: "Feature",
      properties: {
        id: t.id,
        filename: t.filename,
        color: t.color,
        gpxUrl: t.gpxUrl,
      },
      geometry: {
        type: "LineString",
        coordinates: t.coordinates.map(([lat, lon]) => [lon, lat]),
      },
      leafletCoordinates: t.coordinates,
    })),
  };
  fs.mkdirSync(path.dirname(outPath), { recursive: true });
  fs.writeFileSync(outPath, JSON.stringify(payload));
}

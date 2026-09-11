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

function readPointsFromGpx(filePath: string): LatLon[] {
  const xml = fs.readFileSync(filePath, "utf8");
  const doc = parser.parse(xml);
  const gpx = doc.gpx;
  if (!gpx) return [];

  const points: LatLon[] = [];

  for (const trk of asArray(gpx.trk)) {
    for (const seg of asArray(trk.trkseg)) {
      for (const pt of asArray(seg.trkpt)) {
        const lat = Number(pt["@_lat"]);
        const lon = Number(pt["@_lon"]);
        if (Number.isFinite(lat) && Number.isFinite(lon)) {
          points.push({ lat, lon });
        }
      }
    }
  }

  // Fallback: routes
  if (points.length === 0) {
    for (const rte of asArray(gpx.rte)) {
      for (const pt of asArray(rte.rtept)) {
        const lat = Number(pt["@_lat"]);
        const lon = Number(pt["@_lon"]);
        if (Number.isFinite(lat) && Number.isFinite(lon)) {
          points.push({ lat, lon });
        }
      }
    }
  }

  return points;
}

export function processGpxFiles(
  gpxFiles: string[],
  slug: string,
  siteTripDir: string,
): { tracks: TripTrack[]; bounds: BBox } {
  const gpxOutDir = path.join(siteTripDir, "gpx");
  fs.mkdirSync(gpxOutDir, { recursive: true });

  const bounds = emptyBBox();
  const tracks: TripTrack[] = [];

  gpxFiles.forEach((filePath, index) => {
    const filename = path.basename(filePath);
    const dest = path.join(gpxOutDir, filename);
    fs.copyFileSync(filePath, dest);

    const raw = readPointsFromGpx(filePath);
    for (const p of raw) expandBBox(bounds, p.lat, p.lon);

    const coordinates = simplifyTrack(raw);
    const id = path.basename(filename, path.extname(filename));

    tracks.push({
      id,
      filename,
      color: trackColor(index),
      gpxUrl: `/trips/${slug}/gpx/${filename}`,
      coordinates,
    });
  });

  return {
    tracks,
    bounds: isValidBBox(bounds) ? bounds : emptyBBox(),
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
        // GeoJSON is [lon, lat]; we also keep leafletCoords for convenience
        coordinates: t.coordinates.map(([lat, lon]) => [lon, lat]),
      },
      leafletCoordinates: t.coordinates,
    })),
  };
  fs.mkdirSync(path.dirname(outPath), { recursive: true });
  fs.writeFileSync(outPath, JSON.stringify(payload));
}

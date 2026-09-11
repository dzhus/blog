import fs from "node:fs";
import path from "node:path";
import sharp from "sharp";
import { renderBasemap, TILE_SOURCE_ID } from "./basemap.ts";
import { discoverTrips } from "./discover.ts";
import {
  formatIsoCapturedAt,
  readPhotoExif,
} from "./exif.ts";
import {
  emptyBBox,
  expandBBox,
  isValidBBox,
  mergeBBox,
  padBBox,
} from "./geo.ts";
import { processGpxFiles, writeTracksJson } from "./gpx.ts";
import { processPhotoImages } from "./images.ts";
import { writeMapScript } from "./mapScript.ts";
import type { TripManifest, TripPhoto, TripsManifest } from "./types.ts";
import { vendorLeaflet } from "./vendor.ts";

export type BuildTripsOptions = {
  projectRoot: string;
  tripsRoot?: string;
  siteRoot?: string;
  cacheRoot?: string;
};

export async function buildTrips(
  options: BuildTripsOptions,
): Promise<TripsManifest> {
  // Avoid libvips racing many open path handles under Node 22+.
  sharp.concurrency(1);
  sharp.cache(false);
  const projectRoot = options.projectRoot;
  const tripsRoot = options.tripsRoot ?? path.join(projectRoot, "trips");
  const siteRoot = options.siteRoot ?? path.join(projectRoot, "_site");
  const cacheRoot = options.cacheRoot ?? path.join(projectRoot, "_cache");

  const tileCacheDir = path.join(cacheRoot, "tiles", TILE_SOURCE_ID);
  const tripsCacheDir = path.join(cacheRoot, "trips");
  const manifestPath = path.join(tripsCacheDir, "manifest.json");

  vendorLeaflet(projectRoot, siteRoot);

  const discovered = discoverTrips(tripsRoot);
  const trips: TripManifest[] = [];

  for (const trip of discovered) {
    console.log(`[trips] building ${trip.slug}…`);
    const siteTripDir = path.join(siteRoot, "trips", trip.slug);
    const cacheTripDir = path.join(tripsCacheDir, trip.slug);
    fs.mkdirSync(siteTripDir, { recursive: true });
    fs.mkdirSync(cacheTripDir, { recursive: true });

    const { tracks, bounds: trackBounds } = processGpxFiles(
      trip.gpxFiles,
      trip.slug,
      siteTripDir,
    );

    const photoMetas: Array<{
      src: string;
      filename: string;
      capturedAt: Date;
      displayCapturedAt: string;
      lat: number | null;
      lon: number | null;
    }> = [];

    for (const src of trip.images) {
      const exif = await readPhotoExif(src);
      photoMetas.push({
        src,
        filename: path.basename(src),
        capturedAt: exif.capturedAt,
        displayCapturedAt: exif.displayCapturedAt,
        lat: exif.lat,
        lon: exif.lon,
      });
    }

    photoMetas.sort((a, b) => +a.capturedAt - +b.capturedAt);

    const photos: TripPhoto[] = [];
    const photoBounds = emptyBBox();

    for (const meta of photoMetas) {
      const derivatives = await processPhotoImages(
        meta.src,
        trip.slug,
        meta.filename,
        cacheTripDir,
        siteTripDir,
      );
      const stem = path.basename(meta.filename, path.extname(meta.filename));
      if (meta.lat != null && meta.lon != null) {
        expandBBox(photoBounds, meta.lat, meta.lon);
      }
      photos.push({
        id: stem,
        basename: stem,
        filename: meta.filename,
        capturedAt: formatIsoCapturedAt(meta.capturedAt),
        displayCapturedAt: meta.displayCapturedAt,
        lat: meta.lat,
        lon: meta.lon,
        gridThumbUrl: derivatives.gridThumbRel,
        mapThumbUrl: derivatives.mapThumbRel,
        displayUrl: derivatives.displayRel,
        originalUrl: derivatives.originalRel,
        photoPageUrl: `/trips/${trip.slug}/photo/${stem}.html`,
      });
    }

    let bounds = trackBounds;
    if (isValidBBox(photoBounds)) {
      bounds = isValidBBox(trackBounds)
        ? mergeBBox(trackBounds, photoBounds)
        : photoBounds;
    }
    if (!isValidBBox(bounds)) {
      // Fallback tiny box if somehow empty
      bounds = { south: 0, west: 0, north: 0.01, east: 0.01 };
    }
    bounds = padBBox(bounds, 0.1);

    const mapDir = path.join(siteTripDir, "map");
    fs.mkdirSync(mapDir, { recursive: true });
    const basemapPath = path.join(mapDir, "basemap.jpg");
    const tracksPath = path.join(mapDir, "tracks.json");
    const mapScriptPath = path.join(mapDir, "map.js");

    // Cache basemap by tile source + bounds
    const boundsKey = JSON.stringify({ source: TILE_SOURCE_ID, bounds });
    const basemapMeta = path.join(cacheTripDir, "basemap.key");
    const basemapCache = path.join(cacheTripDir, "basemap.jpg");
    if (
      fs.existsSync(basemapCache) &&
      fs.existsSync(basemapMeta) &&
      fs.readFileSync(basemapMeta, "utf8") === boundsKey
    ) {
      fs.copyFileSync(basemapCache, basemapPath);
    } else {
      await renderBasemap(bounds, basemapCache, tileCacheDir);
      fs.writeFileSync(basemapMeta, boundsKey);
      fs.copyFileSync(basemapCache, basemapPath);
    }

    writeTracksJson(tracks, tracksPath);
    writeMapScript(mapScriptPath);

    const coverThumbUrl = photos[0]?.gridThumbUrl ?? null;

    const mapPhotosJson = JSON.stringify(
      photos
        .filter((p) => p.lat != null && p.lon != null)
        .map((p) => ({
          lat: p.lat,
          lon: p.lon,
          thumb: p.mapThumbUrl,
          url: p.photoPageUrl,
        })),
    );

    trips.push({
      slug: trip.slug,
      title: trip.title,
      period: trip.period,
      from: trip.from,
      to: trip.to,
      dateRange: trip.dateRange,
      url: `/trips/${trip.slug}/`,
      coverThumbUrl,
      photoCount: photos.length,
      trackCount: tracks.length,
      bounds,
      boundsJson: JSON.stringify(bounds),
      mapPhotosJson,
      basemapUrl: `/trips/${trip.slug}/map/basemap.jpg`,
      tracksJsonUrl: `/trips/${trip.slug}/map/tracks.json`,
      mapScriptUrl: `/trips/${trip.slug}/map/map.js`,
      photos,
      tracks: tracks.map((t) => ({
        id: t.id,
        filename: t.filename,
        color: t.color,
        gpxUrl: t.gpxUrl,
      })),
    });
  }

  const manifest: TripsManifest = {
    generatedAt: new Date().toISOString(),
    trips,
  };

  fs.mkdirSync(path.dirname(manifestPath), { recursive: true });
  fs.writeFileSync(manifestPath, JSON.stringify(manifest, null, 2));

  // Also write a lightweight trips index asset dir
  fs.mkdirSync(path.join(siteRoot, "trips"), { recursive: true });

  return manifest;
}

export function readTripsManifest(projectRoot: string): TripsManifest {
  const manifestPath = path.join(
    projectRoot,
    "_cache",
    "trips",
    "manifest.json",
  );
  if (!fs.existsSync(manifestPath)) {
    return { generatedAt: "", trips: [] };
  }
  return JSON.parse(fs.readFileSync(manifestPath, "utf8")) as TripsManifest;
}

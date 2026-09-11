import fs from "node:fs";
import path from "node:path";
import sharp from "sharp";
import { renderTripTiles, TILE_SOURCE_ID } from "./basemap.ts";
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
import { processGpxFiles, writeTracksJson, formatDistanceKm } from "./gpx.ts";
import { processPhotoImages, formatFileSize } from "./images.ts";
import { writeMapScript } from "./mapScript.ts";
import { formatTripDateRange } from "./metadata.ts";
import type { TripManifest, TripPhoto, TripsManifest } from "./types.ts";
import type { BBox } from "./types.ts";
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
  sharp.concurrency(1);
  sharp.cache(false);
  const projectRoot = options.projectRoot;
  const tripsRoot = options.tripsRoot ?? path.join(projectRoot, "trips");
  const siteRoot = options.siteRoot ?? path.join(projectRoot, "_site");
  const cacheRoot = options.cacheRoot ?? path.join(projectRoot, "_cache");

  const colorCacheDir = path.join(cacheRoot, "tiles", TILE_SOURCE_ID);
  const greyCacheDir = path.join(cacheRoot, "tiles", `${TILE_SOURCE_ID}-grey`);
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

    const {
      tracks,
      bounds: trackBounds,
      distanceMeters,
      from,
      to,
    } = processGpxFiles(trip.gpxFiles, trip.slug, siteTripDir);
    const dateRange = formatTripDateRange(from, to);
    const distanceKm = formatDistanceKm(distanceMeters);

    const photoMetas: Array<{
      src: string;
      filename: string;
      capturedAt: Date;
      displayCapturedAt: string;
      lat: number;
      lon: number;
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
      expandBBox(photoBounds, meta.lat, meta.lon);
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
        originalSize: formatFileSize(fs.statSync(meta.src).size),
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
      bounds = { south: 0, west: 0, north: 0.01, east: 0.01 };
    }
    bounds = padBBox(bounds, 0.1);

    const mapDir = path.join(siteTripDir, "map");
    fs.mkdirSync(mapDir, { recursive: true });
    const tracksPath = path.join(mapDir, "tracks.json");
    const mapScriptPath = path.join(mapDir, "map.js");

    type TileCacheMeta = {
      version: 5;
      source: string;
      requestBounds: BBox;
      zoom: number;
      xMin: number;
      xMax: number;
      yMin: number;
      yMax: number;
      bounds: BBox;
      tileUrlTemplate: string;
    };
    const tilesMetaPath = path.join(cacheTripDir, "tiles.json");

    let tileSet: {
      zoom: number;
      xMin: number;
      xMax: number;
      yMin: number;
      yMax: number;
      bounds: BBox;
      tileUrlTemplate: string;
    } | null = null;

    let cacheHit = false;
    if (fs.existsSync(tilesMetaPath)) {
      try {
        const cached = JSON.parse(
          fs.readFileSync(tilesMetaPath, "utf8"),
        ) as TileCacheMeta;
        if (
          cached.version === 5 &&
          cached.source === TILE_SOURCE_ID &&
          JSON.stringify(cached.requestBounds) === JSON.stringify(bounds)
        ) {
          tileSet = {
            zoom: cached.zoom,
            xMin: cached.xMin,
            xMax: cached.xMax,
            yMin: cached.yMin,
            yMax: cached.yMax,
            bounds: cached.bounds,
            tileUrlTemplate: cached.tileUrlTemplate,
          };
          // Re-copy greyscale tiles from cache into _site
          const z = cached.zoom;
          let missing = false;
          outer: for (let ty = cached.yMin; ty <= cached.yMax; ty++) {
            for (let tx = cached.xMin; tx <= cached.xMax; tx++) {
              const from = path.join(
                greyCacheDir,
                String(z),
                String(tx),
                `${ty}.png`,
              );
              if (!fs.existsSync(from)) {
                missing = true;
                break outer;
              }
              const toDir = path.join(mapDir, "tiles", String(z), String(tx));
              fs.mkdirSync(toDir, { recursive: true });
              fs.copyFileSync(from, path.join(toDir, `${ty}.png`));
            }
          }
          if (missing) {
            tileSet = null;
            cacheHit = false;
          } else {
            cacheHit = true;
          }
        }
      } catch {
        cacheHit = false;
        tileSet = null;
      }
    }

    if (!cacheHit || !tileSet) {
      tileSet = await renderTripTiles(
        bounds,
        trip.slug,
        mapDir,
        colorCacheDir,
        greyCacheDir,
      );
      const meta: TileCacheMeta = {
        version: 5,
        source: TILE_SOURCE_ID,
        requestBounds: bounds,
        zoom: tileSet.zoom,
        xMin: tileSet.xMin,
        xMax: tileSet.xMax,
        yMin: tileSet.yMin,
        yMax: tileSet.yMax,
        bounds: tileSet.bounds,
        tileUrlTemplate: tileSet.tileUrlTemplate,
      };
      fs.writeFileSync(tilesMetaPath, JSON.stringify(meta));
      // Drop legacy basemap cache files
      for (const legacy of ["basemap.json", "basemap.jpg", "basemap.key"]) {
        const p = path.join(cacheTripDir, legacy);
        if (fs.existsSync(p)) fs.unlinkSync(p);
      }
    }

    writeTracksJson(tracks, tracksPath);
    writeMapScript(mapScriptPath);

    const coverThumbUrl = photos[0]?.gridThumbUrl ?? null;

    const mapPhotosJson = JSON.stringify(
      photos.map((p) => ({
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
      from,
      to,
      dateRange,
      distanceKm,
      url: `/trips/${trip.slug}/`,
      coverThumbUrl,
      photoCount: photos.length,
      trackCount: tracks.length,
      bounds: tileSet.bounds,
      boundsJson: JSON.stringify(tileSet.bounds),
      tileUrlTemplate: tileSet.tileUrlTemplate,
      tileZoom: tileSet.zoom,
      mapPhotosJson,
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

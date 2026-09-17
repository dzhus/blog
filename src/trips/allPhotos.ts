import fs from "node:fs";
import path from "node:path";
import {
  formatIsoCapturedAt,
  readPhotoExif,
} from "./exif.ts";
import { processPhotoImages, formatFileSize } from "./images.ts";
import type { AllPhotosManifest, SitePhoto, TripManifest } from "./types.ts";
import { localizeAllPhotos } from "./i18n.ts";

const IMAGE_EXT = new Set([".jpg", ".jpeg", ".png", ".webp"]);

/** Photos per all-photos listing page. */
export const ALL_PHOTOS_PAGE_SIZE = 100;

/** Entry URL for the all-photos section (newest page). */
export function allPhotosHomeUrl(
  lang: "ru" | "en",
  totalPhotos: number,
): string {
  const prefix = lang === "en" ? "/en" : "";
  if (totalPhotos <= ALL_PHOTOS_PAGE_SIZE) {
    return `${prefix}/photos/index.html`;
  }
  const pages = Math.ceil(totalPhotos / ALL_PHOTOS_PAGE_SIZE);
  return `${prefix}/photos/page/${pages}.html`;
}

export function discoverLoosePhotoFiles(photosRoot: string): string[] {
  if (!fs.existsSync(photosRoot)) return [];
  return fs
    .readdirSync(photosRoot)
    .filter((name) => {
      if (name.startsWith(".")) return false;
      return IMAGE_EXT.has(path.extname(name).toLowerCase());
    })
    .sort((a, b) => a.localeCompare(b, "en"))
    .map((name) => path.join(photosRoot, name));
}

async function buildLoosePhotos(
  photosRoot: string,
  siteRoot: string,
  cacheRoot: string,
): Promise<SitePhoto[]> {
  const files = discoverLoosePhotoFiles(photosRoot);
  if (files.length === 0) return [];

  console.log(`[photos] building ${files.length} loose photo(s)…`);
  const sitePhotosDir = path.join(siteRoot, "photos");
  const cachePhotosDir = path.join(cacheRoot, "photos", "loose");
  fs.mkdirSync(sitePhotosDir, { recursive: true });
  fs.mkdirSync(cachePhotosDir, { recursive: true });

  const metas: Array<{
    src: string;
    filename: string;
    capturedAt: Date;
    displayCapturedAt: string;
    exifTooltip: string;
    lat: number | null;
    lon: number | null;
    sourceSize: number;
  }> = [];

  for (const src of files) {
    const exif = await readPhotoExif(src, cachePhotosDir, { requireGps: false });
    metas.push({
      src,
      filename: path.basename(src),
      capturedAt: exif.capturedAt,
      displayCapturedAt: exif.displayCapturedAt,
      exifTooltip: exif.tooltip,
      lat: exif.lat,
      lon: exif.lon,
      sourceSize: exif.sourceSize,
    });
  }

  metas.sort((a, b) => +a.capturedAt - +b.capturedAt);

  const photos: SitePhoto[] = [];
  for (const meta of metas) {
    const derivatives = await processPhotoImages(
      meta.src,
      "/photos",
      meta.filename,
      cachePhotosDir,
      sitePhotosDir,
    );
    const stem = path.basename(meta.filename, path.extname(meta.filename));
    photos.push({
      id: stem,
      basename: stem,
      filename: meta.filename,
      capturedAt: formatIsoCapturedAt(meta.capturedAt),
      displayCapturedAt: meta.displayCapturedAt,
      exifTooltip: meta.exifTooltip,
      exifLine: meta.exifTooltip.split("\n").filter(Boolean).join(" · "),
      lat: meta.lat,
      lon: meta.lon,
      gridThumbUrl: derivatives.gridThumbRel,
      mapThumbUrl: derivatives.mapThumbRel,
      displayUrl: derivatives.displayRel,
      originalUrl: derivatives.originalRel,
      originalSize: formatFileSize(meta.sourceSize),
      photoPageUrl: `/photos/${stem}.html`,
      tripSlug: null,
    });
  }
  return photos;
}

function catalogEntry(photo: SitePhoto) {
  return {
    id: photo.id,
    basename: photo.basename,
    filename: photo.filename,
    displayUrl: photo.displayUrl,
    originalUrl: photo.originalUrl,
    originalSize: photo.originalSize,
    exifLine: photo.exifLine,
    photoPageUrl: photo.photoPageUrl,
    gridThumbUrl: photo.gridThumbUrl,
    tripSlug: photo.tripSlug,
    capturedAt: photo.capturedAt,
  };
}

/** Merge trip + loose photos oldest-first and write cache + site catalogs. */
export async function buildAllPhotos(
  trips: TripManifest[],
  options: {
    projectRoot: string;
    photosRoot?: string;
    siteRoot?: string;
    cacheRoot?: string;
  },
): Promise<AllPhotosManifest> {
  const projectRoot = options.projectRoot;
  const photosRoot = options.photosRoot ?? path.join(projectRoot, "photos");
  const siteRoot = options.siteRoot ?? path.join(projectRoot, "_site");
  const cacheRoot = options.cacheRoot ?? path.join(projectRoot, "_cache");

  const loose = await buildLoosePhotos(photosRoot, siteRoot, cacheRoot);
  const fromTrips: SitePhoto[] = trips.flatMap((trip) =>
    trip.photos.map((p) => ({
      ...p,
      tripSlug: p.tripSlug ?? trip.slug,
    })),
  );

  const photos = [...fromTrips, ...loose].sort((a, b) => {
    if (a.capturedAt !== b.capturedAt) {
      return a.capturedAt.localeCompare(b.capturedAt, "en");
    }
    const slugA = a.tripSlug ?? "";
    const slugB = b.tripSlug ?? "";
    if (slugA !== slugB) return slugA.localeCompare(slugB, "en");
    return a.basename.localeCompare(b.basename, "en");
  });

  const manifest: AllPhotosManifest = {
    generatedAt: new Date().toISOString(),
    photos,
  };

  const cacheManifestPath = path.join(cacheRoot, "photos", "manifest.json");
  fs.mkdirSync(path.dirname(cacheManifestPath), { recursive: true });
  fs.writeFileSync(cacheManifestPath, JSON.stringify(manifest, null, 2));

  const siteCatalogDir = path.join(siteRoot, "photos");
  fs.mkdirSync(siteCatalogDir, { recursive: true });
  fs.writeFileSync(
    path.join(siteCatalogDir, "catalog.json"),
    JSON.stringify({ photos: photos.map(catalogEntry) }),
  );

  const enPhotos = localizeAllPhotos(photos, "en");
  const enCatalogDir = path.join(siteRoot, "en", "photos");
  fs.mkdirSync(enCatalogDir, { recursive: true });
  fs.writeFileSync(
    path.join(enCatalogDir, "catalog.json"),
    JSON.stringify({ photos: enPhotos.map(catalogEntry) }),
  );

  return manifest;
}

export function readAllPhotosManifest(projectRoot: string): AllPhotosManifest {
  const manifestPath = path.join(
    projectRoot,
    "_cache",
    "photos",
    "manifest.json",
  );
  if (!fs.existsSync(manifestPath)) {
    return { generatedAt: "", photos: [] };
  }
  const manifest = JSON.parse(
    fs.readFileSync(manifestPath, "utf8"),
  ) as AllPhotosManifest;
  for (const photo of manifest.photos) {
    if (!photo.exifLine && photo.exifTooltip) {
      photo.exifLine = photo.exifTooltip.split("\n").filter(Boolean).join(" · ");
    }
    if (photo.tripSlug === undefined) {
      photo.tripSlug = null;
    }
  }
  return manifest;
}

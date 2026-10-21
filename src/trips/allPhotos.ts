import fs from "node:fs";
import path from "node:path";
import { asyncPool } from "./asyncPool.ts";
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

/** Log every N loose photos while building. */
const LOOSE_PHOTO_PROGRESS_EVERY = 100;

function logLoosePhotoProgress(done: number, total: number): void {
  if (done === total || done % LOOSE_PHOTO_PROGRESS_EVERY === 0) {
    console.log(`[photos] ${done}/${total}…`);
  }
}

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

/** Listing page URL that contains the photo at `photoIndex` (oldest-first). */
export function allPhotosListingUrl(
  lang: "ru" | "en",
  photoIndex: number,
  totalPhotos: number,
): string {
  const prefix = lang === "en" ? "/en" : "";
  if (totalPhotos <= ALL_PHOTOS_PAGE_SIZE || photoIndex < ALL_PHOTOS_PAGE_SIZE) {
    return `${prefix}/photos/index.html`;
  }
  const pageNumber = Math.floor(photoIndex / ALL_PHOTOS_PAGE_SIZE);
  return `${prefix}/photos/page/${pageNumber + 1}.html`;
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
  concurrency: number,
): Promise<SitePhoto[]> {
  const files = discoverLoosePhotoFiles(photosRoot);
  if (files.length === 0) return [];

  const total = files.length;
  console.log(
    `[photos] building ${total} loose photo(s)… concurrency=${concurrency}`,
  );
  const sitePhotosDir = path.join(siteRoot, "photos");
  const cachePhotosDir = path.join(cacheRoot, "photos", "loose");
  fs.mkdirSync(sitePhotosDir, { recursive: true });
  fs.mkdirSync(cachePhotosDir, { recursive: true });

  let done = 0;
  const photos = await asyncPool(concurrency, files, async (src) => {
    const filename = path.basename(src);
    const exif = await readPhotoExif(src, cachePhotosDir, { requireGps: false });
    const derivatives = await processPhotoImages(
      src,
      "/photos",
      filename,
      cachePhotosDir,
      sitePhotosDir,
    );
    const stem = path.basename(filename, path.extname(filename));
    done += 1;
    logLoosePhotoProgress(done, total);
    return {
      id: stem,
      basename: stem,
      filename,
      capturedAt: formatIsoCapturedAt(exif.capturedAt),
      displayCapturedAt: exif.displayCapturedAt,
      exifTooltip: exif.tooltip,
      exifLine: exif.tooltip.split("\n").filter(Boolean).join(" · "),
      lat: exif.lat,
      lon: exif.lon,
      gridThumbUrl: derivatives.gridThumbRel,
      mapThumbUrl: derivatives.mapThumbRel,
      displayUrl: derivatives.displayRel,
      originalUrl: derivatives.originalRel,
      originalSize: formatFileSize(exif.sourceSize),
      photoPageUrl: `/photos/${stem}.html`,
      tripSlug: null,
    } satisfies SitePhoto;
  });

  photos.sort((a, b) => a.capturedAt.localeCompare(b.capturedAt, "en"));
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
    /** Parallel loose-photo builds (same pool size as trip builds). */
    concurrency: number;
  },
): Promise<AllPhotosManifest> {
  const projectRoot = options.projectRoot;
  const photosRoot = options.photosRoot ?? path.join(projectRoot, "photos");
  const siteRoot = options.siteRoot ?? path.join(projectRoot, "_site");
  const cacheRoot = options.cacheRoot ?? path.join(projectRoot, "_cache");

  const loose = await buildLoosePhotos(
    photosRoot,
    siteRoot,
    cacheRoot,
    options.concurrency,
  );
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

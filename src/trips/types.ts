import type { TripNameEntry } from "./i18n.ts";

export type LatLon = { lat: number; lon: number };

export type BBox = {
  south: number;
  west: number;
  north: number;
  east: number;
};

export type TripPhoto = {
  id: string;
  basename: string;
  filename: string;
  capturedAt: string;
  displayCapturedAt: string;
  /** Hover tooltip with date/time, exposure, GPS. */
  exifTooltip: string;
  /** Single-line EXIF summary for display views. */
  exifLine: string;
  /** WGS84 latitude, or null when GPS EXIF is absent (loose photos). */
  lat: number | null;
  /** WGS84 longitude, or null when GPS EXIF is absent (loose photos). */
  lon: number | null;
  gridThumbUrl: string;
  mapThumbUrl: string;
  displayUrl: string;
  originalUrl: string;
  /** Human-readable size of the original file, e.g. "4.2 MB". */
  originalSize: string;
  photoPageUrl: string;
  /** Trip folder slug, or null for photos outside trips. */
  tripSlug: string | null;
};

/** Alias for photos in the unified all-photos catalog. */
export type SitePhoto = TripPhoto;

export type AllPhotosManifest = {
  generatedAt: string;
  photos: SitePhoto[];
};

export type TripTrack = {
  id: string;
  filename: string;
  color: string;
  gpxUrl: string;
  coordinates: [number, number][];
};

export type TripManifest = {
  slug: string;
  /** Effective title for the current language (set by localizeTrip). */
  title: string;
  /** Slug-derived title fallback. */
  folderTitle: string;
  /** Top-level trip.yml `name`, if any. */
  name?: string;
  /** Localized titles from trip.yml `names`. */
  names?: TripNameEntry[];
  period: string;
  from: string;
  to: string;
  dateRange: string;
  /** Raw GPX distance; format per language via localizeTrip. */
  distanceMeters: number | null;
  /** Formatted distance for the current language (set by localizeTrip). */
  distanceKm: string | null;
  url: string;
  coverThumbUrl: string | null;
  photoCount: number;
  trackCount: number;
  bounds: BBox;
  boundsJson: string;
  tileUrlTemplate: string;
  tileZoom: number;
  mapPhotosJson: string;
  tracksJsonUrl: string;
  photos: TripPhoto[];
  tracks: Array<{
    id: string;
    filename: string;
    color: string;
    gpxUrl: string;
  }>;
};

export type TripsManifest = {
  generatedAt: string;
  trips: TripManifest[];
};

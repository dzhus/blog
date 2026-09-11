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
  lat: number;
  lon: number;
  gridThumbUrl: string;
  mapThumbUrl: string;
  displayUrl: string;
  originalUrl: string;
  /** Human-readable size of the original file, e.g. "4.2 MB". */
  originalSize: string;
  photoPageUrl: string;
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
  title: string;
  period: string;
  from: string;
  to: string;
  dateRange: string;
  distanceKm: string | null;
  url: string;
  coverThumbUrl: string | null;
  photoCount: number;
  trackCount: number;
  bounds: BBox;
  boundsJson: string;
  /** Exact WGS84 coverage of fetched tiles (may extend past fit bounds). */
  tileBoundsJson: string;
  /** Web Mercator width/height of the fitted trip bbox; used for CSS aspect-ratio. */
  mapAspect: number;
  tileUrlTemplate: string;
  tileZoom: number;
  mapPhotosJson: string;
  tracksJsonUrl: string;
  mapScriptUrl: string;
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

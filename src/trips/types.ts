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
  lat: number | null;
  lon: number | null;
  gridThumbUrl: string;
  mapThumbUrl: string;
  displayUrl: string;
  originalUrl: string;
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
  from: string | null;
  to: string | null;
  dateRange: string | null;
  url: string;
  coverThumbUrl: string | null;
  photoCount: number;
  trackCount: number;
  bounds: BBox;
  boundsJson: string;
  mapPhotosJson: string;
  basemapUrl: string;
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

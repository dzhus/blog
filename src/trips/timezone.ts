import { find } from "geo-tz";

/** Resolve an IANA timezone id for GPS coordinates, or null if unknown. */
export function findTimeZone(lat: number, lon: number): string | null {
  if (!Number.isFinite(lat) || !Number.isFinite(lon)) return null;
  if (lat < -90 || lat > 90 || lon < -180 || lon > 180) return null;
  try {
    const zones = find(lat, lon);
    const zone = zones[0];
    return typeof zone === "string" && zone.length > 0 ? zone : null;
  } catch {
    return null;
  }
}

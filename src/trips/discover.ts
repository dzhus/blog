import fs from "node:fs";
import path from "node:path";
import { formatTripDateRange, readTripMetadata } from "./metadata.ts";

const IMAGE_EXT = new Set([".jpg", ".jpeg", ".png", ".webp"]);

export type DiscoveredTrip = {
  slug: string;
  title: string;
  period: string;
  dateRange: string | null;
  from: string | null;
  to: string | null;
  dir: string;
  images: string[];
  gpxFiles: string[];
};

function parseSlug(slug: string): { title: string; period: string } {
  const m = /^(\d{4}-\d{2})-(.+)$/.exec(slug);
  if (m) {
    return { period: m[1]!, title: m[2]!.replace(/-/g, " ") };
  }
  return { period: "", title: slug.replace(/-/g, " ") };
}

export function discoverTrips(tripsRoot: string): DiscoveredTrip[] {
  if (!fs.existsSync(tripsRoot)) return [];

  const entries = fs
    .readdirSync(tripsRoot, { withFileTypes: true })
    .filter((d) => d.isDirectory() && !d.name.startsWith("."));

  const trips: DiscoveredTrip[] = [];

  for (const entry of entries) {
    const dir = path.join(tripsRoot, entry.name);
    const files = fs.readdirSync(dir);
    const images = files
      .filter((f) => IMAGE_EXT.has(path.extname(f).toLowerCase()))
      .map((f) => path.join(dir, f))
      .sort();
    const gpxFiles = files
      .filter((f) => path.extname(f).toLowerCase() === ".gpx")
      .map((f) => path.join(dir, f))
      .sort();

    if (images.length === 0 && gpxFiles.length === 0) continue;

    const { title: folderTitle, period } = parseSlug(entry.name);
    const meta = readTripMetadata(dir);

    trips.push({
      slug: entry.name,
      title: meta.name ?? folderTitle,
      period,
      dateRange: formatTripDateRange(meta.from, meta.to),
      from: meta.from ?? null,
      to: meta.to ?? null,
      dir,
      images,
      gpxFiles,
    });
  }

  trips.sort((a, b) => b.slug.localeCompare(a.slug, "en"));
  return trips;
}

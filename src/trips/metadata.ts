import fs from "node:fs";
import path from "node:path";
import { load as loadYaml } from "js-yaml";

const META_FILENAMES = ["trip.yml", "trip.yaml", "metadata.yml", "metadata.yaml"];

export type TripMetadata = {
  name?: string;
  /** Basename of a trip photo to use as the listing cover (e.g. "2026-08-08-07-33.jpg"). */
  thumbnail?: string;
};

function isPlainObject(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null && !Array.isArray(v);
}

export function findMetadataPath(tripDir: string): string | null {
  for (const name of META_FILENAMES) {
    const p = path.join(tripDir, name);
    if (fs.existsSync(p) && fs.statSync(p).isFile()) return p;
  }
  return null;
}

export function readTripMetadata(tripDir: string): TripMetadata {
  const metaPath = findMetadataPath(tripDir);
  if (!metaPath) return {};

  const fileLabel = path.relative(process.cwd(), metaPath) || metaPath;
  let raw: unknown;
  try {
    raw = loadYaml(fs.readFileSync(metaPath, "utf8"));
  } catch (err) {
    const detail = err instanceof Error ? err.message : String(err);
    throw new Error(`Failed to parse trip metadata ${fileLabel}: ${detail}`);
  }

  if (raw == null) return {};
  if (!isPlainObject(raw)) {
    throw new Error(`Trip metadata ${fileLabel} must be a YAML mapping`);
  }

  const meta: TripMetadata = {};

  if (raw.name !== undefined) {
    if (typeof raw.name !== "string" || raw.name.trim() === "") {
      throw new Error(
        `Invalid name in ${fileLabel}: expected a non-empty string`,
      );
    }
    meta.name = raw.name.trim();
  }

  if (raw.thumbnail !== undefined) {
    if (typeof raw.thumbnail !== "string" || raw.thumbnail.trim() === "") {
      throw new Error(
        `Invalid thumbnail in ${fileLabel}: expected a non-empty filename`,
      );
    }
    const thumb = raw.thumbnail.trim();
    if (thumb.includes("/") || thumb.includes("\\") || thumb === ".." || thumb === ".") {
      throw new Error(
        `Invalid thumbnail in ${fileLabel}: expected a bare filename in the trip folder, got ${JSON.stringify(raw.thumbnail)}`,
      );
    }
    meta.thumbnail = thumb;
  }

  const allowed = new Set(["name", "thumbnail"]);
  for (const key of Object.keys(raw)) {
    if (!allowed.has(key)) {
      throw new Error(
        `Unknown key "${key}" in ${fileLabel}. Allowed: name, thumbnail`,
      );
    }
  }

  return meta;
}

/** Human-readable range for listings / trip pages. */
export function formatTripDateRange(
  from?: string | null,
  to?: string | null,
): string | null {
  if (from && to) {
    if (from === to) return from;
    return `${from} — ${to}`;
  }
  if (from) return from;
  if (to) return to;
  return null;
}

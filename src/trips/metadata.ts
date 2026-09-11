import fs from "node:fs";
import path from "node:path";
import { load as loadYaml } from "js-yaml";

const META_FILENAMES = ["trip.yml", "trip.yaml", "metadata.yml", "metadata.yaml"];

const DATE_RE = /^\d{4}-\d{2}-\d{2}$/;

export type TripMetadata = {
  name?: string;
  from?: string;
  to?: string;
};

function isPlainObject(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null && !Array.isArray(v);
}

function validateDate(value: unknown, field: string, fileLabel: string): string {
  let iso: string | null = null;

  if (typeof value === "string") {
    iso = value.trim();
  } else if (value instanceof Date && !Number.isNaN(+value)) {
    iso = value.toISOString().slice(0, 10);
  }

  if (!iso || !DATE_RE.test(iso)) {
    throw new Error(
      `Invalid ${field} in ${fileLabel}: expected YYYY-MM-DD, got ${JSON.stringify(value)}`,
    );
  }

  const d = new Date(`${iso}T00:00:00Z`);
  if (Number.isNaN(+d) || d.toISOString().slice(0, 10) !== iso) {
    throw new Error(`Invalid calendar date for ${field} in ${fileLabel}: ${iso}`);
  }
  return iso;
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

  if (raw.from !== undefined) {
    meta.from = validateDate(raw.from, "from", fileLabel);
  }
  if (raw.to !== undefined) {
    meta.to = validateDate(raw.to, "to", fileLabel);
  }

  if (meta.from && meta.to && meta.from > meta.to) {
    throw new Error(
      `Invalid date range in ${fileLabel}: from (${meta.from}) is after to (${meta.to})`,
    );
  }

  const allowed = new Set(["name", "from", "to"]);
  for (const key of Object.keys(raw)) {
    if (!allowed.has(key)) {
      throw new Error(
        `Unknown key "${key}" in ${fileLabel}. Allowed: name, from, to`,
      );
    }
  }

  return meta;
}

/** Human-readable range for listings / trip pages. */
export function formatTripDateRange(
  from?: string,
  to?: string,
): string | null {
  if (from && to) {
    if (from === to) return from;
    return `${from} — ${to}`;
  }
  if (from) return from;
  if (to) return to;
  return null;
}

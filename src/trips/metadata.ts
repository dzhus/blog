import fs from "node:fs";
import path from "node:path";
import { load as loadYaml } from "js-yaml";
import { isTripLang, type TripLang, type TripNameEntry } from "./i18n.ts";

const META_FILENAMES = ["trip.yml", "trip.yaml", "metadata.yml", "metadata.yaml"];

export type TripMetadata = {
  name?: string;
  names?: TripNameEntry[];
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

  if (raw.names !== undefined) {
    if (!Array.isArray(raw.names) || raw.names.length === 0) {
      throw new Error(
        `Invalid names in ${fileLabel}: expected a non-empty list of { lang, name }`,
      );
    }
    const names: TripNameEntry[] = [];
    const seen = new Set<TripLang>();
    for (let i = 0; i < raw.names.length; i++) {
      const entry = raw.names[i];
      if (!isPlainObject(entry)) {
        throw new Error(
          `Invalid names[${i}] in ${fileLabel}: expected a mapping with lang and name`,
        );
      }
      if (!isTripLang(entry.lang)) {
        throw new Error(
          `Invalid names[${i}].lang in ${fileLabel}: expected one of ru, en, got ${JSON.stringify(entry.lang)}`,
        );
      }
      if (typeof entry.name !== "string" || entry.name.trim() === "") {
        throw new Error(
          `Invalid names[${i}].name in ${fileLabel}: expected a non-empty string`,
        );
      }
      if (seen.has(entry.lang)) {
        throw new Error(
          `Duplicate names lang ${JSON.stringify(entry.lang)} in ${fileLabel}`,
        );
      }
      seen.add(entry.lang);
      names.push({ lang: entry.lang, name: entry.name.trim() });
    }
    meta.names = names;
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

  const allowed = new Set(["name", "names", "thumbnail"]);
  for (const key of Object.keys(raw)) {
    if (!allowed.has(key)) {
      throw new Error(
        `Unknown key "${key}" in ${fileLabel}. Allowed: name, names, thumbnail`,
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

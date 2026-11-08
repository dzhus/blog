import fs from "node:fs";
import path from "node:path";
import { load as loadYaml } from "js-yaml";
import type { TripLang, TripNameEntry } from "./i18n.ts";
import { parseNamesEntries } from "./metadata.ts";

export type TripNameAlias = {
  names: TripNameEntry[];
};

const cache = new Map<string, TripNameAlias[]>();

function isPlainObject(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null && !Array.isArray(v);
}

function escapeRegExp(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

/** Replace whole-word/phrase `from` with `to` using Unicode letter/number boundaries. */
function replaceWholeWord(haystack: string, from: string, to: string): string {
  if (!from || from === to) return haystack;
  const re = new RegExp(
    `(?<![\\p{L}\\p{N}_])${escapeRegExp(from)}(?![\\p{L}\\p{N}_])`,
    "gu",
  );
  return haystack.replace(re, to);
}

export function aliasesPath(tripsRoot?: string): string {
  return path.join(tripsRoot ?? path.join(process.cwd(), "trips"), "aliases.yml");
}

export function loadTripAliases(tripsRoot?: string): TripNameAlias[] {
  const filePath = path.resolve(aliasesPath(tripsRoot));
  const cached = cache.get(filePath);
  if (cached) return cached;

  if (!fs.existsSync(filePath) || !fs.statSync(filePath).isFile()) {
    cache.set(filePath, []);
    return [];
  }

  const fileLabel = path.relative(process.cwd(), filePath) || filePath;
  let raw: unknown;
  try {
    raw = loadYaml(fs.readFileSync(filePath, "utf8"));
  } catch (err) {
    const detail = err instanceof Error ? err.message : String(err);
    throw new Error(`Failed to parse trip aliases ${fileLabel}: ${detail}`);
  }

  if (raw == null) {
    cache.set(filePath, []);
    return [];
  }
  if (!isPlainObject(raw)) {
    throw new Error(`Trip aliases ${fileLabel} must be a YAML mapping`);
  }

  const allowed = new Set(["aliases"]);
  for (const key of Object.keys(raw)) {
    if (!allowed.has(key)) {
      throw new Error(
        `Unknown key "${key}" in ${fileLabel}. Allowed: aliases`,
      );
    }
  }

  if (raw.aliases === undefined) {
    throw new Error(`Missing top-level "aliases" key in ${fileLabel}`);
  }
  if (!Array.isArray(raw.aliases)) {
    throw new Error(`Invalid aliases in ${fileLabel}: expected a list`);
  }

  const aliases: TripNameAlias[] = [];
  for (let i = 0; i < raw.aliases.length; i++) {
    const entry = raw.aliases[i];
    if (!isPlainObject(entry)) {
      throw new Error(
        `Invalid aliases[${i}] in ${fileLabel}: expected a mapping with names`,
      );
    }
    for (const key of Object.keys(entry)) {
      if (key !== "names") {
        throw new Error(
          `Unknown key "${key}" in aliases[${i}] of ${fileLabel}. Allowed: names`,
        );
      }
    }
    if (entry.names === undefined) {
      throw new Error(`Missing names in aliases[${i}] of ${fileLabel}`);
    }
    const names = parseNamesEntries(
      entry.names,
      fileLabel,
      `aliases[${i}].names`,
    );
    if (names.length < 2) {
      throw new Error(
        `Invalid aliases[${i}].names in ${fileLabel}: expected at least two language entries`,
      );
    }
    aliases.push({ names });
  }

  cache.set(filePath, aliases);
  return aliases;
}

/** Apply linguistic aliases as whole-word replacements for the target language. */
export function applyNameAliases(
  title: string,
  lang: TripLang,
  aliases: TripNameAlias[],
): string {
  let result = title;
  for (const alias of aliases) {
    const target = alias.names.find((n) => n.lang === lang)?.name;
    if (!target) continue;
    const sources = alias.names
      .filter((n) => n.lang !== lang && n.name !== target)
      .map((n) => n.name)
      .sort((a, b) => b.length - a.length);
    for (const source of sources) {
      result = replaceWholeWord(result, source, target);
    }
  }
  return result;
}

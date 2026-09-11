import { execFileSync } from "node:child_process";
import { statSync } from "node:fs";

const cache = new Map<string, Date>();

/** Latest git author date for a file, falling back to filesystem mtime. */
export function getGitAuthorDate(filePath: string): Date {
  const cached = cache.get(filePath);
  if (cached) return cached;

  try {
    const out = execFileSync(
      "git",
      ["log", "-n1", "--format=%aI", "--", filePath],
      { encoding: "utf8" },
    ).trim();
    if (out) {
      const d = new Date(out);
      if (!Number.isNaN(d.getTime())) {
        cache.set(filePath, d);
        return d;
      }
    }
  } catch {
    // fall through
  }

  const d = statSync(filePath).mtime;
  cache.set(filePath, d);
  return d;
}

export function formatIsoDate(d: Date): string {
  return d.toISOString().slice(0, 10);
}

export function formatIso8601(d: Date): string {
  return d.toISOString().replace(/\.\d{3}Z$/, "Z");
}

/** Format as dd.mm.yyyy */
export function formatDisplayDate(d: Date): string {
  const dd = String(d.getUTCDate()).padStart(2, "0");
  const mm = String(d.getUTCMonth() + 1).padStart(2, "0");
  const yyyy = d.getUTCFullYear();
  return `${dd}.${mm}.${yyyy}`;
}

/** Parse YYYY-MM-DD from a post filename like 2019-03-19-thirty.md */
export function dateFromFilename(fileSlugOrPath: string): Date | undefined {
  const base = fileSlugOrPath.split("/").pop() ?? fileSlugOrPath;
  const m = base.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (!m) return undefined;
  return new Date(Date.UTC(Number(m[1]), Number(m[2]) - 1, Number(m[3])));
}

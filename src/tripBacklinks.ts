import fs from "node:fs";
import path from "node:path";
import matter from "gray-matter";
import { dateFromFilename } from "./gitDates.ts";
import type { TripLang } from "./trips/i18n.ts";
import { extractLeadingH1, resolveTitle } from "./title.ts";

export type TripBacklinkPost = {
  title: string;
  url: string;
};

/** lang → trip slug → posts (newest first). */
export type TripBacklinksIndex = Record<
  TripLang,
  Record<string, TripBacklinkPost[]>
>;

/** lang → trip slug → photo basename → posts (newest first). */
export type TripPhotoBacklinksIndex = Record<
  TripLang,
  Record<string, Record<string, TripBacklinkPost[]>>
>;

export type TripBacklinksResult = {
  byTrip: TripBacklinksIndex;
  byPhoto: TripPhotoBacklinksIndex;
};

/** Capture trip slug and optional photo stem from href-like URLs. */
const TRIP_HREF_RE =
  /(?:^|["'(\s])\/?(?:en\/)?trips\/([^/?#"'\s>]+)(?:\/photo\/([^/"'\s>]+)\.html|\/index\.html|\/)?/g;

type PostRef = {
  title: string;
  url: string;
  lang: TripLang;
  sortMs: number;
  basename: string;
};

function emptyTripIndex(): TripBacklinksIndex {
  return { ru: {}, en: {} };
}

function emptyPhotoIndex(): TripPhotoBacklinksIndex {
  return { ru: {}, en: {} };
}

function postLang(data: Record<string, unknown>): TripLang {
  return data.lang === "en" ? "en" : "ru";
}

function postPermalink(basename: string, lang: TripLang): string {
  return lang === "en"
    ? `/en/posts/${basename}.html`
    : `/posts/${basename}.html`;
}

function sortPosts(postsMap: Map<string, PostRef>): TripBacklinkPost[] {
  return [...postsMap.values()]
    .sort((a, b) => {
      if (b.sortMs !== a.sortMs) return b.sortMs - a.sortMs;
      return b.basename.localeCompare(a.basename);
    })
    .map(({ title, url }) => ({ title, url }));
}

/** Trip slugs referenced by any trip/photo href in the body. */
export function tripSlugsFromBody(body: string): string[] {
  const slugs = new Set<string>();
  TRIP_HREF_RE.lastIndex = 0;
  let match: RegExpExecArray | null;
  while ((match = TRIP_HREF_RE.exec(body)) !== null) {
    const slug = match[1];
    if (slug && slug !== "index.html") slugs.add(slug);
  }
  return [...slugs];
}

/** Photo stems linked for a given trip slug (`slug` → stem[]). */
export function tripPhotoStemsFromBody(
  body: string,
): Map<string, Set<string>> {
  const bySlug = new Map<string, Set<string>>();
  TRIP_HREF_RE.lastIndex = 0;
  let match: RegExpExecArray | null;
  while ((match = TRIP_HREF_RE.exec(body)) !== null) {
    const slug = match[1];
    const stem = match[2];
    if (!slug || slug === "index.html" || !stem) continue;
    let set = bySlug.get(slug);
    if (!set) {
      set = new Set();
      bySlug.set(slug, set);
    }
    set.add(stem);
  }
  return bySlug;
}

export function buildTripBacklinks(postsDir: string): TripBacklinksResult {
  const byTrip = emptyTripIndex();
  const byPhoto = emptyPhotoIndex();
  if (!fs.existsSync(postsDir)) return { byTrip, byPhoto };

  const files = fs
    .readdirSync(postsDir)
    .filter((name) => name.endsWith(".md"))
    .sort();

  const tripMaps = new Map<string, Map<string, PostRef>>();
  const photoMaps = new Map<string, Map<string, PostRef>>();

  for (const file of files) {
    const filePath = path.join(postsDir, file);
    const basename = path.basename(file, ".md");
    const raw = fs.readFileSync(filePath, "utf8");
    const parsed = matter(raw);
    const lang = postLang(parsed.data as Record<string, unknown>);
    const { title: h1 } = extractLeadingH1(parsed.content, { strip: true });
    const yamlTitle =
      typeof parsed.data.title === "string" ? parsed.data.title : undefined;
    const title = resolveTitle(h1, yamlTitle, basename);
    const url = postPermalink(basename, lang);
    const date = dateFromFilename(basename) ?? new Date(0);
    const sortMs = date.getTime();
    const post: PostRef = { title, url, lang, sortMs, basename };

    const tripSlugs = tripSlugsFromBody(parsed.content);
    for (const slug of tripSlugs) {
      const key = `${lang}\0${slug}`;
      let map = tripMaps.get(key);
      if (!map) {
        map = new Map();
        tripMaps.set(key, map);
      }
      map.set(basename, post);
    }

    for (const [slug, stems] of tripPhotoStemsFromBody(parsed.content)) {
      for (const stem of stems) {
        const key = `${lang}\0${slug}\0${stem}`;
        let map = photoMaps.get(key);
        if (!map) {
          map = new Map();
          photoMaps.set(key, map);
        }
        map.set(basename, post);
      }
    }
  }

  for (const [key, postsMap] of tripMaps) {
    const [lang, slug] = key.split("\0") as [TripLang, string];
    byTrip[lang][slug] = sortPosts(postsMap);
  }

  for (const [key, postsMap] of photoMaps) {
    const [lang, slug, stem] = key.split("\0") as [TripLang, string, string];
    if (!byPhoto[lang][slug]) byPhoto[lang][slug] = {};
    byPhoto[lang][slug]![stem] = sortPosts(postsMap);
  }

  return { byTrip, byPhoto };
}

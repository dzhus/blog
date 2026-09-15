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

const TRIP_HREF_RE =
  /(?:^|["'(\s])(\/?(?:en\/)?trips\/([^/?#"'\s>]+)(?:\/(?:index\.html)?)?(?:\/photo\/[^/"'\s>]+\.html)?)/g;

type PostRef = {
  title: string;
  url: string;
  lang: TripLang;
  sortMs: number;
  basename: string;
};

function emptyIndex(): TripBacklinksIndex {
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

/** Collect unique trip slugs referenced by hrefs in markdown/HTML body. */
export function tripSlugsFromBody(body: string): string[] {
  const slugs = new Set<string>();
  TRIP_HREF_RE.lastIndex = 0;
  let match: RegExpExecArray | null;
  while ((match = TRIP_HREF_RE.exec(body)) !== null) {
    const slug = match[2];
    if (slug && slug !== "index.html") {
      slugs.add(slug);
    }
  }
  return [...slugs];
}

export function buildTripBacklinks(postsDir: string): TripBacklinksIndex {
  const index = emptyIndex();
  if (!fs.existsSync(postsDir)) return index;

  const files = fs
    .readdirSync(postsDir)
    .filter((name) => name.endsWith(".md"))
    .sort();

  const byLangSlug = new Map<string, Map<string, PostRef>>();

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

    const slugs = tripSlugsFromBody(parsed.content);
    if (slugs.length === 0) continue;

    const post: PostRef = { title, url, lang, sortMs, basename };
    for (const slug of slugs) {
      const key = `${lang}\0${slug}`;
      let map = byLangSlug.get(key);
      if (!map) {
        map = new Map();
        byLangSlug.set(key, map);
      }
      map.set(basename, post);
    }
  }

  for (const [key, postsMap] of byLangSlug) {
    const [lang, slug] = key.split("\0") as [TripLang, string];
    const posts = [...postsMap.values()].sort((a, b) => {
      if (b.sortMs !== a.sortMs) return b.sortMs - a.sortMs;
      return b.basename.localeCompare(a.basename);
    });
    if (!index[lang][slug]) index[lang][slug] = [];
    index[lang][slug] = posts.map(({ title, url }) => ({ title, url }));
  }

  return index;
}

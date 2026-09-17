import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import type { CollectionItem, UserConfig } from "@11ty/eleventy";
import pluginRss from "@11ty/eleventy-plugin-rss";
import { md } from "./src/markdown.ts";
import { extractLeadingH1 } from "./src/title.ts";
import {
  defaultCreator,
  defaultTitle,
  email,
  feedAuthorName,
  feedTitle,
  gravatar,
  languages,
  localizedMeta,
  rootUrl,
  thisYear,
} from "./src/siteConstants.ts";
import { collectTags, renderTagCloud } from "./src/tags.ts";
import { buildTrips, readTripsManifest } from "./src/trips/build.ts";
import {
  allPhotosHomeUrl,
  readAllPhotosManifest,
} from "./src/trips/allPhotos.ts";
import {
  localizeAllPhotos,
  localizeTrips,
  photosBase,
  tripUi,
} from "./src/trips/i18n.ts";
import type { SitePhoto, TripManifest, TripPhoto } from "./src/trips/types.ts";
import { buildTripBacklinks } from "./src/tripBacklinks.ts";
import { addTripAnchorIds } from "./src/tripAnchors.ts";
import type { SiteLang } from "./src/siteConstants.ts";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

export type TripPhotoPage = {
  trip: TripManifest;
  photo: TripPhoto;
  prev: TripPhoto | null;
  next: TripPhoto | null;
  index: number;
  total: number;
};

/** Permanent page for a loose (non-trip) photo. */
export type LoosePhotoPage = {
  photo: SitePhoto;
  prev: SitePhoto | null;
  next: SitePhoto | null;
  index: number;
  total: number;
};

function isEnglishPost(item: CollectionItem): boolean {
  return item.data.lang === "en";
}

function sortNewestFirst(a: CollectionItem, b: CollectionItem): number {
  return +b.date - +a.date;
}

export default function (eleventyConfig: UserConfig) {
  eleventyConfig.setWatchJavaScriptDependencies(false);

  eleventyConfig.addPlugin(pluginRss);

  eleventyConfig.setQuietMode(true);

  eleventyConfig.ignores.add("README.md");
  eleventyConfig.ignores.add("LICENSE");
  eleventyConfig.ignores.add("package.json");
  eleventyConfig.ignores.add("package-lock.json");
  eleventyConfig.ignores.add("tsconfig.json");
  eleventyConfig.ignores.add("src/**");
  eleventyConfig.ignores.add("node_modules/**");
  eleventyConfig.ignores.add("_cache/**");
  eleventyConfig.ignores.add(".stack-work/**");
  eleventyConfig.ignores.add(".github/**");
  eleventyConfig.ignores.add("*.cabal");
  eleventyConfig.ignores.add("package.yaml");
  eleventyConfig.ignores.add("stack.yaml");
  eleventyConfig.ignores.add("stack.yaml.lock");
  eleventyConfig.ignores.add("site.hs");
  eleventyConfig.ignores.add("trips/**");
  eleventyConfig.ignores.add("photos/**");

  eleventyConfig.addPassthroughCopy("images");
  eleventyConfig.addPassthroughCopy("css/default.css");
  eleventyConfig.addPassthroughCopy("css/trips.css");
  eleventyConfig.addPassthroughCopy("js");

  eleventyConfig.addGlobalData("languages", () => {
    const count = getAllPhotosManifest().photos.length;
    const patch = (lang: SiteLang) => ({
      ...languages[lang],
      allPhotosUrl: allPhotosHomeUrl(lang, count),
    });
    return { ru: patch("ru"), en: patch("en") };
  });
  eleventyConfig.addGlobalData("rootUrl", rootUrl);
  eleventyConfig.addGlobalData("gravatar", gravatar);
  eleventyConfig.addGlobalData("thisYear", thisYear());
  eleventyConfig.addGlobalData("creator", defaultCreator);
  eleventyConfig.addGlobalData("siteTitle", defaultTitle);
  eleventyConfig.addGlobalData("lang", "ru");
  eleventyConfig.addGlobalData("langPrefix", "");
  eleventyConfig.addGlobalData("site", () => ({
    rootUrl,
    email,
    gravatar,
    thisYear: thisYear(),
    feedAuthorName,
    feedTitle,
  }));
  eleventyConfig.addGlobalData("eleventyComputed.creator", () => {
    return (data: any) => localizedMeta(data.lang).creator;
  });
  eleventyConfig.addGlobalData("eleventyComputed.siteTitle", () => {
    return (data: any) => localizedMeta(data.lang).siteTitle;
  });
  eleventyConfig.addGlobalData("eleventyComputed.langPrefix", () => {
    return (data: any) => localizedMeta(data.lang).langPrefix;
  });
  eleventyConfig.addGlobalData("eleventyComputed.langDef", () => {
    return (data: any) => localizedMeta(data.lang);
  });

  let cachedManifest: ReturnType<typeof readTripsManifest> | null = null;
  let cachedAllPhotos: ReturnType<typeof readAllPhotosManifest> | null = null;
  function getTripsManifest() {
    if (!cachedManifest) {
      cachedManifest = readTripsManifest(__dirname);
    }
    return cachedManifest;
  }
  function getAllPhotosManifest() {
    if (!cachedAllPhotos) {
      cachedAllPhotos = readAllPhotosManifest(__dirname);
    }
    return cachedAllPhotos;
  }

  eleventyConfig.on("eleventy.before", async () => {
    cachedManifest = null;
    cachedAllPhotos = null;
    await buildTrips({ projectRoot: __dirname });
  });

  eleventyConfig.addGlobalData("tripUi", tripUi);

  let cachedBacklinks: ReturnType<typeof buildTripBacklinks> | null = null;
  function getTripBacklinks() {
    if (!cachedBacklinks) {
      cachedBacklinks = buildTripBacklinks(path.join(__dirname, "posts"));
    }
    return cachedBacklinks;
  }
  eleventyConfig.on("eleventy.before", () => {
    cachedBacklinks = null;
  });
  eleventyConfig.addGlobalData("tripBacklinks", () => getTripBacklinks().byTrip);
  eleventyConfig.addGlobalData(
    "tripPhotoBacklinks",
    () => getTripBacklinks().byPhoto,
  );
  eleventyConfig.addGlobalData(
    "loosePhotoBacklinks",
    () => getTripBacklinks().byLoosePhoto,
  );

  function tripPhotoPages(trips: TripManifest[]): TripPhotoPage[] {
    const pages: TripPhotoPage[] = [];
    for (const trip of trips) {
      const total = trip.photos.length;
      trip.photos.forEach((photo, index) => {
        pages.push({
          trip,
          photo,
          prev: index > 0 ? trip.photos[index - 1]! : null,
          next: index < total - 1 ? trip.photos[index + 1]! : null,
          index,
          total,
        });
      });
    }
    return pages;
  }

  function loosePhotoPages(photos: SitePhoto[]): LoosePhotoPage[] {
    const loose = photos.filter((p) => !p.tripSlug);
    const all = photos;
    return loose.map((photo) => {
      const globalIndex = all.findIndex(
        (p) =>
          p.basename === photo.basename &&
          p.tripSlug === photo.tripSlug &&
          p.photoPageUrl === photo.photoPageUrl,
      );
      const index = globalIndex >= 0 ? globalIndex : 0;
      return {
        photo,
        prev: index > 0 ? all[index - 1]! : null,
        next: index < all.length - 1 ? all[index + 1]! : null,
        index,
        total: all.length,
      };
    });
  }

  eleventyConfig.addGlobalData("trips", () => {
    return localizeTrips(getTripsManifest().trips, "ru");
  });

  eleventyConfig.addGlobalData("tripsEn", () => {
    return localizeTrips(getTripsManifest().trips, "en");
  });

  eleventyConfig.addGlobalData("tripPhotos", (): TripPhotoPage[] => {
    return tripPhotoPages(localizeTrips(getTripsManifest().trips, "ru"));
  });

  eleventyConfig.addGlobalData("tripPhotosEn", (): TripPhotoPage[] => {
    return tripPhotoPages(localizeTrips(getTripsManifest().trips, "en"));
  });

  eleventyConfig.addGlobalData("allPhotos", (): SitePhoto[] => {
    return localizeAllPhotos(getAllPhotosManifest().photos, "ru");
  });

  eleventyConfig.addGlobalData("allPhotosEn", (): SitePhoto[] => {
    return localizeAllPhotos(getAllPhotosManifest().photos, "en");
  });

  eleventyConfig.addGlobalData("loosePhotoPages", (): LoosePhotoPage[] => {
    return loosePhotoPages(localizeAllPhotos(getAllPhotosManifest().photos, "ru"));
  });

  eleventyConfig.addGlobalData("loosePhotoPagesEn", (): LoosePhotoPage[] => {
    return loosePhotoPages(localizeAllPhotos(getAllPhotosManifest().photos, "en"));
  });

  eleventyConfig.addGlobalData("photosBase", () => photosBase("ru"));
  eleventyConfig.addGlobalData("photosBaseEn", () => photosBase("en"));

  eleventyConfig.addCollection("postsAll", (api) =>
    api.getFilteredByGlob("posts/*.md").sort(sortNewestFirst),
  );

  eleventyConfig.addCollection("postsRu", (api) =>
    api
      .getFilteredByGlob("posts/*.md")
      .filter((item) => !isEnglishPost(item))
      .sort(sortNewestFirst),
  );

  eleventyConfig.addCollection("postsEn", (api) =>
    api
      .getFilteredByGlob("posts/*.md")
      .filter(isEnglishPost)
      .sort(sortNewestFirst),
  );

  eleventyConfig.addCollection("blogTags", (api) => {
    const posts = api.getFilteredByGlob("posts/*.md");
    const map = collectTags(posts);
    return [...map.entries()]
      .map(([tag, taggedPosts]) => ({
        tag,
        posts: [...taggedPosts].sort(sortNewestFirst),
      }))
      .sort((a, b) => a.tag.localeCompare(b.tag, "en"));
  });

  eleventyConfig.addFilter("tagCloud", (posts: unknown) => {
    const list = Array.isArray(posts) ? (posts as CollectionItem[]) : [];
    const map = collectTags(list);
    const counts = [...map.entries()].map(([tag, tagged]) => ({
      tag,
      count: tagged.length,
    }));
    return renderTagCloud(counts, 100, 150);
  });

  eleventyConfig.setLibrary("md", md);

  // Site post pages only — keep Atom/contentHtml free of scroll anchors.
  eleventyConfig.addTransform(
    "trip-anchor-ids",
    (content: string, outputPath: string | false) => {
      if (!outputPath || typeof outputPath !== "string") return content;
      const normalized = outputPath.replace(/\\/g, "/");
      if (!/\/(?:en\/)?posts\/[^/]+\.html$/.test(normalized)) return content;
      return addTripAnchorIds(content);
    },
  );

  eleventyConfig.addPreprocessor(
    "strip-post-h1",
    "md",
    (data: { page?: { inputPath?: string } }, content: string) => {
      const input = String(data.page?.inputPath ?? "").replace(/\\/g, "/");
      if (/(^|\/)posts\/[^/]+\.md$/.test(input)) {
        return extractLeadingH1(content, { strip: true }).body;
      }
      return content;
    },
  );
  eleventyConfig.addFilter("absoluteUrl", (url: string, base: string) => {
    try {
      return new URL(url, base).toString();
    } catch {
      return url;
    }
  });

  eleventyConfig.addFilter("head", (arr: unknown[], n: number) => {
    if (!Array.isArray(arr)) return [];
    return arr.slice(0, n);
  });

  eleventyConfig.addFilter("htmlDateString", (date: Date | string) => {
    const d = date instanceof Date ? date : new Date(date);
    return d.toISOString();
  });

  eleventyConfig.on("eleventy.before", () => {
    const candidates = [
      "node_modules/highlight.js/styles/atom-one-light.css",
      "node_modules/highlight.js/styles/stackoverflow-light.css",
      "node_modules/highlight.js/styles/vs.css",
    ];
    const dest = path.join(__dirname, "css/syntax.css");
    const from = candidates
      .map((rel) => path.join(__dirname, rel))
      .find((p) => fs.existsSync(p));
    if (from) {
      const css = fs.readFileSync(from, "utf8");
      fs.writeFileSync(
        dest,
        `/* Syntax highlighting (highlight.js light theme; Pandoc tango substitute) */\n${css}`,
      );
    } else {
      fs.writeFileSync(
        dest,
        "pre code.hljs { display: block; overflow-x: auto; padding: 1em; background: #f8f8f8; }\n",
      );
    }
  });

  eleventyConfig.addPassthroughCopy("css/syntax.css");

  return {
    dir: {
      input: ".",
      includes: "templates",
      layouts: "templates",
      data: "_data",
      output: "_site",
    },
    htmlTemplateEngine: "njk",
    markdownTemplateEngine: false,
    dataTemplateEngine: false,
    templateFormats: ["md", "njk", "html", "liquid"],
  };
}

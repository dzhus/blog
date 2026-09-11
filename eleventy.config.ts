import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import type { CollectionItem, UserConfig } from "@11ty/eleventy";
import pluginRss from "@11ty/eleventy-plugin-rss";
import MarkdownIt from "markdown-it";
import hljs from "highlight.js";
import { extractLeadingH1 } from "./src/title.ts";
import {
  defaultCreator,
  defaultTitle,
  gravatar,
  rootUrl,
  thisYear,
} from "./src/siteConstants.ts";
import { collectTags, renderTagCloud } from "./src/tags.ts";
import { buildTrips, readTripsManifest } from "./src/trips/build.ts";
import type { TripManifest, TripPhoto } from "./src/trips/types.ts";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

export type TripPhotoPage = {
  trip: TripManifest;
  photo: TripPhoto;
  prev: TripPhoto | null;
  next: TripPhoto | null;
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

  eleventyConfig.addPassthroughCopy("images");
  eleventyConfig.addPassthroughCopy("css/default.css");
  eleventyConfig.addPassthroughCopy("css/trips.css");

  eleventyConfig.addGlobalData("rootUrl", rootUrl);
  eleventyConfig.addGlobalData("gravatar", gravatar);
  eleventyConfig.addGlobalData("thisYear", thisYear());
  eleventyConfig.addGlobalData("creator", defaultCreator);
  eleventyConfig.addGlobalData("siteTitle", defaultTitle);
  eleventyConfig.addGlobalData("lang", "ru");
  eleventyConfig.addGlobalData("langPrefix", "");

  eleventyConfig.on("eleventy.before", async () => {
    await buildTrips({ projectRoot: __dirname });
  });

  eleventyConfig.addGlobalData("trips", () => {
    return readTripsManifest(__dirname).trips;
  });

  eleventyConfig.addGlobalData("tripPhotos", (): TripPhotoPage[] => {
    const trips = readTripsManifest(__dirname).trips;
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
  });

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

  const md: MarkdownIt = new MarkdownIt({
    html: true,
    linkify: true,
    typographer: false,
    highlight(str: string, lang: string): string {
      if (lang && hljs.getLanguage(lang)) {
        try {
          return (
            '<pre class="hljs"><code>' +
            hljs.highlight(str, { language: lang, ignoreIllegals: true }).value +
            "</code></pre>"
          );
        } catch {
          // fall through
        }
      }
      return (
        '<pre class="hljs"><code>' + md.utils.escapeHtml(str) + "</code></pre>"
      );
    },
  });
  eleventyConfig.setLibrary("md", md);

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

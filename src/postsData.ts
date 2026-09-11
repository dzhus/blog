import { readFileSync } from "node:fs";
import path from "node:path";
import matter from "gray-matter";
import {
  dateFromFilename,
  formatDisplayDate,
  formatIso8601,
  formatIsoDate,
  getGitAuthorDate,
} from "./gitDates.ts";
import { makeDescription, renderMarkdown } from "./markdown.ts";
import { computeNavigation } from "./navigation.ts";
import { localizedMeta } from "./siteConstants.ts";
import { tagHref } from "./tags.ts";
import { extractLeadingH1, resolveTitle } from "./title.ts";

function postBasename(inputPath: string): string {
  return path.basename(inputPath, path.extname(inputPath));
}

function readMarkdownBody(inputPath: string): string {
  const raw = readFileSync(inputPath, "utf8");
  return matter(raw).content;
}

function nav(data: any) {
  const collection =
    data.lang === "en" ? data.collections.postsEn : data.collections.postsRu;
  if (!collection) {
    return { currentPageNum: 0, numPages: 0 };
  }
  return computeNavigation(data.page.url, collection);
}

export default {
  layout: "post.njk",
  eleventyComputed: {
    lang(data: any) {
      return data.lang === "en" ? "en" : "ru";
    },
    langPrefix(data: any) {
      return data.lang === "en" ? "en/" : "";
    },
    creator(data: any) {
      return localizedMeta(data.lang).creator;
    },
    siteTitle(data: any) {
      return localizedMeta(data.lang).siteTitle;
    },
    permalink(data: any) {
      const slug = postBasename(data.page.inputPath);
      return data.lang === "en"
        ? `/en/posts/${slug}.html`
        : `/posts/${slug}.html`;
    },
    title(data: any) {
      const raw = readFileSync(data.page.inputPath, "utf8");
      const parsed = matter(raw);
      const { title: h1 } = extractLeadingH1(parsed.content, { strip: true });
      const yamlTitle =
        typeof parsed.data.title === "string" ? parsed.data.title : undefined;
      return resolveTitle(h1, yamlTitle, postBasename(data.page.inputPath));
    },
    date(data: any) {
      return (
        dateFromFilename(postBasename(data.page.inputPath)) ??
        getGitAuthorDate(data.page.inputPath)
      );
    },
    displayDate(data: any) {
      const d =
        dateFromFilename(postBasename(data.page.inputPath)) ??
        getGitAuthorDate(data.page.inputPath);
      return formatDisplayDate(d);
    },
    isoDate(data: any) {
      const d =
        dateFromFilename(postBasename(data.page.inputPath)) ??
        getGitAuthorDate(data.page.inputPath);
      return formatIsoDate(d);
    },
    modificationDate(data: any) {
      return formatIsoDate(getGitAuthorDate(data.page.inputPath));
    },
    updated(data: any) {
      return formatIso8601(getGitAuthorDate(data.page.inputPath));
    },
    path(data: any) {
      const input = data.page.inputPath.replace(/\\/g, "/");
      const marker = "/posts/";
      const idx = input.lastIndexOf(marker);
      return idx >= 0
        ? input.slice(idx + 1)
        : `posts/${postBasename(data.page.inputPath)}.md`;
    },
    description(data: any) {
      const body = readMarkdownBody(data.page.inputPath);
      const { html } = renderMarkdown(body, { stripLeadingH1: true });
      return makeDescription(html);
    },
    contentHtml(data: any) {
      const body = readMarkdownBody(data.page.inputPath);
      return renderMarkdown(body, { stripLeadingH1: true }).html;
    },
    tagList(data: any) {
      const tags = data.tags;
      if (!Array.isArray(tags)) return [];
      return tags
        .filter((t: unknown) => typeof t === "string")
        .map((name: string) => ({ name, href: tagHref(name) }));
    },
    previousPageUrl(data: any) {
      return nav(data).previousPageUrl;
    },
    nextPageUrl(data: any) {
      return nav(data).nextPageUrl;
    },
    currentPageNum(data: any) {
      return nav(data).currentPageNum;
    },
    numPages(data: any) {
      return nav(data).numPages;
    },
  },
};

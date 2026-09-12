import { readFileSync } from "node:fs";
import matter from "gray-matter";
import { localizedMeta } from "./siteConstants.ts";
import { extractLeadingH1, resolveTitle } from "./title.ts";

export default {
  layout: "single-page.njk",
  eleventyComputed: {
    lang(data: any) {
      return data.lang === "en" ? "en" : "ru";
    },
    permalink(data: any) {
      const input = data.page.inputPath.replace(/\\/g, "/");
      const rel = input.split("/pages/").pop() ?? `${data.page.fileSlug}.md`;
      return "/" + rel.replace(/\.md$/, ".html");
    },
    title(data: any) {
      const raw = readFileSync(data.page.inputPath, "utf8");
      const parsed = matter(raw);
      const { title: h1 } = extractLeadingH1(parsed.content, { strip: false });
      const yamlTitle =
        typeof parsed.data.title === "string" ? parsed.data.title : undefined;
      return resolveTitle(h1, yamlTitle, data.page.fileSlug);
    },
  },
};

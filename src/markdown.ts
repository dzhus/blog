import MarkdownIt from "markdown-it";
import hljs from "highlight.js";
import { extractLeadingH1 } from "./title.ts";

export const md: MarkdownIt = new MarkdownIt({
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

export type RenderResult = {
  titleFromH1?: string;
  html: string;
};

/** Render markdown; for posts, strip leading H1 from the body. */
export function renderMarkdown(
  source: string,
  options: { stripLeadingH1?: boolean } = {},
): RenderResult {
  const strip = options.stripLeadingH1 ?? false;
  const { title, body } = extractLeadingH1(source, { strip });
  return {
    titleFromH1: title,
    html: md.render(body),
  };
}

export function stripTags(html: string): string {
  return html.replace(/<[^>]*>/g, "").replace(/\s+/g, " ").trim();
}

export function makeDescription(html: string, maxLen = 190): string {
  const text = stripTags(html);
  if (text.length <= maxLen) return text + "…";
  return text.slice(0, maxLen) + "…";
}

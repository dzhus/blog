/** Extract a leading ATX H1 from markdown; optionally strip it from the body. */
export function extractLeadingH1(
  markdown: string,
  options: { strip?: boolean } = { strip: true },
): { title: string | undefined; body: string } {
  const match = markdown.match(/^(\s*)#\s+([^\n]+)\n?/);
  if (!match) {
    return { title: undefined, body: markdown };
  }
  const title = match[2].trim();
  if (options.strip === false) {
    return { title, body: markdown };
  }
  const body = markdown.slice(match[0].length).replace(/^\n/, "");
  return { title, body };
}

/** Prefer leading H1 title over YAML title when both exist. */
export function resolveTitle(
  h1Title: string | undefined,
  yamlTitle: unknown,
  fallback: string,
): string {
  const yaml =
    typeof yamlTitle === "string"
      ? yamlTitle
      : yamlTitle != null && typeof yamlTitle !== "function"
        ? String(yamlTitle)
        : undefined;
  return (h1Title ?? yaml ?? fallback).trim();
}

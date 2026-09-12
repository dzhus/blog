export function tagHref(tag: string): string {
  return `/tag/${encodeURIComponent(tag)}.html`;
}

export function collectTags<T extends { data: { tags?: string[] } }>(
  posts: T[],
): Map<string, T[]> {
  const map = new Map<string, T[]>();
  for (const post of posts) {
    const tags = post.data.tags;
    if (!Array.isArray(tags)) continue;
    for (const tag of tags) {
      if (typeof tag !== "string" || tag === "posts") continue;
      const list = map.get(tag) ?? [];
      list.push(post);
      map.set(tag, list);
    }
  }
  return map;
}

/** Hakyll-like tag cloud: font size between minSize% and maxSize%. */
export function renderTagCloud(
  tagCounts: Array<{ tag: string; count: number }>,
  minSize = 100,
  maxSize = 150,
): string {
  if (tagCounts.length === 0) return "";
  const counts = tagCounts.map((t) => t.count);
  const min = Math.min(...counts);
  const max = Math.max(...counts);
  const sorted = [...tagCounts].sort((a, b) =>
    a.tag.localeCompare(b.tag, "en"),
  );
  return sorted
    .map(({ tag, count }) => {
      const size =
        max === min
          ? (minSize + maxSize) / 2
          : minSize + ((count - min) / (max - min)) * (maxSize - minSize);
      const href = tagHref(tag);
      return `<a style="font-size: ${Math.round(size)}%" href="${href}">${escapeHtml(tag)}</a>`;
    })
    .join(" ");
}

function escapeHtml(s: string): string {
  return s
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

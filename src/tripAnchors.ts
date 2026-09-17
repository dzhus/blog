/** Stable fragment ids for trip/photo mentions in post HTML. */

export function photoAnchorId(stem: string): string {
  return `trip-photo-${stem}`;
}

export function tripAnchorId(slug: string): string {
  return `trip-${slug}`;
}

const PHOTO_HREF_RE =
  /^\/?(?:en\/)?trips\/([^/?#]+)\/photo\/([^/?#]+)\.html\/?$/i;
const TRIP_HREF_RE =
  /^\/?(?:en\/)?trips\/([^/?#]+)(?:\/(?:index\.html)?\/?)?$/i;
const DISPLAY_SRC_RE =
  /^\/?(?:en\/)?trips\/([^/?#]+)\/display\/([^/?#]+)\.(?:jpe?g|png|webp|gif)$/i;
const LOOSE_PHOTO_HREF_RE =
  /^\/?(?:en\/)?photos\/([^/?#]+)\.html\/?$/i;
const LOOSE_DISPLAY_SRC_RE =
  /^\/?(?:en\/)?photos\/display\/([^/?#]+)\.(?:jpe?g|png|webp|gif)$/i;

type TripHrefTarget =
  | { kind: "photo"; slug: string; stem: string }
  | { kind: "trip"; slug: string }
  | { kind: "loose"; stem: string };

function parseTripHref(href: string): TripHrefTarget | null {
  const photo = href.match(PHOTO_HREF_RE);
  if (photo?.[1] && photo[2]) {
    return { kind: "photo", slug: photo[1], stem: photo[2] };
  }
  const loose = href.match(LOOSE_PHOTO_HREF_RE);
  if (loose?.[1] && loose[1] !== "index") {
    return { kind: "loose", stem: loose[1] };
  }
  const trip = href.match(TRIP_HREF_RE);
  if (trip?.[1] && trip[1] !== "index.html") {
    return { kind: "trip", slug: trip[1] };
  }
  return null;
}

function parseDisplaySrc(src: string): { stem: string } | null {
  const trip = src.match(DISPLAY_SRC_RE);
  if (trip?.[2]) return { stem: trip[2] };
  const loose = src.match(LOOSE_DISPLAY_SRC_RE);
  if (loose?.[1]) return { stem: loose[1] };
  return null;
}

function getAttr(attrs: string, name: string): string | null {
  const re = new RegExp(
    `\\s${name}\\s*=\\s*(?:"([^"]*)"|'([^']*)'|([^\\s>]+))`,
    "i",
  );
  const m = attrs.match(re);
  if (!m) return null;
  return m[1] ?? m[2] ?? m[3] ?? null;
}

function collectExistingIds(html: string): Set<string> {
  const used = new Set<string>();
  const re = /\sid\s*=\s*(?:"([^"]*)"|'([^']*)'|([^\s>]+))/gi;
  let m: RegExpExecArray | null;
  while ((m = re.exec(html)) !== null) {
    const id = m[1] ?? m[2] ?? m[3];
    if (id) used.add(id);
  }
  return used;
}

/**
 * Inject first-occurrence `id`s on trip/photo `<a href>` and display `<img src>`.
 * Idempotent: skips tags that already have an id, and ids already used in the document.
 */
export function addTripAnchorIds(html: string): string {
  const used = collectExistingIds(html);

  return html.replace(/<(a|img)\b([^>]*)>/gi, (full, tagName: string, attrs: string) => {
    if (/\sid\s*=/i.test(attrs)) return full;

    let id: string | null = null;
    if (/^a$/i.test(tagName)) {
      const href = getAttr(attrs, "href");
      if (href) {
        const target = parseTripHref(href);
        if (target?.kind === "photo" || target?.kind === "loose") {
          id = photoAnchorId(target.stem);
        } else if (target?.kind === "trip") {
          id = tripAnchorId(target.slug);
        }
      }
    } else {
      const src = getAttr(attrs, "src");
      if (src) {
        const target = parseDisplaySrc(src);
        if (target) id = photoAnchorId(target.stem);
      }
    }

    if (!id || used.has(id)) return full;
    used.add(id);
    return `<${tagName} id="${id}"${attrs}>`;
  });
}

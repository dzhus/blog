# Dmitry's journal

Personal static blog built with [Eleventy](https://www.11ty.dev/) (TypeScript).
It replaces the earlier Hakyll (and before that, Django) engines.

## Features

- Home page(s)
- Titles from leading Markdown `#` headings (preferred over YAML `title`)
- Responsive layout (`css/default.css`)
- `lang: ..` metadata support
- Same-language previous/next links and neighbour footer
- Tag pages and tag cloud
- Atom feeds
- Sitemap for posts (`/sitemap.xml`)
- Syntax highlighting (highlight.js) with horizontal scroll for code blocks
- `modificationDate` / `updated` from each file’s latest Git author date
- Trip photo galleries with GPX map overlays (`trips/`)

## Develop

Requires Node.js 22+.

```bash
npm install
npm run build    # writes to _site/
npm start        # build + local server
```

The first trip build downloads OpenTopoMap tiles (with contour lines; cached under `_cache/tiles/opentopomap/`), converts them to greyscale, and copies them into `_site/trips/<slug>/map/tiles/{z}/{x}/{y}.png`. Later builds reuse that cache. Rendered pages load those local tiles via Leaflet (EPSG:3857)—no tile CDN at runtime.

## Content layout

| Path | Role |
|------|------|
| `posts/*.md` | Blog posts (`YYYY-MM-DD-slug.md`) |
| `pages/` | Static pages (`about`, `en/about`, `error`) |
| `trips/<YYYY-MM-Name>/` | Trip sources: photos (JPEG/PNG) + `.gpx` tracks |
| `templates/` | Nunjucks layouts |
| `css/`, `images/` | Assets |
| `eleventy.config.ts` | Site configuration |
| `src/` | Build helpers (dates, titles, tags, markdown, trips) |
| `_cache/trips/`, `_cache/tiles/` | Generated image/tile cache (gitignored) |

### Trips

Each subdirectory of `trips/` is one trip and must include at least one `.gpx` file. Photos are ordered by EXIF `DateTimeOriginal` and must include GPS coordinates (build fails if either is missing).

Optional metadata file in the trip folder (`trip.yml`, `trip.yaml`, `metadata.yml`, or `metadata.yaml`):

```yaml
name: Алтай
thumbnail: 2026-08-08-07-33.jpg
```

`name` overrides the display title derived from the folder name (URL slug stays the folder name). `thumbnail` is an optional photo filename in the trip folder used as the cover on `/trips/` (defaults to the earliest photo by EXIF date). Trip `from` / `to` dates are inferred from min/max `<time>` values on GPX track points (required; build fails if a trip has no `.gpx` or no usable point times).

Build output:

- `/trips/` — index
- `/trips/<slug>/` — map (left) + thumbnail grid (right), GPX download links
- `/trips/<slug>/photo/<id>.html` — single photo, prev/next, original download

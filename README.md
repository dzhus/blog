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

## Develop

Requires Node.js 22+.

```bash
npm install
npm run build    # writes to _site/
npm start        # build + local server
```

## Content layout

| Path | Role |
|------|------|
| `posts/*.md` | Blog posts (`YYYY-MM-DD-slug.md`) |
| `pages/` | Static pages (`about`, `en/about`, `error`) |
| `templates/` | Nunjucks layouts |
| `css/`, `images/` | Assets |
| `eleventy.config.ts` | Site configuration |
| `src/` | Build helpers (dates, titles, tags, markdown) |

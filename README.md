This repo contains [Hakyll][]-based personal blog of Dmitry Dzhus. It
replaces the old Django-based blog engine which was used in 2006-2012.

## Features

- [x] Home page

- [x] Extract title from the first `<h1>` heading in the file

- [x] Mobile view with responsive design

- [x] Multi-language support

    Language-specific sections: post page, feeds, post list, tags,
    pagination.

    Each entry can hava a language specified via `lang` metadata
    field. There are feeds in both languages. Template must depend on
    entry language. Next/previous page links must lead to entries in
    the same language.

- [x] Atom feeds

- [x] Sitemap for posts

- [x] Source code highlighting and scrolling

- [x] Next/previous page link

- [x] `modificationTimeField` for each entry is populated from the
      author date of its latest Git revision

## Known issues

- [ ] Presumably due to pagination/tags code, all pages are rebuilt
      after even a single post is modified.

## TOCs

It's possible to get Pandoc to include table of contents in the HTML
generated for a Markdown post with something like

    pandocCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions{writerTemplate=Just "$toc$ $body$", writerTOCDepth = 4, writerTableOfContents = True}

Ideally we'd want to place TOC in the post manually (for example by
placing `<!-- toc -->` comment at the desired position).

Perhaps it's best to leave it to the [client][client-toc].

[client-toc]: https://chrome.google.com/webstore/detail/smart-toc/lifgeihcfpkmmlfjbailfpfhbahhibba
[hakyll]: https://jaspervdj.be/hakyll/index.html

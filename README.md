This repo contains [Hakyll][]-based personal blog of Dmitry Dzhus. This
replaces the old Django-based blog engine which was used in 2006-2012.

## A note on Semantic Web technologies

The last big changes to the old engine were made somewhere in 2009. In
2017, the state of the Semantic Web tech adoption has changed
(positively) compared to 2010s:

- HTML5 is mainstream

- Microformats, RDFa were «superseded» by [microformats2][]

## TOCs

It's possible to get Pandoc to include table of contents in the HTML
generated for a Markdown post with something like

    pandocCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions{writerTemplate=Just "$toc$ $body$", writerTOCDepth = 4, writerTableOfContents = True}

Ideally we'd want to place TOC in the post manually (for example by
placing `<!-- toc -->` comment at the desired position).

Perhaps it's best to leave it to the [client][client-toc].

## TODO

- [ ] Atom feed

- [ ] article list page (w/ pagination)

- [ ] tagged article list page (w/ pagination)

- [ ] list of tags on article page

- [ ] next/previous page link?

[hakyll]: https://jaspervdj.be/hakyll/index.html
[microformats2]: http://microformats.org/wiki/microformats2
[client-toc]: https://chrome.google.com/webstore/detail/smart-toc/lifgeihcfpkmmlfjbailfpfhbahhibba

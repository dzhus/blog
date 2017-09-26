This repo contains [Hakyll][]-based personal blog of Dmitry Dzhus. This
replaces the old Django-based blog engine which was used in 2006-2012.

## A note on Semantic Web technologies

The last big changes to the old engine were made somewhere in 2009. In
2017, the state of the Semantic Web tech adoption has changed
(positively) compared to 2010s:

- HTML5 is mainstream

- Microformats, RDFa were «superseded» by [microformats2][]

- Some `<link>` `rel` values were [dropped][rel-drop]

## TOCs

It's possible to get Pandoc to include table of contents in the HTML
generated for a Markdown post with something like

    pandocCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions{writerTemplate=Just "$toc$ $body$", writerTOCDepth = 4, writerTableOfContents = True}

Ideally we'd want to place TOC in the post manually (for example by
placing `<!-- toc -->` comment at the desired position).

Perhaps it's best to leave it to the [client][client-toc].

## Features

- [x] Atom feed

- [x] article list page (w/ pagination)

- [ ] tagged article list page (w/ pagination)

- [ ] list of tags on article page

- [x] alternate links

    For some reason, trying to do `loadAll pat .&&. hasVersion "raw"`
    in the body of `paginateRules` results in no items loaded, so
    we're using `load` instead (we know the identifier anyways).

- [x] next/previous page link?

    This one seems more difficult than it should be:

    1. Pagination. `buildPaginateWith` has no means to specify sorting
       order. It's unclear what `pattern` argument in `paginateRules`
       refers to.

       Somehow I got this to work:

       - Make page identifiers equal to file identifiers

       - Using paginateRules forces files with correct names to be
         created (with a single identifier per page). The question is
         how `pandocCompiler` picks the right source file if the
         enclosing `Rules` are `create`, not `match`?

    2. `sortChronologically` + `getMatches`: this operates on
       Identifiers only, but it seems there's no way to go from an
       Identifier to its URL outside of compiler context for the Item
       which corresponds to that identifier. It means that mutual
       (previous/next) links between consecutive items will result in
       circular dependencies.


[client-toc]: https://chrome.google.com/webstore/detail/smart-toc/lifgeihcfpkmmlfjbailfpfhbahhibba
[hakyll]: https://jaspervdj.be/hakyll/index.html
[microformats2]: http://microformats.org/wiki/microformats2
[rel-drop]: http://lists.w3.org/Archives/Public/public-html/2011Feb/att-0481/issue-118-decision.html

---
tags:
- Gentoo
- HOWTO
- TeX
- заметки
title:  TeX Live на Gentoo
---

Таки установил [TeX Live][] 2007 на Gentoo. До этого не получалось.

Рецепт прост:

1.  [Ебилд][] из №168177 в Багзилле Gentoo со всеми наложенными на него
    патчами оттуда положить в локальный оверлей Portage (например, в
    `/usr/local/portage`). Разумеется, в соответствующую категорию,
    получится типа
    `/usr/local/portage/app-text/texlive/texlive-2007.ebuild`.

2.  Патчи для сборки TeX Live из Багзиллы же закинуть в директорию
    `files/2007` ебилда, переименовав их в соответствии с именнем ебилда
    (добавить `texlive-2007` к имени файлов). Это:
    `texlive-2007-gentoo-texmf.patch`,
    `texlive-2007-mpware-libtool.patch`, `texlive-2007-mv-texmf.patch`,
    `texlive-2007-use-system-libtool.patch`, `xpdf-3.02pl1.patch`;
    `texlive-2007-gentoo-texmf-site.patch` взять из ебилда для TeX
    Live 2005.

3.  В директорию `files` (без номера версии) ебилда положить скрипт
    `texmf-update` из [оверлея Aballier][] (в том оверлее тоже лежат
    ебилды для модульной сборки TeX Live; не пробовал). `texmf-update`
    из ебилда для TeX Live 2005 не пойдёт. Без этого скрипта после
    инсталляции не получится, к примеру, использовать LaTeX и т. п.

4.  `# emerge =texlive-2007`

TeX Live — огромный дистрибутив [TeX][], который включает WEB, TeX,
[METAFONT][], [MetaPost][], [BibTeX][], [LaTeX2ε][] (и кучу пакетов к
нему). Меня радует, что там есть хороший комплект кошернейших шрифтов —
[cm-super][] (там, например, есть русский шрифт из «[Конкретной
математики][]» и математический шрифт «AMS Euler» оттуда же).

TeX Live доступен в том числе и на [Live DVD][].

  [TeX Live]: https://web.archive.org/web/20071022153712/http://www.tug.org/texlive/
  [Ебилд]: https://web.archive.org/web/20071022153712/http://bugs.gentoo.org/show_bug.cgi?id=168177
  [оверлея Aballier]: https://web.archive.org/web/20071022153712/http://overlays.gentoo.org/dev/aballier/browser/texlive-overlay/app-text/texlive-core/files/texmf-update
  [TeX]: https://web.archive.org/web/20071022153712/http://ru.wikipedia.org/wiki/TeX
  [METAFONT]: https://web.archive.org/web/20071022153712/http://ru.wikipedia.org/wiki/METAFONT
  [MetaPost]: https://web.archive.org/web/20071022153712/http://ru.wikipedia.org/wiki/MetaPost
  [BibTeX]: https://web.archive.org/web/20071022153712/http://ru.wikipedia.org/wiki/BibTeX
  [LaTeX2ε]: https://web.archive.org/web/20071022153712/http://ru.wikipedia.org/wiki/LaTeX
  [cm-super]: https://web.archive.org/web/20071022153712/http://tug.ctan.org/cgi-bin/ctanPackageInformation.py?id=cm-super
  [Конкретной математики]: https://web.archive.org/web/20071022153712/http://ru.wikipedia.org/wiki/Конкретная_математика
  [Live DVD]: https://web.archive.org/web/20071022153712/http://www.tug.org/texlive/acquire.html
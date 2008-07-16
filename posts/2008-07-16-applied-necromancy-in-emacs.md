---
tags:
- RuNIX
title: Прикладная некромантия в Emacs
---

Пользуюсь [Gentoo][]. Частенько приходится писать [ебилды][] для
пакетов, которых нет в [Portage][]. Все ебилды похожи, пишу я их в
Емаксе. В таком случае определённо стоит призвать на помощь скелетов.

Скелеты, описанные в доке «[Autotype][]» — это, в общем-то, обычные
шаблоны на стероидах. Помимо обычной вставки текста, скелет может
вежливо спросить через минибуфер, что именно вставить, через минибуфер,
а то и сплясать как нам захочется, выполнив произвольную конструкцию на
Emacs Lisp. Скелет также может обернуть уже написанное в буфере. То
есть, скелет — интерактивный динамический шаблон. Можно сделать так,
чтобы скелет вставлялся в буфер, когда создаётся новый файл
определённого типа (который определяется по расширению или текущему
режиму).

## Пример скелета

Полностью несложное дело конструирования нежити раскрыто в разделе
«[Skeleton Language][]», я покажу простой пример.

### Синтаксис

Скелет представляет собой обычный список, каждый элемент трактуется
определённым образом. Например, строковые элементы тупо вставляются в
буфер, символ `>` делает отступ текущий строчки буфера согласно режиму и
т. д.

Первый элемент имеет особое значение для скелетов, когда-либо задающих
вопросы — если это строка, то она используется как приглашение при
вводе, при этом вопрос задаётся не сразу, а тогда, когда в скелете
встретится специальный элемент `str` (вместо которого и вставится то,
что мы ответим скелету). Брутальные же молчаливые скелеты имеют `nil` в
качестве первого элемента.

Частью скелета в том числе может быть и другой скелет :-)

### Постановка задачи

Я хотел бы иметь скелета, который вставлял бы в начало буфера
стандартный заголовок ебилда (`/usr/portage/header.txt`), спрашивал
меня, как заполнить `DESCRIPTION`, `HOMEPAGE`, `SRC_URI`, `LICENSE` (при
этом я хочу иметь мудрый комплишен по списку лицензий), `IUSE`, узнавал
у меня поочерёдно элементы `DEPEND`, гламурно форматируя их список, а
также вставлял простые предопределённые заготовки `src_unpack()`,
`src_compile()`, `src_install()` и ещё некоторые конструкции.

### Решение

Функция `define-skeleton` определяет команду для вставки скелета. Первым
аргументом идёт имя скелета, потом дока, а потом собственно содержимое
скелетика.

    (define-skeleton ebuild-skeleton
      "Insert new ebuild skeleton.

    Ask for package description, homepage, source URL, license, USE
    flags and dependencies interactively."
      nil
      '(insert-file-contents "/usr/portage/header.txt")
      '(goto-char (point-max))
      "DESCRIPTION=\"" (read-string "Enter description: ") "\"" \n
      "HOMEPAGE=\"" (read-string "Enter homepage URL: " "http://") "\"" \n
      "SRC_URI=\"" (read-string "Enter source code URL: " "http://") "\"" \n
      "LICENSE=\"" (ebuild-read-license "Enter package license: ") "\"" \n \n
      "SLOT=\"0\"" \n
      "KEYWORDS=\"~x86\"" \n
      "IUSE=\"" (read-string "Enter USE flags: ") "\"" \n \n
      "DEPEND=\"" ("Enter dependency atom: " str & (nil ?\n "   "))
                  & (if skeleton-untabify -5 -2) "\"" ?\n
      "RDEPEND=\"${DEPEND}\"" \n \n
      "inherit eutils" \n \n
      "src_unpack() {" \n _ "unpack ${A}" > _ ?\n "}" \n \n
      "src_compile() {" \n "emake || die \"emake failed\"" > ?\n "}" \n \n
      "src_install() {" \n "emake DESTDIR=\"${D}\" install || die \"Install failed\"" > ?\n "}" \n \n)

Голова скелета здесь `nil` — сам он не будет задавать вопросов.

#### По косточкам

Поехали.

    '(insert-file-contents "/usr/portage/header.txt")
    '(goto-char (point-max))

Когда встречается цитированная конструкция, она выполняется только ради
своего побочного эффекта — вставить хидер из файла, переместиться в
конец буфера.

    "DESCRIPTION=\"" (read-string "Enter description: ") "\"" \n
    "HOMEPAGE=\"" (read-string "Enter homepage URL: " "http://") "\"" \n
    "SRC_URI=\"" (read-string "Enter source code URL: " "http://") "\"" \n
    "LICENSE=\"" (ebuild-read-license "Enter package license: ") "\"" \n \n
    "SLOT=\"0\"" \n
    "KEYWORDS=\"~x86\"" \n
    "IUSE=\"" (read-string "Enter USE flags: ") "\"" \n \n

Строки просто вставляются (кавычки `"` нужно экранировать). Когда
встречается просто выражение (как здесь `read-string`), его результат (в
данном случае — считанная из минибуфера строка) вновь интерпретируется
как часть скелета. Тело функции `ebuild-read-license`, которая считывает
имя лицензии, покажу попозже. Элементы `\n` вставляют перевод строки с
выравниванием по предыдущей.

    "DEPEND=\"" ("Enter dependency atom (RET to finish): " str & (nil ?\n " "))
                & (if skeleton-untabify -5 -2) "\"" ?\n

После вставки строки `DEPEND="` встречается подскелет! Подскелеты хитры,
и вставляются до тех пор, пока пользователь что-нибудь им говорит. В
данном случае первый элемент субскелета используется как строка
приглашения для ввода очередной зависимости. Появление `str` собственно
заставляет скелет задать вопрос. `&` работает как логическое «И» — если
предыдущий элемент перемещал текущую точку в буфере, вставляется
очередной субскелет — `(nil ?\n " ")`. В нём используется `?\n`, который
вставляет перевод строки без выравнивания, и обычная табуляция
(приходится вручную вставлять табуляцию, потому что сейчас в
`ebuild-mode` нет правил расставления отступов в `DEPEND`). Так
продолжается до тех пор, пока скелет получает непустые ответы на свои
вопросы.

Если подскелет вызвал вставку хотя бы одной зависимости, на конце
получится лишний перевод строки с табом, которые сжираются при помощи
`(if skeleton-untabify -5 -2)` — элементы вида `-N` удаляют N предыдущих
символов.

    "RDEPEND=\"${DEPEND}\"" \n \n
    "inherit eutils" \n \n

Тупо вставка строк (`RDEPEND`, конечно, в ебилдах не всегда такой, но
его достоверное определение может потребовать установки пакета, а там
всякие `ldd(1)`, `yolk(1)` и пр. помогают).

    "src_unpack() {" \n _ "unpack ${A}" > _ ?\n "}" \n \n     
    "src_compile() {" \n "emake || die \"emake failed\"" > ?\n "}" \n \n
    "src_install() {" \n "emake DESTDIR=\"${D}\" install || die \"Install failed\"" > ?\n "}" \n \n

Опять тупо вставка строк, три заготовки функций. Элемент `>` делает
выравнивание текущий строки согласно режиму (тела функций в
`gentoo-mode` смещаются нормально, на таб), а `_` указывает на положение
точки после того, как скелет закончит работу (скорее всего доводку
ебилда придётся начать с `src_unpack`, поэтому туда).

#### ebuild-read-license

Все лицензии, доступные в Portage, лежат в `/usr/portage/licenses`,
поэтому удобно считывать имя лицензии с комплишеном по доступным.

    (defun ebuild-read-license (prompt)
      "Read a license name from the minibuffer.

    PROMPT is used as minibuffer prompt, input is completed it to one
    of licenses in Portage if possible."
      (completing-read prompt
       (file-name-all-completions "" "/usr/portage/licenses")
       nil
       nil
       "GPL-2"))

### Пример работы

После определения скелета можно, например, вызвать функцию
`ebuild-skeleton`, которая задаст нужные вопросы и выведет в буфер
что-то вроде этого:

    # Copyright 1999-2008 Gentoo Foundation
    # Distributed under the terms of the GNU General Public License v2
    # $Header: $

    DESCRIPTION="libfoobar is an ultimate solution for extensive foobaring"
    HOMEPAGE="http://libfoobar.org"
    SRC_URI="http://libfoobar.org/download/libfoobar-1.3.tgz"
    LICENSE="MIT"

    SLOT="0"
    KEYWORDS="~x86"
    IUSE="doc emacs vim"

    DEPEND="dev-libs/nettle
            emacs? ( virtual/emacs )
            vim? ( app-editors/vim )"
    RDEPEND="${DEPEND}"

    inherit eutils

    src_unpack() {
            unpack ${A}
    }

    src_compile() {
            emake || die "emake failed"
    }

    src_install() {
            emake DESTDIR="${D}" install || die "Install failed"
    }

## Customize

Средство «[auto-insert][]» поможет сделать так, чтобы созданная нежить
прибегала по первому свисту.

Функция `auto-insert` вставляет в текущий буфер некоторое содержимое
(скелета или простой текстовый шаблон) согласно режиму или маске файла.
Например, для `latex-mode` скелет спрашивает `\documentclass` и какие
пакеты с какими опциями подключить.

Сопоставление режима/маски файла и содержимого, включаемого по
`M-x auto-insert`, выполняется при помощи `define-auto-insert` (это
нужно написать куда-нибудь в `.emacs.el`):

    (define-auto-insert '("\\.ebuild$" "Ebuild") 'ebuild-skeleton)

Чтобы автоматическая вставка фрагментов выполнялась каждый раз, когда я
открываю новый файл с подходящим расширением, нужно включить и сохранить
опцию `auto-insert-mode`.

Собственно, это включит автовставку содержимого и для других типов
файлов (см. [доки][auto-insert]). Можно настроить переменную
`auto-insert-query`, чтобы вставка происходила с подтверждением (будет
задаваться вопрос типа «*Perform (Ebuild) auto-insertion? (y or n)*»)

  [Gentoo]: https://web.archive.org/web/20080716202321/http://sphinx.net.ru/blog/entry/hello-gentoo/
  [ебилды]: https://web.archive.org/web/20080716202321/http://ru.wikipedia.org/wiki/Ebuild
  [Portage]: https://web.archive.org/web/20080716202321/http://www.gentoo.org/doc/en/handbook/handbook-x86.xml?part=2&amp;chap=1
  [Autotype]: https://web.archive.org/web/20080716202321/http://www.gnu.org/software/emacs/manual/html_node/autotype/index.html
  [Skeleton Language]: https://web.archive.org/web/20080716202321/http://www.gnu.org/software/emacs/manual/html_node/autotype/Skeleton-Language.html
  [auto-insert]: https://web.archive.org/web/20080716202321/http://www.gnu.org/software/emacs/manual/html_node/autotype/Autoinserting.html
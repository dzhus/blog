---
tags:
- Emacs
- Free Software
- 'GSoC-2009'
- Google
- Lisp
- программирование
title: 'Немного мыслей о GDB/MI'
---

[GDB/MI][] выводит информацию в весьма структурированном виде. Например,
информация от фреймах, возвращаемая командой `-stack-list-frames`,
выглядит так:

    (gdb) 
    -stack-list-frames
    ^done,stack=[frame={level="0",addr="0x0804868f",func="hello",file="hello.c",fullname="/home/sphinx/projects/gsoc/hello.c",line="6"},frame={level="1",addr="0xb7faa900",func="start_thread",from="/lib/libpthread.so.0"}]

Выглядит хорошо! Как с этим работать?

[Читать далее][]

  [GDB/MI]: https://web.archive.org/web/20100116140710/http://sourceware.org/gdb/current/onlinedocs/gdb_26.html#SEC263
    "The GDB/MI Interface"
  [Читать далее]: /web/20100116140710/http://sphinx.net.ru:80/blog/entry/gdb-mi-parsing/
---
tags:
- Free Software
- 'GNU/Linux'
title: Bootchart
---

[Bootchart][] — утилита для профилирования процесса загрузки GNU/Linux.
Измеряет загрузку процессора, дисков и информацию о процессах. На выходе
получается несколько текстовых файлов, которые можно визуализировать и
получить вот такую клюкву™:

[![bootchart][1]][]

Bootchart запускается вместо `init` при старте системы, сохраняя
информацию в памяти, потом сбрасывая всю статистику на диск.

После

    # USE="java" emerge bootchart

в Gentoo для включения профилирования достаточно поправить
`/etc/conf.d/rc`:

    RC_USE_BOOTCHART="yes"

Или же, когда надо, в загрузчике к опциям ядра добавлять
`init=/sbin/bootchartd`.

Сборка с флагом `java` установит Java-приложение для рендеринга дерева
(не обязательно).

После установки можно поправить `/etc/bootchartd.conf`, например,
прописать там

    AUTO_RENDER="yes"

    AUTO_RENDER_FORMAT="png"

    AUTO_RENDER_DIR="/var/log"

Чтобы после загрузки системы картинка с деревом процессов помещалась в
`/var/log/bootchart.png`.

По умолчанию файлы со статистикой пакуются в `/var/log/bootchart.tgz`.
Сгенерировать изображение из архива можно на [сайте Bootchart][] или
командой `bootchart /var/log/bootchart.tgz`.

  [Bootchart]: http://www.bootchart.org/
  [1]: http://farm3.static.flickr.com/2118/2170860519_5ee91fae62.jpg
  {width="500" height="497"}
  [![bootchart][1]]: http://www.flickr.com/photos/nothingpersonal/2170860519/
    "bootchart by Dmitry Dzhus, on Flickr"
  [сайте Bootchart]: http://www.bootchart.org/download.html

---
title: 'emacs-shellfm'
---

Мне нравится [Last.fm][]. Это один из немногих по-настоящему полезных
вебдванольных сервисов.

[Официальный плеер][] Last.fm хоть и свободен (GNU GPL), но уныл почти
что до бесконечности. То есть, он значителен, гламурен и им практически
нельзя управлять *не* напрямую. В действительности, официальный плеер
Last.fm сделан из говна.

Есть гномоальтернатива — [last-exit][]. Это более лёгкое решение,
использующее для интерфейса Gtk+ и C\# в качестве языка реализации.
last-exit поддерживает [D-Bus][], поэтому можно рулить им *издалека*, то
есть из моего православного GNU Emacs (в начале декабря 2007 в Emacs
была добавлена поддержка D-Bus).

Есть возможность слушать Last.fm при помощи [EMMS][]. Вроде ничётак, но
мне, например, фишек и не хватает. Нельзя вешать теги, комплишенов
няшных нет совсем.

# Shell.fm

Другим кошерным решением, однако, является консольный плеер
[Shell.FM][].

    # USE="ao" emerge shell-fm

Перед его использованием нужно записать свои данные в
`~/.shell-fm/shell-fm.rc`:

    username = SphinxTheGeek
    password = c@pt@!in_c@lculuzz

Программа управляется интерактивным командным интерфейсом или
(ограничено) удалённо через сокет. Его работа ориентирована на URL вида
`lastfm://`. Нельзя сказать «играй тег nu-jazz», нужно именно ввести
`lastfm://globaltags/nu-jazz`. Это не очень удобно.

Поддерживается весь комплекс фич протокола Last.fm. Приятным дополнением
является возможность поставить проигрывание на паузу (этого нет даже в
официальном клиенте).

# emacs-shellfm

Не очень удобно использовать Shell.FM постоянно из командной строки. В
итоге я написал небольшой компактненький интерфейс для управления
Shell.fm прямо из Emacs. Тексты и описание лежат по адресу
[github.com/dzhus/emacs-shellfm/][].

[![emacs-shellfm][]][]

Некоторые фичи:

-   глобальный элемент меню «Shell.FM» даёт обзор всего интерфейса.
    Поддерживаются почти все операции, которые предлагает консольная
    программа `shell-fm(1)`.

-   при выборе станции для прослушивания выполняется автодополнение по
    списку самых популярных тегов, исполнителей; дополняются теги при
    пометке треков; дополняются имена друзей при отправке рекоммендаций.

-   почти всё остальное, что поддерживает консольный Shell.FM

Установка описана в файлике `PROJECT` (вкратце, требуется прописать
загрузку `shellfm.el` в файл инициализации Emacs и обязательно
просмотреть всё в группе настроек `shellfm`).

`emacs-shellfm` — очень маленькое и компактное решение. Когда Tassilo
Horn допилит полноценный Last.fm-клиент для EMMS, я вновь попробую
перейти на него.

  [Last.fm]: http://dzhus.org/posts/2006-03-05-what-is-lastfm.html
  [Официальный плеер]: http://www.lastfm.ru/download/
  [last-exit]: http://www.lastexit-player.org/index.php/Main_Page
  [D-Bus]: http://www.freedesktop.org/wiki/Software/dbus
  [EMMS]: http://www.gnu.org/software/emms
  [Shell.FM]: http://nex.scrapping.cc/shell-fm/
  [github.com/dzhus/emacs-shellfm/]: http://github.com/dzhus/emacs-shellfm/
  [emacs-shellfm]: http://farm4.static.flickr.com/3171/2296244385_d4104e9846.jpg

  [![emacs-shellfm][]]: http://www.flickr.com/photos/nothingpersonal/2296244385/
    "emacs-shellfm by Dmitry Dzhus, on Flickr"

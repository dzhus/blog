---
tags:
- Emacs
- Free Software
- HOWTO
- Python
- музыка
- статьи
title: ' Emacs как медиаплеер: Bongo'
---

[Emacs][] продолжает з*о*хавывать мой и без того усталый, воспалённый и
измученный этой планетой мозг.

Недавно я решил поюзать медиаплеер для Emacs - Bongo. Ещё смотрел
[EMMS][], не пропёрло; хотя Bongo попроще EMMS будет.

## Сказ о Bongo

### Общее

[Bongo][] (название придурковатое, ИМХО) - простая мультимедийная
система для Emacs. Как и большинство программ для Emacs, является мордой
к другим утилитам. В данном случае, Bongo предоставляет прослушивание
музыки и просмотр видео с использованием mpg321, ogg123, [vlc][],
[TiMidity][], speexdec и mikmod; поддерживается воспроизведение Audio CD
и запросы к CDDB. Bongo прост в использовании и работает из коробки,
настраиваясь через customize.

### Установка

По инфе от автора, в Debian есть пакет с Bongo. Всем остальным придётся
слить вручную с [сайта Bongo][] сорцы и пачку иконок. Можно также
утянуть свежую версию с помощью [darcs][]:

    $ cd ~/.emacs.d
    $ darcs get http://www.brockman.se/software/bongo/

И прописать загрузку `bongo.el` в файл настроек Emacs:

    (load-library "~/.emacs.d/bongo/bongo.el")

Для ускорения можно так же выполнить
`M-x byte-compile-file RET ~/.emacs.d/bongo/bongo.el` и загружать
байт-компилированный файл вместо `bongo.el` (так и для остальных файлов
далее по тексту):

    (load-library "~/.emacs.d/bongo/bongo.elc")

Можно экономить память и время загрузки Emacs, подключая Bongo лишь при
надобности:

    (add-to-list 'load-path "~/.emacs.d/bongo")
    (autoload 'bongo "bongo" "bongo" t)

Кроме того, если хочется юзать [скробблинг на Last.fm][] из Bongo (об
это подробнее - чуть позже), надо будет прописать:

    (load-library "~/.emacs.d/bongo/lastfm-submit.elc")

Для управления уровнем звука из Emacs может понадобиться также поставить
[volume.el][] и подключить его в настроечном файле:

    (load-library "~/.emacs.d/volume-el/volume.elc")

Пути к библиотекам могут, конечно же, отличаться.

После установки пробуем пустить Bongo: `M-x bongo RET` и, если нужно,
настраиваем через `M-x customize-group RET bongo RET`. Настроек много.

### Концепция

Сущности Bongo - библиотека и плейлист, представленные в соответствующих
буферах. В библиотеку импортируем медиа-файлы по одному, целые
директории или данные по URL'у, потом напихиваем нужное в плейлист.
Часто быстрее сразу импортировать нужные треки сразу в буфер плейлиста
или открыть сохранённый плейлист. В списке воспроизведения композиции
одного исполнителя группируются в раскрывающийся список.

Подход достаточно примитивный, что особенно заметно если библиотека
большая и плохо структурирована по папкам; ориентация в записях в Bongo
затрудняется. Например, по жанрам не удастся поискать. Если в
библиотечный буфер Bongo импортировать большое количество треков, он
становится похож на одну большую непонятную жопу из текста.

Зато буферы Bongo легко редактируются стандартными средствами Emacs для
правки текста.

#### Quick start

Запускаем Bongo:

    M-x bongo RET

Переходим в буфер с плейлистом, нажав `h`. Нажимаем `i t`, вводим имя
директории для вставки в плейлист. После импорта выбираем любой трек
мышкой или с клавиатуры и слушаем.

Послушав, нажимаем `C-h m` и читаем, какие в Bongo есть фичи и команды.

Нормального мануала пока нет. Инструкции также можно найти на [сайте
Bongo][сайта Bongo].

### Некоторые фичи

В modeline всех видимых буферов Emacs Bongo умеет выводить кошерные
аккуратные кнопочки управления воспроизведением.

Необычной (имхо) особенностью Bongo является то, что в определении имени
дорожки, исполнителя и альбома он основывается лишь на имени файла, не
принимая во внимание тэг вообще! В принципе, достаточно спорно; с другой
стороны, разбор структурированного имени файла быстрее, чем доступ к
тэгу. Поддерживается несколько схем именования и конечно же можно всё
перенастроить по своему.

Я уже давно все музыкальные файлы [EasyTAG'ом][] переименовываю по схеме
ИСПОЛНИТЕЛЬ - НАЗВАНИЕ КОМПОЗИЦИИ.mp3, так что этого нюанса Bongo и не
заметил. В комплекте с Bongo есть скриптик для создания иерархического
дерева с симлинками на аудиофайлы согласно тэгам. Тем, у кого помимо
нормальных названий для файлов нет ещё и нормальных тэгов в них,
предлагается убить себя об стену с разбега, дабы нех.

Нажатие 'f' в плейлисте позволяет удалить из списка уже воспроизведённые
треки.

#### Scrobbling

Bongo поддерживает отправку инфы о проигрываемых треках на Last.fm при
помощи [lastfmsubmitd][]. Lastfmsubmitd - достаточно прикольная штука,
представляющая собой демон на Python, опрашивающий каталог-спул
'/var/spool/lastfm/', куда другое средство из комплекта - lastfmsubmit -
сбрасывает файлики с информацией о прослушанных треках. Предполагается,
что сторонние программы, которым требуется поддержка
[скробблинга][скробблинг на Last.fm], вызывают lastfmsubmit с
соответствующими параметрами, заставляя его передавать информацию в спул
lastfmsubmitd, который в свою очередь отправляет инфу на Audioscrobbler.
Спул используется для обеспечения надёжности доставки информации на AS
при возможном временном отсутствии доступа к сети. Информация об
использовании lastfmsubmit и lastfmsubmitd лежит в man'ах по этим
программам.

Установка lastfmsubmitd достаточно проста. Нужно слить пакет со
[страницы программы][lastfmsubmitd]; от рута из папки с программой
пустить `python setup.py install`, затем `sh install-example.sh`.
Последнее создаст нужные для работы каталоги и группу lastfm. В эту
группу нужно добавить пользователей, которым потребуется использовать
lastfmsubmitd:

    # usermod -aG lastfm sphinx

При проблемах с установкой - читать файл INSTALL из архива с
lastfmsubmitd.

Остаётся создать конфигурационный файл, общесистемный
(/etc/lastfmsubmitd.conf) или для каждого пользователя свой (в его
домашнем каталоге: \~/.lastfmsubmitd.conf), в котором написать такие
строчки:

    [account]
    user: SphinxTheGeek
    password: p4$$w0rd!

Там же можно прописать прокси, см. файл 'INSTALL' в дистрибутиве
lastfmsubmitd.

Всё, lastfmsubmitd настроен, осталось вызвать в Emacs
`M-x customize-group RET bongo RET`, найти там опцию 'Global Lastfm
Mode', включить на 'on' и сохранить настройки ('Save for future
sessions' или `C-x C-s`). Теперь будет автоматически включаться
скробблинг при прослушке треков.

Bongo, похоже, *не* умеет автоматически запускать демон lastfmsubmitd,
так что лучше прописать запуск `lastfmsubmitd` куда-нибудь сразу при
входе в систему (или при запуске Emacs).

Ещё в комплекте с [lastfmsubmitd][] есть небольшой
mpd-клиент с поддержкой скробблинга, также на Python - LastMP.

### К вопросу о труъ

Bongo планируется включить в Emacs 23 по просьбе лично RMS. Таким
образом, Bongo - мегатруъ.

### Замеченные неприятности

В [VLC][] иногда получается очень плохой звук, будто перекрутили
громкость на слабых колонках. VLC использует какой-то внутренний уровень
громкости (от 0 до 1024, в настройках есть), если поставить его поменьше
и управлять громкостью звука общесистемным микшером (`alsamixer`,
`volume.el` в Emacs), всё будет ок.

  [Emacs]: http://dzhus.org/posts/2007-02-20-emacs-intro.html
  [EMMS]: http://www.gnu.org/software/emms/
  [Bongo]: http://www.emacswiki.org/cgi-bin/wiki/Bongo
  [vlc]: http://www.videolan.org/vlc/
    "VLC media player"
  [TiMidity]: http://timidity.sourceforge.net/
    "Программный проигрыватель MIDI"
  [сайта Bongo]: http://www.brockman.se/software/bongo/
  [darcs]: http://dzhus.org/posts/2006-12-31-darcs.html
  [скробблинг на Last.fm]: http://dzhus.org/posts/2006-03-05-what-is-lastfm.html
  [volume.el]: http://www.brockman.se/software/volume-el/
    "Регулятор уровня громкости каналов для Emacs"
  [EasyTAG'ом]: http://easytag.sourceforge.net/
  [lastfmsubmitd]: http://www.red-bean.com/~decklin/software/lastfmsubmitd/
    "Системный Python-демон для отправки информации на Audioscrobbler"

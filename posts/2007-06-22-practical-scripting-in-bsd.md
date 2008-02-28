---
tags:
- FreeBSD
- администрирование
- происшествия
title:  Два раза отключили свет
---

Сижу переписываю (sorta «рефакторенк») скрипт запуска FastCGI-сервера с
Django во FreeBSD у себя на сервере, чтобы почище был. Отладил, всё как
надо прописал. Старый процесс убил. Ввожу:

    $ sudo ./django start

Запустилось. «Woohoo!».

«Загасим для опыта», — и ввожу:

    $ sudo ./django stop

Пилик! В квартире погас свет! Попытки вернуть его вводом
`./django start` и `./django forcestart` не увенчались успехом — всё
вокруг оставалось черно, как мак на бублике.

Через 10 минут свет вернули, есессно `fsck` при загрузке компьютера и
ещё через пару минут всё гаснет снова. Потом вернули.

Бойтесь демонов.

[Practical rc.d scripting in BSD][]

  [Practical rc.d scripting in BSD]: https://web.archive.org/web/20081210231716/http://www.freebsd.org/doc/en_US.ISO8859-1/articles/rc-scripting/
---
tags:
- Free Software
- 'GNU/Linux'
- HOWTO
- железки
- статьи
title: 'Подключение Microsoft Natural 4000 в GNU/Linux'
---

Долгие годы я пользовался эргономичной клавиатурой SVEN 3000. Несмотря
на то, что от неё отлетели почти все кнопки, клавиатурка была хороша и
достаточно удобна. Прошло пять лет, я польстился на творенье Мелкософта
— новую Microsoft Natural 4000. Представлялось удобным иметь рычажок
зума и скромное количество дополнительных клавиш для работы в [Emacs][].

Подробное ощупывание девайса, а также *вдумчивое* описание процедуры
подключения и настройки в GNU/Linux, включая раскрытие современных
возможностей взаимодействия X-сервера с HAL.

## Щупаем

Я не решился брать набор из клавиатуры с мышкой [MS Natural 7000][]
(сомнительна необходимость в беспроводном подключении такого монстра для
чисто настольного использования; используется левый несовместимый с
другими вендорами формат беспроводной передачи данных), взяв лишь одну
клаву.

[![Microsoft Ergonomic 4000][]][]

Как устройство ввода она мне понравилась. Наверное, расскажу поподробнее
про сам девайс.

По сравнению с моей старой клавой эта гораздо легче. SVEN Ergonomic 3000
— просто гроб.

[![Black &amp; White][]][]

Геометрия достаточно традиционная для эргономичных клавиатур, однако
менее грубая и агрессивная, нежели на моей старенькой клаве. Некоторые
алфавитные кнопки увеличены в размерах. Заметил (следя за своими
руками), что это и впрямь удобно, однако я не отказался бы от чуть
увеличенной кнопки «Y/Н».

К клавиатуре прилагается дополнительная подставка, которая крепится к
дну и сильно «заваливает» весь девайс вперёд:

[![Подставка для наклона][]][]

Сначала я думал, что будет жутко неудобно, но с такой подставкой кисти
рук опущены после запястья и совсем расслабленно лежат на клавиатуре.
Отрицательный эффект — начали уставать локти (из-за увеличенного угла
положения рук от локтя нагрузка на локти чуть выросла).

Между двумя блоками алфавитных символов находится зум-качелька. До него
легко достать указательными пальцами. Было бы здорово прокручивать этой
качелькой буферы в Emacs. Качелька прорезиненная и ребристая, потому
дёргать за неё приятно. Ещё не TrackPoint, но уже не пустое место.

[![Clit-zoom][]][]

Пробел всего один. На самом деле, приятнее нажимать как раз когда
пробела два под разными углами (как было на моей старой клаве). Буду
надеятся, что механизм надёжный, чтобы выдержать годы неравномерных
ударов под такой длинной клавише.

Под пробелом легкодоступные посредством больших пальцев кнопочки «Back»
и «Forward». Было бы приятно привязать их к переключению буферов в
Emacs.

Подложка для рук очень приятная на ощупь, однако пластиковая подставка,
вероятно, гигиеничнее.

Над F1-F12 находится ряд дополнительных клавиш: «Web/Home», «Search»,
«Mail», пять кнопок просто с цифрами, «Mute», пара кнопок регулировки
громкости, «Play/Pause» и в самом конце над «Screen Lock» зачем-то
впихнули кнопку «Calculator».

Справа от самого ряда F1-F12 находится скромная кнопка «F Lock», которая
при нажатии меняет режим традиционных функциональных клавиш! То есть,
когда «F Lock» активен, F1 работает как F1, в противном же случае кнопки
с F генерируют совершенно другие коды.

Отечественные пидорасы (или русскоговорящие пидорасы Микрософта из
какого-нибудь отдела локализации?) не смогли удержаться от того, чтобы
перевести несколько надписей на кнопках. В результате «Enter» стал
«Вводом», а вот «Zoom» и «My Favorites» почему-то не тронули (а я так
надеялся увидеть нетленное «*Зум*» ;-) Кроме того, шрифт надписей на
английском приятный, а на русском — говёный. К счастью, мне на
клавиатуру не любоваться.

[![Пе-ре-вод…][]][]

Над Num Pad поместили кнопки «=», «(» и «)», а также дополнительный
Backspace (полностью дублирует по действию основной).

Вообще по сравнению с моей прошлой клавой нажатия мягче и тише. Насчёт
тишины — спорное достоинство (я привык слышать, что печатаю :-) Из всех
кнопок кошерно звучит только левый «Alt». Пробел утопает с гулким
отстуком.

## Подключаем

Оказывается, в BIOS есть специальная настройка для включения поддержки
USB-клавиатуры. А я так сильно переживал по этому поводу.

### Ядро

На первый взгляд мне показалось, что поддержки клавиатуры в Linux нет.
Первые поиски приводят к нескольким патчам к ядру, но на самом деле в
[HID-ветке ядра][] уже есть [поддержка MS Natural 4000][], более того, с
момента добавления функциональности HID-ветку уже слияли [с основной
веткой Линуса][], так что её можно смело тянуть, компилять и работать с
клавиатурой. На момент написания данных строк последним свежаком
является версия ядра 2.6.24-rc6, так что всё нижесказанное будет на
всякий случай относится именно к ней. Драйвер *полностью* функционален.

Итак,

    # emerge git-sources

В новейших версиях Linux уже включена поддержка многих устройств
[USB-HID][], включая MS Natural 4000. В конфиге ядра нужно отметить:

    Device Drivers --->
        HID Devices --->
            <*> USB Human Interface Device (full HID) support
        Input Device Support --->
            <*> Event interface

Тогда на уровне X11 можно будет юзать драйвер `evdev`. Он кошерен тем,
что предоставляет обобщённый интерфейс к самым разным устройствам ввода.

Никаких специфичных для сабжа опций в *свежем* ядре уже нет (ранние
патчи добавляли для клавиатуры аж отдельную опцию в конфиге ядра).

Ядро пока не компилируем, придётся накатить ещё маленький патчик на 3
строки, обоснование которого приведено далее.

#### Немножко о keycodes

##### Keycodes, scancodes

Каждой кнопке клавиатуры соответствует скан-код («scancode»), которые
ядром переводится в целое число — код клавиши («keycode»).

При помощи программы `showkey(1)`, запущенной в консоли (не X!), можно
увидеть коды клавиш.

Привязывать скан-код к новому keycode можно при помощи `setkeycodes(1)`.
В свою очередь, keycode (или их комбинацию) в консоли можно привязать к
определённому действию (большинство таких привязок уже по умолчанию
сделано, поэтому при нажатии кнопки «пробел» на клавиатуре в консоли
появляется пробел и т. д.) при помощи программы `loadkeys(1)` и файла
keymap. Синтаксис файлов keymap описан в `keymaps(5)`. Например, такой
простейший keymap-файл:

    keycode 172 = F20
    string F20 = "cd ~\n"

После выполнения команды `loadkeys keymapfile` заставит клавишу с кодом
172 действовать так, будто была введена последовательность символов «cd
\~\\n». В большинстве командных оболочек это будет соответствовать
переходу в домашний каталог пользователя.

##### HID, HUT, 255 и X-сервер

Скан-коды клавиатуры Microsoft Natural 4k уже на уровне драйвера для
USB-устройств класса HID привязываются к кодам клавиш. *Семантика*
каждого скан-кода такого устройства зафиксирована в одном из документов
по HID — [HID Usage Tables][], поэтому скан-коды какой-нибудь очередной
дополнительной кнопки для отправки почты на всех клавиатурах (или других
устройствах) этого класса должны быть одинаковы. Вот как в
[drivers/hid/hid-input.c][] по стандарту HUT скан-коды, генерируемые
качелькой Zoom, привязываются к кодам клавиш с макрозначениями
`KEY_ZOOMIN` и `KEY_ZOOMOUT`:

    case 0x22d: map_key_clear(KEY_ZOOMIN);      break;
    case 0x22e: map_key_clear(KEY_ZOOMOUT);     break;

В свою очередь, в эпическом заголовочном файле [include/linux/input.h][]
задаётся числовое значение макросов `KEY_ZOOMIN` и `KEY_ZOOMOUT`:

    #define KEY_ZOOMIN      0x1a2   /* AC Zoom In */
    #define KEY_ZOOMOUT     0x1a3   /* AC Zoom Out */

С учётом объявлений в `hid-input.c`, зум-качелька получает пару keycode
— 418 и 419. Очевидно, что 418\>255 и 419\>255

В этом кроется гадость. X-сервер просто *не будет* видеть нажатия клавиш
таким высоким индексом! Кнопки, которые имеют назначенный ядром keycode
выше 255, игнорируются иксами (ограничение на 255 кнопок — [в самом
стандарте X11][])!

##### Решение проблемы (костыль)

К сожалению, при помощи `setkeycodes(1)` не удастся поменять привязанный
к клавише скан-код. Другое дело, что в `input.h` заняты коды, которые с
сабжевой клавиатурой не используются. Можно поменять встроенные в ядро
привязки.скан-кодов к кодам клавиш на другие, поменьше 255 (но так,
чтобы не было коллизий с другими кнопками на клавиатуре).

Чтобы можно было пользоваться зумом в исках, в на `input.h` накатим
такой микропатчик, меняющий всего три строчки (его можно наложить даже
руками :-):

    --- /home/sphinx/input.h.orig       2007-12-31 12:36:02.000000000 +0300
    +++ /home/sphinx/input.h    2007-12-30 18:37:19.000000000 +0300
    @@ -510,8 +510,8 @@
     #define KEY_TWEN           0x19f
     #define KEY_VIDEOPHONE     0x1a0   /* Media Select Video Phone */
     #define KEY_GAMES          0x1a1   /* Media Select Games */
    -#define KEY_ZOOMIN         0x1a2   /* AC Zoom In */
    -#define KEY_ZOOMOUT        0x1a3   /* AC Zoom Out */
    +#define KEY_ZOOMIN         246     /* AC Zoom In */
    +#define KEY_ZOOMOUT        247     /* AC Zoom Out */
     #define KEY_ZOOMRESET      0x1a4   /* AC Zoom */
     #define KEY_WORDPROCESSOR  0x1a5   /* AL Word Processor */
     #define KEY_EDITOR         0x1a6   /* AL Text Editor */
    @@ -524,7 +524,7 @@
     #define KEY_ADDRESSBOOK    0x1ad   /* AL Contacts/Address Book */
     #define KEY_MESSENGER      0x1ae   /* AL Instant Messaging */
     #define KEY_DISPLAYTOGGLE  0x1af   /* Turn display (LCD) on and off */
    -#define KEY_SPELLCHECK     0x1b0   /* AL Spell Check */
    +#define KEY_SPELLCHECK     235     /* AL Spell Check */
     #define KEY_LOGOFF         0x1b1   /* AL Logoff */

     #define KEY_DOLLAR         0x1b2

#### Наконец, make!

Компилируем, устанавливаем новое ядро
(`make && make install && make modules_install`), перезагружаем систему.
(Я заодно ядро перебрал. Без средств `genkernel` получается ещё
быстрее). Должно работать!

### Проверка работы в консоли

В терминале (не X) запускаем `showkey(1)`, при нажатии *любой* (включая
отклонение качельки зума) из клавиш должно появляться соответствующее
сообщение, типа:

    keycode 418 press
    keycode 418 press
    keycode 418 release

Из всех кнопок не генерирует кода только «F Lock» (этот переключатель
действует полностью на уровне клавиатуры).

Особенностью устройства является то, что при подключении клавиатуры на
самом деле ядро видит сразу *два* девайса:

    $ cat /proc/bus/input/devices | grep N: | grep Microsoft
    N: Name="Microsoft Natural® Ergonomic Keyboard 4000"
    N: Name="Microsoft Natural® Ergonomic Keyboard 4000"

Многие современные клавиатуры работают именно так — фактически одно
устройство отвечает за обработку «стандартных» кнопок, а другое —
«дополнительных».

### X.org

Я опишу современный способ подключения устройства ввода к X.org на
примере этой клавиатуры.

#### Ингредиенты

Дальнешее описание относится к следующему ПО:

-   <span class="underline">`xorg-server` 1.5.3</span>

-   `hal` 0.5.11

-   Драйвер `xf86-input-evdev` 2.1.0, который позволяет работать с
    устройствами на уровне событий. Это выливается в то, что для
    большинства мышек и клавиатур понадобиться только этот драйвер —
    кошерность как она есть.

В будущих версиях, наверное, многое будет так же.

В Gentoo строчку `evdev` лучше прописать в `INPUT_DEVICES` прямо в
`make.conf`, чтобы случайно не собрать X-сервер без поддержки HAL.

Должны запускаться `hald(1)` и `dbus-daemon(1)`:

     # rc-update add hald default
     # rc-update add dbus default

#### HAL + X = hotplug!

С релиза [X11R7.3][] X-сервер поддерживает автообнаружение устройств
ввода через [HAL][]. Устройства ввода не описываются в `xorg.conf`, а
сам сервер определяет их наличию у них специальных HAL-свойств. Можно
также выдёргивать USB-мышки и клавы на лету, сервер подцепит их даже на
другом порте после переподключения (слышны баг-репорты о глюках в этой
функции X; сохраняйте бдительность).

##### Чистим xorg.conf

Во-первых, смело удаляем секцию `InputDevice`, соответствующюю
клавиатуре в `/etc/X11/xorg.conf`! Можно также попробовать использовать
драйвер `evdev(4)` и для мыши, так что её секцию `InputDevice` тоже
удаляем це-ли-ком. Я делаю так и всё пашет, поэтому в дальнейшем я буду
рассматривать именно такую конфигураци. Для сложных конфигов с кучей
экзотических устройств ввода (планшеты там и т. п.) это возможно и не
сработает. Для большинства десктопов с клавиатурой и мышкой, думаю,
сразу заработает.

В секции `ServerLayout` не оставляем ни единого упоминания об
устройствах ввода! У меня она стала выглядеть так:

    Section "ServerLayout"
        Identifier  "Main Layout"
        Screen      "Screen1"
    EndSection

Чтобы сервер X не сошёл с ума от одиночества при загрузке, добавляем
опцию `AllowEmptyInput` в секцию `ServerFlags`:

    Section "ServerFlags"
        Option      "blank time" "10"
        Option      "standby time" "20"
        Option      "suspend time" "30"
        Option      "off time" "60"
        Option      "AllowEmptyInput"
    EndSection

Сервер пока не перезагружаем, нужно ещё прописать политики для HAL.

##### Волшебство HAL

HAL, в общем, предоставляет интерфейс высокого уровня для доступа *к
данным* о всех устройствах системы. В комплект HAL входят такие
прикольные программы, как `hal-device(1)` ийли
`hal-find-by-property(1)`. Команда `hal-device` просто печатает подряд
данные о *всех* устройствах в системе:

    $ hal-device

    ..

    64: udi = '/org/freedesktop/Hal/devices/pci_1106_305'
      pci.product = 'VT8363/8365 [KT133/KM133]'  (string)
      info.bus = 'pci'  (string)
      pci.device_protocol = 0  (0x0)  (int)
      info.udi = '/org/freedesktop/Hal/devices/pci_1106_305'  (string)
      pci.linux.sysfs_path = '/sys/devices/pci0000:00/0000:00:00.0'  (string)
      pci.subsys_product_id = 0  (0x0)  (int)
      linux.subsystem = 'pci'  (string)
      info.vendor = 'VIA Technologies, Inc.'  (string)
      info.subsystem = 'pci'  (string)
      pci.product_id = 773  (0x305)  (int)
      pci.vendor = 'VIA Technologies, Inc.'  (string)
      info.product = 'VT8363/8365 [KT133/KM133]'  (string)
      linux.hotplug_type = 2  (0x2)  (int)
      linux.sysfs_path = '/sys/devices/pci0000:00/0000:00:00.0'  (string)
      pci.vendor_id = 4358  (0x1106)  (int)
      info.parent = '/org/freedesktop/Hal/devices/computer'  (string)
      info.linux.driver = 'agpgart-via'  (string)
      pci.subsys_vendor_id = 0  (0x0)  (int)
      pci.device_class = 6  (0x6)  (int)
      pci.device_subclass = 0  (0x0)  (int)

Видно, что в HAL информация о каждом устройстве представляется в виде
набора пар «свойство»-«значение». Универсальный идентификатор устройства
— значение `udi`.

Существует гламурный GUI к HAL: `hal-device-manager(1)`.

Больше информации содержится в [спецификации по HAL][].

Можно искать устройства по свойствам и по «возможностям». Вот что нам
как раз понадобится:

    $ hal-find-by-capability --capability 'input.keyboard'
    /org/freedesktop/Hal/devices/usb_device_45e_db_noserial_if1_logicaldev_input
    /org/freedesktop/Hal/devices/usb_device_45e_db_noserial_if0_logicaldev_input

Это наша клавиатура и есть (при просмотре информации об одном устройстве
часть `/org/freedesktop/Hal/devices/` можно опускать):

    $ hal-device usb_device_45e_db_noserial_if0_logicaldev_input
    udi = '/org/freedesktop/Hal/devices/usb_device_45e_db_noserial_if0_logicaldev_input'
      linux.device_file = '/dev/input/event3'  (string)
      input.product = 'Microsoft Natural? Ergonomic Keyboard 4000'  (string)
      info.capabilities = { 'input', 'input.keyboard', 'button' } (string list)
      info.udi = '/org/freedesktop/Hal/devices/usb_device_45e_db_noserial_if0_logicaldev_input'  (string)
      linux.subsystem = 'input'  (string)
      input.originating_device = '/org/freedesktop/Hal/devices/usb_device_45e_db_noserial_if0'  (string)
      info.product = 'Microsoft Natural? Ergonomic Keyboard 4000'  (string)
      linux.hotplug_type = 2  (0x2)  (int)
      linux.sysfs_path = '/sys/devices/pci0000:00/0000:00:07.2/usb1/1-2/1-2:1.0/input/input3/event3'  (string)
      info.addons = { 'hald-addon-keyboard' } (string list)
      info.category = 'input'  (string)
      input.physical_device = '/org/freedesktop/Hal/devices/usb_device_45e_db_noserial_if0'  (string)
      input.device = '/dev/input/event3'  (string)
      info.parent = '/org/freedesktop/Hal/devices/usb_device_45e_db_noserial_if0'  (string)

Задача состоит в следующем — заставить HAL добавлять к клавиатурным
устройствам новые *свойства*, по которым сервер X сможет распознать
клавиатуру. Эти свойства имеют имена вида `input.x11_options.*` и, на
самом деле, соответствуют аналогичным свойствам секции `InputDevice`
конфига `xorg.conf`!

Нужная функциональность по прописыванию этих свойств достигается при
помощи написания специальных файлов политик, реализованных на XML
(да-да, XML уже в двух важных конфигах GNU/Linux — [fontconfig][] и сам
HAL). Посмотреть на файлы политики, поставляемые с HAL, можно в
`/usr/share/hal/fdi/policy/`.

Локальные же политики пользователя помещаются в условное место —
`/etc/hal/fdi/policy/`. Кинем туда файл `10-x11-input.fdi` с таким
содержимым <span class="underline">(в версии 1.5 сервера X изменились
названия ключей зумельного конфига с `input.xkb.*` на
`input.x11_options.*`, о чём сказано в [config/x11-input.fdi][])</span>:

    <?xml version="1.0" encoding="UTF-8"?>
    <deviceinfo version="0.2">
      <device>
        <match key="info.capabilities" contains="input.mouse">
          <merge key="input.x11_driver" type="string">evdev</merge>
        </match>

        <match key="info.product" contains="Microsoft Natural">
          <merge key="input.x11_driver" type="string">evdev</merge>
          <merge key="input.x11_options.XkbModel" type="string">evdev</merge>
          <merge key="input.x11_options.XkbVariant" type="string">,winkeys</merge>
          <merge key="input.x11_options.XkbLayout" type="string">us,ru</merge>
          <merge key="input.x11_options.XkbOptions" type="strlist">grp:caps_toggle</merge>
          <append key="input.x11_options.XkbOptions" type="strlist">grp_led:caps</append>
          <append key="input.x11_options.XkbOptions" type="strlist">compose:ralt</append>
        </match>
      </device>
    </deviceinfo>

Если вчитаться, то смысл этого `.fdi`-файла становится прозрачным:

-   элементы `<match></match>` определяют устройства, к которым будут
    применены изменения свойства, описанные в дочерних элементах\
-   элементы `<merge></merge>` и `<append></append>` создают новые
    свойства устройства или добавляют информацию к имеющимся.

В первом блоке `<match></match>` ищется устройство с *возможностями*
мыши и ему назначается драйвер `evdev`!

Во втором блоке мы ищем устройство, в названии которого есть слова
«Microsoft Natural» и вновь назначаем ему драйвер `evdev`, а также
прописываем небольшую кучку свойств. Стоит обратить внимание на то, как
прописываются `input.x11_options.XkbOptions`.

Содержание второго блока полностью аналогично таким опциям секции
`InputDevice` из `xorg.conf`:

    Option      "XkbModel" "evdev"
    Option      "XkbVariant" ",winkeys"
    Option      "XkbLayout" "us,ru"
    Option      "XkbOptions" "grp:caps_toggle,grp_led:caps,compose:ralt"

Разумеется, старый конфиг был гораздо понятнее. Причина в том, что XML —
для нелюдей.

После обновления политики нужно перезапустить демон HAL:

    # /etc/init.d/hald restart

Итак, все демоны запущены, софт и драйвера свежие, `xorg.conf` почищен,
политика к HAL написана, можно перезапускать X-сервер и начинать
работать с клавиатурой.

Если что-то не будет работать, то все ошибки вылавливаются посредством
анализа `/var/log/Xorg.0.log`.

### Проверка работы в X

Нашим другом здесь является `xev(1)`. Запускаем его и по порядку
нажимаем *все* кнопки клавиатуры. Все должны давать событие в `xev(1)`!

## Использование дополнительных кнопок

Об этом — [в следующей записи][].

  [Emacs]: http://dzhus.org/blog/entry/emacs-intro/
  [MS Natural 7000]: http://www.microsoft.com/hardware/mouseandkeyboard/productdetails.aspx?pid=095
  [Microsoft Ergonomic 4000]: https://web.archive.org/web/20090221130518im_/http://farm3.static.flickr.com/2267/2133420395_c413d536b7.jpg
  {width="500" height="375"}
  [![Microsoft Ergonomic 4000][]]: http://www.flickr.com/photos/nothingpersonal/2133420395/
    "Microsoft Ergonomic 4000 by Sphinx The Geek, on Flickr"
  [Black &amp; White]: https://web.archive.org/web/20090221130518im_/http://farm3.static.flickr.com/2149/2133420379_9f2587dc81.jpg
  {width="500" height="375"}
  [![Black &amp; White][]]: http://www.flickr.com/photos/nothingpersonal/2133420379/
    "Black &amp; White by Sphinx The Geek, on Flickr"
  [Подставка для наклона]: https://web.archive.org/web/20090221130518im_/http://farm3.static.flickr.com/2187/2133420389_990dee0813.jpg
  {width="500" height="375"}
  [![Подставка для наклона][]]: http://www.flickr.com/photos/nothingpersonal/2133420389/
    "Подставка для наклона by Sphinx The Geek, on Flickr"
  [Clit-zoom]: https://web.archive.org/web/20090221130518im_/http://farm3.static.flickr.com/2082/2144008662_91305f44b9.jpg
  {width="500" height="375"}
  [![Clit-zoom][]]: http://www.flickr.com/photos/nothingpersonal/2144008662/
    "Clit-zoom by Sphinx The Geek, on Flickr"
  [Пе-ре-вод…]: https://web.archive.org/web/20090221130518im_/http://farm3.static.flickr.com/2332/2143224521_18b6aee4f8.jpg
  {width="500" height="368"}
  [![Пе-ре-вод…][]]: http://www.flickr.com/photos/nothingpersonal/2143224521/
    "Пе-ре-вод… by Sphinx The Geek, on Flickr"
  [HID-ветке ядра]: http://git.kernel.org/?p=linux/kernel/git/jikos/hid.git
  [поддержка MS Natural 4000]: http://git.kernel.org/?p=linux/kernel/git/jikos/hid.git;a=commit;h=1fe8736da695c2b14961438c73d5600538bd92d9
  [с основной веткой Линуса]: http://www.kernel.org/pub/scm/linux/kernel/git/torvalds/linux-2.6.git
  [USB-HID]: http://en.wikipedia.org/wiki/USB_human_interface_device_class
  [HID Usage Tables]: http://www.usb.org/developers/devclass_docs/Hut1_12.pdf
  [drivers/hid/hid-input.c]: http://git.kernel.org/?p=linux/kernel/git/torvalds/linux-2.6.git;a=blob;f=drivers/hid/hid-input.c;h=0b27da7d749700b4d0e2481b7c121ce9fa033aaa;hb=HEAD
  [include/linux/input.h]: http://git.kernel.org/?p=linux/kernel/git/torvalds/linux-2.6.git;a=blob;f=include/linux/input.h;h=2075d6da2a313d9791ea1e714cb5beb18c91f7e6;hb=HEAD
  [в самом стандарте X11]: https://bugs.freedesktop.org/show_bug.cgi?id=11227
  [X11R7.3]: http://xorg.freedesktop.org/archive/X11R7.3/doc/RELNOTES.txt
  [HAL]: http://www.freedesktop.org/wiki/Software/hal
  [спецификации по HAL]: http://people.freedesktop.org/~david/hal-spec/hal-spec.html
  [fontconfig]: http://fontconfig.org/wiki/
  [config/x11-input.fdi]: http://cgit.freedesktop.org/xorg/xserver/tree/config/x11-input.fdi?id=35b14519b4a3158592a089170ec039bbc219603e
  [в следующей записи]: http://dzhus.org/blog/entry/using-extra-keys/

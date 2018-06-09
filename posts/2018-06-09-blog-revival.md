# Снова в эфире

Я почти не писал в этот журнал порядка 8 лет. Сначала было лень, потом
старый сервер прохудился (или не был оплачен?), перезапускать всё
стало совсем не с руки. Прошлой осенью начал думать о восстановлении,
проверил резервные копии (всё это время спокойно думал, что они есть)
— а вся база постов (маленький файлик SQLite) то потерялась. Чёрт
знает, куда я её засунул. Как назло, [исходники][] старого движка
остались.

А написать есть о чём. Закончил Бауманку, нашёл первую работу,
умудрился переехать в Лондон. Стал выпивать, писать на Haskell,
путешествовать, подмечать забавное в незнакомцах, снимать квартиру,
думать о том, что говорю и пишу, почитывать [Дмитрия Евгеньевича][g].
Чувствую некоторую усталость от технологий и многолетней оптимизации
как мне казалось диапазона навыков. Какие в жизни радости? Лёгкий
гедонизм (это я так называю свою инфантильность и почти полное
отстутствие какой-либо ответственности) и саморефлексия.

Кое-как восстановил некоторые прошлые записи (где-то треть всего, что
было) — часть нашлась в [интернет-архивах][ia] старого сайта, ещё
несколько технических заметок выковырял из архивов же [RuNIX][]
(который за это время тоже умер — продолжаем решать задачу сохранности
информации!). Хорошо, что угорал по семантической вёрстке —
[парсилку][ia-crawler] написать было несложно, из обрывков старого
HTML она всё перевела мне обратно в Markdown. Некоторые записи удалил
как совсем мусорные (к счастью, старый стиль в целом почти не вызывает
стыда).

Полистал старое, первая мысль — у ребёнка было много свободного
времени и возможность тратить его «за компьютером». Сейчас я стараюсь
быть ленивым уже не только в работе, но и в том, что изучаю. За
возросший уровень потребления и относительную финансовую независимость
простой человек платит жизненным временем. Да и голова не резиновая.

Движки дело пыльное, а вот публиковать текст в интернете задача
несложная. Теперь я использую [Hakyll][], разместил всё в [S3][] за
[Cloudflare][]. Код и записи разместил в одном репозитарии, [Travis][]
просто пересобирает и публикует всё что есть на каждое изменение. В
общем, склеил сервисы. Hakyll просто потому что что-то подкручивать в
логике приятнее на знакомом языке. Думал, может всё выложить на GitHub
Pages, но не захотелось в одну корзину всё класть, да и поддерживать
что-то в AWS и Cloudflare актуальнее с точки зрения рыночных навыков.

Неизменен только мой [GNU Emacs][gnu-emacs]. За годы я лишь избавился
от личной темы, перейдя на светлую [Solarized][] (она теплее). А
записи я по-прежнему пишу в [markdown-mode][] (к которому по
[прошествии 10 лет][md-undefined] с прошлого раза я недавно написал
очередную полезняшку, чтобы [проверять неиспользуемые
ссылки][md-unused]).

Постараюсь скоро сделать какие-то ретроспективные записи. Некоторые
заметки за прошедшее время просто писал в стол, возможно, что-то и
опубликую задним числом.

Надеюсь, теперь уж точно ничего не потеряется. Хотя SaaS оно такое,
сегодня есть, а завтра его нет, но я хотя бы всё так же контролирую
контент, да и отсутствие приватных записей упрощает резервирование.
Всё стало дешевле и быстрее, на крайний случай даже сам GitHub может
отобразить Markdown.

Жизнь — невыпуклая оптимизация, и приятно иногда отследить свою
траекторию.

[исходники]: https://github.com/dzhus/dima-blog

[ia]: http://web.archive.org/web/20110509001859/http://dzhus.org:80/

[runix]: http://web.archive.org/web/20080101012250/http://runix.org:80/

[ia-crawler]: https://github.com/dzhus/internet-archive-crawler

[hakyll]: https://jaspervdj.be/hakyll/

[s3]: https://github.com/dzhus/globalchypre/blob/4f194e5/terraform/blog.tf

[cloudflare]: https://github.com/dzhus/globalchypre/blob/99046fb/terraform/cdn.tf

[travis]: https://github.com/dzhus/blog/blob/61cccbf/.travis.yml

[gnu-emacs]: http://dzhus.org/posts/2007-02-20-emacs-intro.html

[solarized]: https://github.com/bbatsov/solarized-emacs

[markdown-mode]: https://jblevins.org/projects/markdown-mode/

[md-undefined]: https://jblevins.org/projects/markdown-mode/rev-1-6

[md-unused]: https://github.com/jrblevin/markdown-mode/pull/322

[g]: http://galkovsky.livejournal.com
---
tags:
- JavaScript
- XML
- 'веб-разработка'
title: С ослами вход воспрещён
---

Плюнул на всё и стал отдавать странички в `application/xhtml+xml`.

[Оказалось][], что в браузерах при этом не может работать
`document.write`, так что я [переписал][] отвалившуюся ленту закладок с
[del.icio.us][], по-простому используя стандартный подход с обновлением
DOM-дерева на лету. Заодно разобрался, наконец, в этом вашем JSON.

Надёжно работает во всех доступных мне браузерах (на Gecko, Webkit,
Presto, плюс в рысях).

  [Оказалось]: https://developer.mozilla.org/en/Mozilla_Web_Developer_FAQ#How_is_the_treatment_of_application.2fxhtml.2bxml_documents_different_from_the_treatment_of_text.2fhtml_documents.3f
  [переписал]: http://support.delicious.com/forum/comments.php?DiscussionID=1985
  [del.icio.us]: http://dzhus.org/posts/2006-10-26-what-is-delicious.html

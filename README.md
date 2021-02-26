# far-lua-diagnostics
Lua debug trace, fork of <a href="https://github.com/ignacio/StackTracePlus">StackTracePlus @Ignacio</a>,<br />
will work only in FAR manager environment<br /><br />
Dependencies:<br />
https://github.com/dr-dba/far-lua-general-utils<br />
https://github.com/dr-dba/far-lua-explorer<br />
<br />
FAR manager:<br />
https://farmanager.com/<br />
FAR manager forum discussion:<br />
https://forum.farmanager.com/viewtopic.php?f=15&t=12370<br />

<img src="StackTracePlusPlus-@Xer0X.jpg" />
<br />
Другой пример исползования<br />
Что используем:<br />
1.) Загрузку (можно и выгрузку вышеуказанным способом) одичного фаяла макросов<br />
ИЗ ПРОИЗВОЛьНОЙ ДИРЕКТОРИИ без перезагрузки<br />
(обратите внимание из какой трешевой папки это было загружено)<br />
2.) Использование StackTracePlus@Х для просмотра контекста <br />
(a не как показыватель ошибок в примере ранее)<br />
3.) Можно наставить таких tracebacks в любом месте кода, <br />
и различать их по переданного типа маркера.<br />
в примере это "Hello World"<br />

<img src="stp@x.JPG" />

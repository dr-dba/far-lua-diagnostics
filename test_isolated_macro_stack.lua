-- [[
if true then return end --]] 

--[[
Пример исползования
Что используем:
1.) Загрузку (можно и выгрузку вышеуказанным способом) одичного фаяла макросов
ИЗ ПРОИЗВОЛьНОЙ ДИРЕКТОРИИ без перезагрузки
(обратите внимание из какой трешевой папки это было загружено)
2.) Использование StackTracePlus@Х для просмотра контекста 
(не как показыватель ошибок)
3.) Можно наставить таких tracebacks в любом месте кода, 
и различать их по переданного типа маркера,
в примере это "Hello World"
-- ]]

require("StackTracePlusPlus-@Xer0X")

Macro { description = "my cool macro test";
	area = "Editor"; key = "CtrlSpace";
	priority = 100;
	action = function()
		local abc = 12345
		Xer0X.STP.traceback("Hello World!")
	end;
}

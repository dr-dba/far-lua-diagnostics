--[[
Проверка корректности скрипта в редакторе + выполнение

Original version of @JD:
https://forum.farmanager.com/viewtopic.php?f=60&t=8008
https://gist.github.com/johnd0e/e5ed84eb5d94f7001b57c9b5bcd22991

Modified (a bit more advanced) version of @Xer0X+@citRix:
https://forum.farmanager.com/viewtopic.php?f=15&t=12432
https://github.com/dr-dba/far-lua-diagnostics/
Editor_MacroCheck.lua

Dependencies:
-------------
https://github.com/dr-dba/far-lua-diagnostics/
StackTracePlusPlus-@Xer0X.Lua
https://github.com/dr-dba/far-lua-internals/
introspection-@Xer0X.Lua
https://github.com/dr-dba/far-lua-general-utils/
Lib-Common-@Xer0X.Lua

TODO: stack trace util
]]

local Info = Info or package.loaded.regscript or function(...) return ... end -- luacheck: ignore 113/Info
local nfo = Info {
	_filename or ...,
	description	= "Check macro in editor [Reload/Execute/Variables] (+@Xer0X mod)",
	id		= "DA9B41E0-3896-4533-94E9-D5CE10BB7968",
	name		= "MacroCheck (+@Xer0X mod)",
	version		= "1.2",
	version_mod	= "1.1.3",
	author		= "JD",
	author_mod	= "Xer0X",
	url		= "http://forum.farmanager.com/viewtopic.php?f=60&t=8008",
	url_mod		= "https://forum.farmanager.com/viewtopic.php?f=15&t=12432",
	minfarversion	= { 3, 0, 0, 4261, 0 }, -- far.FarClock
	files		= "scriptscfg.*.sample",
	options = {
		Load_one  = "[ Load   macro ]",
		UnLoadOne = "[ UnLoad macro ]",
		Check_one = "[ Check  macro ]",
		Eval_one  = "[ Eval   macro ]",
		Load_all  = "[ Reload all   ]",
		MoonToLua = "[ Moon to Lua  ]",
		work_area = "Plugins Disks Config",
		file_mask = "*.lua;*.lua.cfg;*.lua.dat;*.lua.ini;*.moon;far_standards.lua.cfg",
		use_selected = {
			Eval_one =	true,
			Check_one =	true,
			MoonToLua =	true,
			Execute	=	true,
		}
		-- todo pos to err (?moon)
		-- todo macro keys
		-- todo run cached f
	}
}
if not nfo then return end
local O = nfo.options

local	le_success, le = pcall(require, "LuaExplorer-@Xer0X")
if not	le_success
then	le_success, le = pcall(require, "LE")
end
if not	le_success
then	le = function()	far.Message([[
Lua Explorer „Advanced“ is required for this function.
http://forum.farmanager.com/viewtopic.php?f=60&t=7988]], "")
	end
end

local F = far.Flags
local meta = { __index = _G }

local function ProcessName(mask, file)
	return far.ProcessName(F.PN_CMPNAMELIST, mask, file, F.PN_SKIPPATH)
end

local function sandbox(f, env, mt)
	return setfenv(f, setmetatable(env or { }, mt or meta))
end

local function env_prompt(Title, Prompt, History, Src)
	repeat
		local	expr = far.InputBox(nil, Title, Prompt, History or Title, Src, nil, nil, F.FIB_ENABLEEMPTY)
		-- ??id
		if not	expr then return end
		local	f, err = loadstring(expr)
		if	f
		then	local	success, err2 = pcall(sandbox(f))
			if	success
			then	return getfenv(f)
			else	far.Message(err2, 'Error', nil, 'w')
			end
		else
			far.Message(err, 'Error in expression', nil, 'wl')
		end
	until false
end

local function getText(ei, sel)
	if not	ei
	then	ei = editor.GetInfo()
	elseif	type(ei) == "number"
	then	ei = editor.GetInfo(ei)
	end
	local str =	{ }
	local from =	ei.UseSelection and sel.StartLine or 1
	local to =	ei.UseSelection and sel.EndLine	or ei.TotalLines
	for ii = from, to
	do	local cur = editor.GetString(ei.EditorID, ii)
		str[ii] = ei.UseSelection
			and	cur.StringText:sub(cur.SelStart, cur.SelEnd)
			or	cur.StringText
	end
	return table.concat(str, "\n", from, to)
end

--[[
local function getAllLocals(level)
	local locals = { }
	for i = 1, 1000
	do	local	k, v = debug.getlocal(level + 1, i)
		if not	k then return locals, i - 1 end
		locals[k] = v
	end
end

local function getinfo(thread) --todo
	local f = debug.getinfo(thread, 1, "f").func
	local locals, nl = {}
	for i = 1, 1000
	do	local	k, v = debug.getlocal(thread,1,i)
		if not	k then nl = i - 1; break end --todo
		locals[k] = v
	end
	local env = getfenv(f)
	if nl > 0
	then	locals.__ENV__ = locals.__ENV__
			or next(env) and env
			or nil
		le(locals, "locals")
	else
		if next(env) then le(env,"environment") end
	end
end

local function sethook(f)
	local function hook()
		-- todo catch error??
		if f == debug.getinfo(2, "f").func
		then
			debug.sethook()
			local	locals, nl = getAllLocals(2)
			local	env = getfenv(f)
			if	nl > 0
			then	locals.__ENV__ = locals.__ENV__ or next(env) and env or nil
				le(locals, "locals")
			else	if next(env) then le(env, "environment") end
			end
		end
	end
	debug.sethook(hook, "r")
end
--]]

local function ReloadMacro()
	local start = far.FarClock()
	if far.MacroLoadAll()
	then
		local elapsed = math.floor((far.FarClock() - start) / 1000)
	--	require("tooltip")("(re)loaded in "..elapsed.." ms", "Macros", 1000)
		local elapsed_2 = mf.size2str(far.FarClock() - start, 256)
	--	require("tooltip")("(re)loaded in "..elapsed.." ms", "Macros", 1000)
		far.Message("(Re)Loaded in "..elapsed_2.." ms", "Macros", "OK?")

	end
end

local function setEditorPos(ei)
	editor.SetPosition(ei.EditorID, ei);
	editor.Redraw(ei.EditorID)
end

local function ErrMessage(msg, line, ei)
	if not	ei
	then
		far.Message(msg, nfo.name, nil, "lw")
	else
		if ei.UseSelection
		then line = line + ei.BlockStartLine - 1
		end
		editor.Select(ei.EditorID, F.BTYPE_STREAM, line, 1, -1, 1)
		-- show the line with the error:
		setEditorPos({ CurLine = line })
		local	msg_answer = far.Message(string.format("%s\nat line %s", msg, line), nfo.name, "&UnLoad one;&Go to error;&Cancel", "lw")
		if	msg_answer ==-1
		or	msg_answer == 3
		then	--[[ do nothing,
			but actually restore the editor position needed ]]
			editor.Select(ei.EditorID, ei.UseSelection and F.BTYPE_STREAM or F.BTYPE_NONE)
			setEditorPos(ei)
		elseif	msg_answer == 1
		then	-- Unload the opened file:
			Xer0X.fnc_mcr_src_all_clean(ei.FileName)
			-- restore the editor position:
			editor.Select(ei.EditorID, ei.UseSelection and F.BTYPE_STREAM or F.BTYPE_NONE)
			setEditorPos(ei)
		elseif	msg_answer == 2
		then	-- goto error, but we are already there
		end
	end
end

local function ErrorCurrent(Err, ei)
	local line, err, err1, err2
	local err_msg_orig = Err
	if	ei.isMoon
	then	err1, line, err2 = Err:match("^(.-) %[(%d+)%] >>%s*(.+)$")
	else	line, err = Err:match(":(%d+): (.+)$")
	end
	if	line
	then	return ErrMessage(err or err1..err2, tonumber(line), ei)
	else	return ErrMessage(Err)
	end
end

--[[
if 	file
and	file:sub(1, 3) == "..."
then
	local trimpos = 4
	local valid, len = file:utf8valid()
	if not valid and len == 3 then trimpos = trimpos + 1 end
	local ending = file:sub(trimpos)
	if not valid then far.Show(ending, ending:utf8valid()) end
	if ending == ei.FileName:sub(-ending:len()) then file = ei.FileName end
end
--]]

local SELECTION = "selection"

local function ErrorRuntime(thread, Err, ei)
	local level, di = 0
	repeat	di = debug.getinfo(thread, level)
		level = level + 1
	until not di or di.what ~= "C"
	if	di
	and (	di.source == SELECTION
	or	di.source:match("^@(.+)$") == ei.FileName)
	then	ErrorCurrent(Err, ei)
	else	-- ?? open in editor
		ErrMessage(Err)
	end
end


-- todo: local stacktrace = export.CustomErrHandler or debug.traceback
local function inspectRet(success, ...)
	if not success then return nil, ... end
	local	n = select("#", ...)
	if	n == 1
	then	-- todo: check if not le
		le(..., type(...))
	elseif
		n > 0
	then
		repeat	local ret, pos = far.Show(...)
			if ret then le(ret.arg, "value "..tostring(pos)) end
		until not ret
	end
	return true, n
end

local function call(f, ei)
	local	thread = coroutine.create(f)
	local	success, res = inspectRet(coroutine.resume(thread))
	if not	success
	then	-- todo: inspect stack
		ErrorRuntime(thread, res, ei)
	end
	return thread, res
end

local scriptspath = win.GetEnv("FARPROFILE"):lower().."\\macros\\scripts\\"
local sp_len = scriptspath:len()
local last_co

local function checkMacro(mode)
	local	ei =	editor.GetInfo()
	local	sel =	editor.GetSelection()
	if	sel
	then	-- save for further restoring
		ei.Selection = {
			BlockType =	ei.BlockType,
			BlockStartLine= ei.BlockStartLine,
			BlockStartPos = sel.StartPos,
			BlockWidth =	sel.EndPos - sel.StartPos + 1,
			BlockHeight =	sel.EndLine -sel.StartLine+ 1,
		}
		if not	mode
		or	O.use_selected[mode]
		then	ei.UseSelection = true
		end
	end
	ei.isMoon = ProcessName("*.moon", ei.FileName)
	-- name chunk only if selection is absent
	local filename = not ei.UseSelection and ei.FileName or nil
	local src = getText(ei, sel)
	--[[ Old original evaluate:
	-- todo return
	if	mode == "Eval"
	and not ei.isMoon
	and not string.find(src, "\n")
	and not string.find(src, "%f[%w]return%f[%W]")
	then	src = "return "..src
	end
	--]]
	-- New "evaluate" @Xer0X:
	if mode == "Eval_one"
	and not ei.isMoon
	then src = "return (function(...)\n"..src.."\nend)(...)"
	end -- end of evaluating
	local loadstring = ei.isMoon and require("moonscript").loadstring or loadstring
	local f, Err = loadstring(src, (filename and "@"..filename or SELECTION))
	if	Err
	then    ErrorCurrent(Err, ei)
		last_co = nil
		return
	end
	if not	mode
	or	mode == "Check_one"
	then
		if ei.FileName:lower():sub(1, sp_len) ~= scriptspath
		then mf.postmacro(Keys, "Tab")
		end
		local	btns = "&UnLoad One;Load &One;Reload &All;&Execute;&Variables"..(ei.isMoon and ";&MoonToLua" or "")
		local	ans = far.Message("Syntax is Ok", nfo.name, btns)
		if	ans == -1
		then	return
		else	mode = ({ "UnLoadOne", "Load_one", "Load_all", "Execute", "Variables", "MoonToLua" })[ans]
		end
	end
	local exec_modes = {
		Execute = true,
		Selection=true,
		Variables=true,
		Eval_one =true,
	--	EvalFile= true, --??
	}
	if	mode == "Load_one"
	then
		Xer0X.fnc_macro_one_load(nil, Editor.FileName)
	elseif
		mode == "UnLoadOne"
	then
		Xer0X.fnc_mcr_src_all_clean(Editor.FileName)
	elseif
		mode == "MoonToLua" and ei.isMoon
	then	-- todo selection
		-- todo button
		local	lua = require("moonscript").to_lua(src)
		if	far.Message(lua, nfo.name, "Copy;", "l")
		then	far.CopyToClipboard(lua)
		end
	elseif
		mode == "Load_all"
	then
		if 0 == band(ei.CurState, F.ECSTATE_SAVED)
		then editor.SaveFile()
		end
		-- если при сохранении не возникло вопросов
		if Area.Editor then ReloadMacro() end
	elseif
		exec_modes[mode]
	then	-- ?? optionally persist
		local	env
		if	mode == "Eval_one"
		and	ei.UseSelection
		then	-- 2ask for env
			local title = ("call: "..src):gsub("[\r\n].+", "...")
			env = env_prompt(title, "env: ... (type as Lua code or leave empty)", "evalSelection")
			if not env then return end
		end
		-- todo: --\\?\C
		env = env or { _filename = filename }
		local n
		last_co, n = call(sandbox(f, env), ei)
		if	mode == "Eval"
		or	mode == "Eval_one"
		or	mode == "EvalFile"
		then
			if n == 0 then far.Message("no values returned") end
		elseif
			mode == "Variables"
		then
			if not	le_success
			then	le();
				return
			end
		end
	else
		far.Message(string.format("wrong mode specified: %s", mode), nfo.name, nil, "w")
	end
end -- checkMacro

Macro { description = "Check macro in editor [LoadOne/Reload/Execute/Variables]",
	id = "8FCF8185-A490-41D3-9F95-19E7669252D9",
	area = "Editor",
	key = "CtrlEnter",
	filemask = O.file_mask,
	action = function() checkMacro() end
}

Macro { description = "Eval selected text and inspect returned values",
	id = "3BCEADCB-FF93-4B7D-9EC3-97B76FAE0ABA",
	area = "Editor",
	key = "CtrlShiftEnter",
	flags = "EVSelection",
	filemask = O.file_mask,
	action = function() checkMacro("Eval_one") end
}

Macro { description = "Moon to Lua", -- todo
	id = "3B5508B4-A0A5-4B74-9573-D2505DBF71D5",
	area = "Editor",
	key = "F1",
	filemask = "*.moon",
	action = function() checkMacro("MoonToLua") end
}

if O.Check_one then
MenuItem {
	description = "Check Lua/Moon script",
	guid = "11958400-9BD7-4173-ABE0-7181682A282D",
	menu = "Plugins",
	area = "Editor",
	text = function() return ProcessName(O.file_mask, editor.GetFileName()) and O.Check_one end,
	action = function() checkMacro("Check_one") end
} end

if O.UnLoadOne then
MenuItem {
	description = "Reload macros",
	guid = "12B23F7B-05CE-4824-9CEC-4AE6BA09AB7A",
	area = "Editor",
	menu = "Plugins",
	text = function() return ProcessName(O.file_mask, editor.GetFileName()) and O.UnLoadOne end,
	action = function(OpenFrom)
		if	OpenFrom == F.OPEN_EDITOR
		then	checkMacro("UnLoadOne")
		else	ReloadMacro()
		end
	end
} end

if O.Load_one then
MenuItem {
	description = "Reload macros",
	guid = "2E0B9056-C8FC-45ED-BE8D-A622658D8113",
	area = "Editor",
	menu = "Plugins",
	text = function() return ProcessName(O.file_mask, editor.GetFileName()) and O.Load_one end,
	action = function(OpenFrom)
		if	OpenFrom == F.OPEN_EDITOR
		then	checkMacro("Load_one")
		end
	end
} end

if O.Eval_one then
MenuItem {
	description = "Eval Lua/Moon script",
	guid = "01ABE288-8C0D-4824-94C4-3F30065D9085",
	menu = "Plugins",
	area = "Editor",
	text = function() return ProcessName(O.file_mask, editor.GetFileName()) and O.Eval_one end,
	action = function() checkMacro("Eval_one") end
} end

MenuItem {
	description = "Reload macros",
	guid = "6F3144E6-38A4-4C5D-B9A2-2B38C4DE83C8",
	area = "Common",
	menu = O.work_area,
	text = O.Load_all,
	action = function(OpenFrom)
		if	OpenFrom == F.OPEN_EDITOR
		and	ProcessName(O.file_mask, editor.GetFileName())
		then	checkMacro("Load_all")
		else	ReloadMacro()
		end
	end
}

-- --------------------- todo todo todo

NoMacro {
	description = "",
	id = "7E57D345-65DD-49F2-AD3D-21A506854B80",
	area = "Editor",
	key = "F1",
	priority = 50,
	condition = function() return last_co end,
	action = function() far.Show(coroutine.resume(last_co)) end
}

-- @@@@@

--[[
https://gist.github.com/johnd0e/e5ed84eb5d94f7001b57c9b5bcd22991
http://forum.farmanager.com/viewtopic.php?f=60&t=8008
TODO: stack trace util
]]

local Info = Info or package.loaded.regscript or function(...) return ... end -- luacheck: ignore 113/Info
local nfo = Info {
	_filename or ...,
	description = "Check macro in editor [Reload/Execute/Variables] (+@Xer0X mod)",
	id = "DA9B41E0-3896-4533-94E9-D5CE10BB7968",
	name = "MacroCheck (+@Xer0X mod)",
	version = "1.2",
	version_mod = "1.0",
	author = "jd",
	author_mod = "Xer0X",
	url = "http://forum.farmanager.com/viewtopic.php?f=60&t=8008",
	url_mod = "http://forum.farmanager.com/viewtopic.php?f=60&t=8008",
	minfarversion = {3, 0, 0, 4261, 0}, -- far.FarClock
	files = "scriptscfg.*.sample",
	options = {
		check	= "[ check  macro  ]",
		eval	= "[ eval   macro  ]",
		reload	= "[ reload macros ]",
		reloadmenu = "Plugins Disks Config",
		useselection = {
			Eval = true,
			MoonToLua = true
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
local function ProcessName(mask, file)
	return far.ProcessName(F.PN_CMPNAMELIST, mask, file, F.PN_SKIPPATH)
end

local meta = { __index = _G }
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
end -- ??

local function ErrMessage(msg, line, ei)
	if not	ei
	then
		far.Message(msg, nfo.name, nil, "lw")
	else
		if ei.UseSelection then line = line + ei.BlockStartLine - 1 end
		editor.Select(ei.EditorID, F.BTYPE_STREAM, line, 1, -1, 1)
		setEditorPos({ CurLine = line })
		if -1 == far.Message(msg, nfo.name, nil, "lw")
		then	editor.Select(ei.EditorID, ei.UseSelection and F.BTYPE_STREAM or F.BTYPE_NONE)
			setEditorPos(ei)
		end
	end
end

local function ErrorCurrent(Err, ei)
	local line, err, err1, err2
	if	ei.isMoon
	then	err1, line, err2 = Err:match("^(.-) %[(%d+)%] >>%s*(.+)$")
	else	line, err = Err:match(":(%d+): (.+)$")
	end
	if not	line
	then	return ErrMessage(Err)
	else	return ErrMessage(err or err1..err2, line, ei)
	end
end

--[[
if 	file and
	file:sub(1, 3) == "..."
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
	if di and (
		di.source == SELECTION or
		di.source:match("^@(.+)$") == ei.FileName
			)
	then	ErrorCurrent(Err, ei)
	else	-- ?? open in editor
		ErrMessage(Err)
	end
end

--[[
local function fnc_test()
	local b = 4
	local ret1, ret2 = far.Show("Hello", "World")
	return ret1, ret2
end
return fnc_test()
--]]

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
		repeat
			local ret, pos = far.Show(...)
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
	--	ErrorRuntime(thread, stacktrace(thread, res), ei)
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
			BlockType = ei.BlockType,
			BlockStartLine = ei.BlockStartLine,
			BlockStartPos = sel.StartPos,
			BlockWidth =	sel.EndPos -	sel.StartPos +	1,
			BlockHeight =	sel.EndLine -	sel.StartLine + 1
		}
		if not	mode
		or	O.useselection[mode]
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
	if mode == "Eval"
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
	then
		if not (ei.FileName:lower():sub(1, sp_len) == scriptspath)
		then	mf.postmacro(Keys, "Tab")
		end
		local	btns = "Reload &One;Reload &All;&Execute;&Variables"
		local	ans = far.Message("Syntax is Ok", nfo.name, btns)
		if	ans == -1
		then	return
		else	mode = ({ "ReloadOne", "Reload", "Execute", "Variables" })[ans]
		end
	end
	local exec_modes = {
		Execute = true,
		Selection = true,
		Variables = true,
		Eval = true
	--	EvalFile = true, --??
	}
	if	mode == "ReloadOne"
	then
		Xer0X.fnc_macro_one_load(nil, Editor.FileName)
	elseif
		mode == "MoonToLua" and ei.isMoon
	then	-- todo selection
		-- todo button
		local lua = require("moonscript").to_lua(src)
		if far.Message(lua, nfo.name, "Copy;", "l")
		then far.CopyToClipboard(lua)
		end
	elseif
		mode == "Reload"
	then
		if 0 == band(ei.CurState, F.ECSTATE_SAVED)
		then editor.SaveFile()
		end
		-- если при сохранении не возникло вопросов
		if Area.Editor then ReloadMacro() end
	elseif
		not exec_modes[mode]
	then
		far.Message("wrong mode specified", nfo.name, nil, "w")
	else	-- ?? optionally persist
		local env
		if	mode == "Eval"
		and	ei.UseSelection
		then -- 2ask for env
			local title = ("call: "..src):gsub("[\r\n].+", "...")
			env = env_prompt(title, "env: ... (type as Lua code or leave empty)", "evalSelection")
			if not env then return end
		end
		-- todo: --\\?\C
		env = env or { _filename = filename }
		local n
		last_co, n = call(sandbox(f, env), ei)
		if	mode == "Eval"
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
	end
end

Macro { description = "Check macro in editor [Reload/Execute/Variables]",
	id = "8FCF8185-A490-41D3-9F95-19E7669252D9",
	area = "Editor", key = "CtrlEnter",
	filemask = "*.lua;*.lua.cfg;*.lua.dat;*.moon;far_standards.lua.cfg",
	action = function() checkMacro() end
}

Macro { description = "Eval selected text and inspect returned values",
	id = "3BCEADCB-FF93-4B7D-9EC3-97B76FAE0ABA",
	area = "Editor", key = "CtrlShiftEnter",
	flags = "EVSelection",
	filemask = "*.lua;*.lua.cfg;*.lua.dat;*.moon",
	action = function() checkMacro("Eval") end
}

Macro { description = "moon to lua", -- todo
	id = "3B5508B4-A0A5-4B74-9573-D2505DBF71D5",
	area = "Editor", key = "F1",
	filemask = "*.moon",
	action = function() checkMacro("MoonToLua") end
}

if O.check then
MenuItem {
	description = "Check Lua/Moon script",
	guid = "11958400-9BD7-4173-ABE0-7181682A282D",
	menu = "Plugins",
	area = "Editor",
	text = function() return ProcessName("*.lua;*.lua.cfg;*.lua.dat;*.lua.ini;*.moon", editor.GetFileName()) and O.check end,
	action = function() checkMacro() end
}
end

if O.eval then
MenuItem {
	description = "Eval Lua/Moon script",
	guid = "01ABE288-8C0D-4824-94C4-3F30065D9085",
	menu = "Plugins",
	area = "Editor",
	text = function() return ProcessName("*.lua;*.lua.cfg;*.lua.dat;*.lua.ini;*.moon", editor.GetFileName()) and O.eval end,
	action = function() checkMacro("Eval") end
}
end

MenuItem {
	description = "Reload macros",
	guid = "6F3144E6-38A4-4C5D-B9A2-2B38C4DE83C8",
	area = "Common",
	menu = O.reloadmenu,
	text = O.reload,
	action = function(OpenFrom)
		if	OpenFrom == F.OPEN_EDITOR
		and	ProcessName("*.lua;*.lua.cfg;*.lua.dat;*.lua.ini;*.moon", editor.GetFileName())
		then	checkMacro("Reload")
		else	ReloadMacro()
		end
	end
}

-----------------------todo todo todo

NoMacro {
	description = "",
	id = "7E57D345-65DD-49F2-AD3D-21A506854B80",
	area = "Editor", key = "F1",
	priority = 50,
	condition = function() return last_co end,
	action = function() far.Show(coroutine.resume(last_co)) end
}

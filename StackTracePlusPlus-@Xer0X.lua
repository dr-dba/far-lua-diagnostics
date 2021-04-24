--[[
if true then return end --]]

--[[	    @Xer0X CopyLeft 2020
	Православие или Смерть! Group

	#############################

	This script is modification,
	and, hopefuly, iprovement,
	of this great utility:
	https://github.com/ignacio/StackTracePlus
	also see here:
	https://forum.farmanager.com/viewtopic.php?f=15&t=9611&p=130485&hilit=stacktrace#p130485

	DEPENDENCIES

	* Lua Explorer "Advanced" +@Xer0X mod
	https://github.com/dr-dba/far-lua-diagnostics
	* General utils for common use
	https://github.com/dr-dba/far-lua-general-utils
]]

--[[ Установка:
See the _macroinit.lua file in the same repository
--]]

require("Lib-Common-@Xer0X")
require("introspection-@Xer0X")
local	req_ok, req_msg, fnc_lua_exp_proc, fnc_hidden_props
				= Xer0X.fnc_safe_require("LuaExplorer-@Xer0X")
if not	req_ok
then	req_ok, req_msg, fnc_lua_exp_proc
				= Xer0X.fnc_safe_require("LE")
end
if	fnc_lua_exp_proc
then	_G.LE = fnc_lua_exp_proc
	_G.le = fnc_lua_exp_proc
end

-- helpers:
local fnc_exec_time_info_read	= Xer0X.fnc_exec_time_info_read
local fnc_norm_script_path	= Xer0X.fnc_norm_script_path

-- tables
local _G = _G
local is_mdl, tbl_args, own_file_path, own_file_fold, own_file_name, own_file_extn
	= Xer0X.fnc_file_whoami(...)

local string, io, debug, coroutine = string, io, debug, coroutine

-- functions
local	tostring, print, require, next, assert, pcall, type, pairs, ipairs, error =
	tostring, print, require, next, assert, pcall, type, pairs, ipairs, error

assert(debug, "debug table must be available at this point")

local print = function() end
local io_open = io.open
local string_gmatch = string.gmatch
local string_sub = string.sub
local table_concat = table.concat

-- controls the maximum length of the 'stringified' table before cutting with ' (more...)'
local _M = {
	max_tbl_output_len = 100,
	max_str_output_len = 100
}

-- this tables should be weak so the elements in them won't become uncollectable
local m_syst_known_tables = { [_G] = "_G (global table)" }

local function add_known_module(name, desc)
	local ok, mod = pcall(require, name)
	if ok then m_syst_known_tables[mod] = desc end
end

add_known_module("string",	"string module")
add_known_module("io",		"io module")
add_known_module("os",		"os module")
add_known_module("table",	"table module")
add_known_module("math",	"math module")
add_known_module("package",	"package module")
add_known_module("debug",	"debug module")
add_known_module("coroutine",	"coroutine module")

-- lua5.2
add_known_module("bit32", "bit32 module")
-- luajit
add_known_module("bit", "bit module")
add_known_module("jit", "jit module")
-- lua5.3
if _VERSION >= "Lua 5.3" then
add_known_module("utf8", "utf8 module")
end

-- by @XeRoX:
add_known_module("bit64",	"bit64 module")
add_known_module("utf8",	"utf8 module")
add_known_module("win",		"win module")

-- good for security, hides unnecessary to see username:
local NORMALIZE_PATH = true


local m_user_known_tables =	{ }
local m_syst_known_functions =	{ }
local m_user_known_functions =	{ }

-- Adds a table to the list of known tables
function _M.add_known_table(tab, description)
	if m_syst_known_tables[tab] then
		error("Cannot override an already known table")
	end
	m_user_known_tables[tab] = description
end

-- Adds a function to the list of known functions
function _M.add_known_function(fun, description)
	if	m_syst_known_functions[fun]
	then	error("Cannot override an already known function")
	end
	m_user_known_functions[fun] = description
end

for _, name in ipairs {
	-- Lua 5.2, 5.1
	"assert", "collectgarbage",	"dofile",	"error",	"getmetatable", "ipairs", "load",	"loadfile",
	"next",		"pairs",	"pcall",	"print",	"rawequal",	"rawget", "rawlen",	"rawset",
	"require",	"select",	"setmetatable", "tonumber",	"tostring",	"type"	, "xpcall",
	-- Lua 5.1
	"gcinfo",	"getfenv",		"loadstring",	"module",	"newproxy",	"setfenv",	"unpack",
	-- TODO: add table.* etc functions
		}
do
	if _G[name] then m_syst_known_functions[_G[name]] = name end
end

local function safe_tostring(value)
	local ok, err = pcall(tostring, value)
	if ok then return err else return ("<failed to get printable value>:'%s'"):format(err) end
end

--[[ Private:
Parses a line, looking for possible function definitions (in a very naïve way)
Returns '(anonymous)' if no function name was found in the line
--]]
local function ParseLine(line)
	return Xer0X.fnc_definition_parse(line)
end

--[[ Private:
Tries to guess a function's name when the debug info structure does not have it.
It parses either the file or the string where the function is defined.
Returns '?' if the line where the function is defined is not found. ]]
local function fnc_guess_more_func_info(info)
	local file, err, line_now, line_def
	if	type(info.source) == "string"
	and	info.source:sub(1, 1) == "@"
	then
		file, err = io.open(info.source:sub(2), "r")
		if	file
		then 	if	true
			then	for	ii = 1, info.linedefined
				do	line_def = file:read("*l")
				end
			end
			if	info.currentline
			and(	info.currentline
			>=	info.linedefined
			or	info.currentline == -1)
			then	for	ii = info.linedefined + 1, info.currentline
				do	line_now = file:read("*l")
				end
			end
			file:close()
		else	print("file not found:"..tostring(err))
		end
	elseif	info.linedefined ~= -1
	and	info.currentline ~= -1
	then
		local	line_num = 0
		for	ii_line in string.gmatch(info.source, "([^\n]+)\n-")
		do	line_num = line_num + 1
			if	line_num ==	info.linedefined
			then	line_def =	ii_line
			elseif	line_num ==	info.currentline
			then	line_now =	ii_line
			end
		end
	end
	if not	file
	then	print("file not found:"..tostring(err))
	elseif
	not	line_def
	then	print("line not found")
	end
	return line_def and ParseLine(line_def) or info.name or "?", line_now
end

-- Dumper instances are used to analyze stacks and collect its information.
local Dumper = { }

Dumper.new = function(thread)
	local t = { lines = { } }
	for k, v in pairs(Dumper) do t[k] = v end
	t.dump_same_thread = thread == coroutine.running()
	--[[ if a thread was supplied, bind it to debug.info and debug.get
	we also need to skip this additional level we are introducing in the callstack
	(only if we are running	in the same thread we're inspecting) ]]
	if type(thread) == "thread"
	then
		t.getinfo = function(level, what)
			if t.dump_same_thread and type(level) == "number"
			then level = level + 1
			end
			return debug.getinfo(thread, level, what)
		end
		t.getlocal = function(level, loc)
			if t.dump_same_thread
			then level = level + 1
			end
			return debug.getlocal(thread, level, loc)
		end
	else
		t.getinfo  = debug.getinfo
		t.getlocal = debug.getlocal
	end
	return t
end

-- helpers for collecting strings to be used when assembling the final trace
function Dumper:add(text)
	self.lines[#self.lines + 1] = text
end

function Dumper:add_f(fmt, ...)
	local text = string.format(fmt, ...)
--	text = string.gsub(text, "\n", "\\n")
	self:add(text)
end

function Dumper:concat_lines() return table_concat(self.lines) end

--[[ Private:
Iterates over the local variables of a given function.
@param level The stack level where the function is.]]
function Dumper:DumpLocals(level, level_to_show, tbl_locals, header_kind)
	if type(tbl_locals) ~= "table"
	then	header_kind = tbl_locals
		tbl_locals = { }
	end
	local test_info = self.getinfo(level, "nS")
	if not test_info then return end
	if self.dump_same_thread then level = level + 1 end
	prefix = header_kind and string.format("%s|", level_to_show) or "\t"
	local i = 0
	local var_N, var_V, vararg_done
	while	true
	do	::vars_loop_start::
		i = i + 1
		if	vararg_done
		then	
			var_N, var_V = self.getlocal(level,  i)
		else	var_N, var_V = self.getlocal(level, -i)
			if not	var_N
			or	var_N ~= "(*vararg)"
			then	vararg_done = true
				i = 0
				goto vars_loop_start
			end
			var_N = var_N:gsub("%)$", " "..i..")")
		end
		if not var_N then break end
		tbl_locals[var_N] = var_V or "<NIL>"
		if	type(var_V) == "number"
		then	self:add_f("%s%s = NUM:%g\n", prefix, var_N, var_V)
		elseif	type(var_V) == "boolean"
		then	self:add_f("%s%s = BIN:%s\n", prefix, var_N, tostring(var_V))
		elseif	type(var_V) == "string"
		then	var_V = #var_V < _M.max_tbl_output_len and var_V or
				string.sub(var_V, 1, _M.max_tbl_output_len - 5).."..."
			var_V = string.gsub(var_V, "\r\n", "\n")
			var_V = string.gsub(var_V, "\n", "/N")
			self:add_f("%s%s = STR:%q\n", prefix, var_N, var_V)
		elseif	type(var_V) == "userdata"
		then	self:add_f("%s%s = %s\n", prefix, var_N, safe_tostring(var_V))
		elseif	type(var_V) == "nil"
		then	self:add_f("%s%s = nil\n", prefix, var_N)
		elseif	type(var_V) == "table"
		then	if	m_syst_known_tables[var_V]
			then	self:add_f("%s%s = %s\n", prefix, var_N, m_syst_known_tables[var_V])
			elseif	m_user_known_tables[var_V]
			then	self:add_f("%s%s = %s\n", prefix, var_N, m_user_known_tables[var_V])
			else	local txt = "{"
				local elem
				for k, v in pairs(var_V)
				do	v = safe_tostring(v)
					v = string.gsub(v, "^function: ",  "FNC:")
					v = string.gsub(v, "^table: ", "TBL:")
					if	#txt + #v > _M.max_tbl_output_len
					then	txt = txt.." (more..)"
						break
					else	txt = txt..safe_tostring(k)..":"..v
					end
					if next(var_V, k) then txt = txt.."," end
				end
				self:add_f("%s%s = %s%s\n", prefix, var_N, string.gsub(safe_tostring(var_V), "^table: ", "TBL:"), txt.."}")
			end
		elseif	type(var_V) == "function"
		then	local	info = self.getinfo(var_V, "nS")
			local	fnc_name = info.name or m_syst_known_functions[var_V] or m_user_known_functions[var_V]
			if	info.what == "C"
			then	self:add_f("%s%s = C %s\n", prefix, var_N, string.gsub(fnc_name and "function:"..fnc_name or tostring(var_V), "^function: ",  "FNC:"))
			else	local	source = info.short_src
				--[[
				if	source:sub(2, 7) == "string"
				then	source = source:sub(9)	-- uno más, por el espacio que viene (string "Baragent.Main", por ejemplo)
				end --]]
			--	for k, v in pairs(info) do print(k, v) end
				fnc_name = fnc_name or fnc_guess_more_func_info(info)
				self:add_f("%s%s = func '%s' def at %d-%d of chunk <%s>\n", prefix, var_N, fnc_name, info.linedefined, info.lastlinedefined, source)
			end
		elseif	type(var_V) == "thread"
		then	self:add_f("%sthread %q = %s\n", prefix, var_N, tostring(var_V))
		end
	end
	return tbl_locals
end -- DumpLocals

local REM_OWN_LEV = true

local function fnc_menu_obj_inf_key_weight(menu_item_key)
	if	menu_item_key:match("^CALLER:")
	then	return 1
	elseif	menu_item_key:match("^INFUNC:")
	then	return 2
	elseif	menu_item_key:match("^UPVALS:")
	then	return 3
	elseif	menu_item_key:match("^PARAMS:")
	then	return 4
	elseif	menu_item_key:match("^VARARG:")
	then	return 5
	elseif	menu_item_key:match("^LOCALS:")
	then	return 6
	elseif	menu_item_key:match("^LEVELS:")
	then	return 7
	elseif	menu_item_key:match("^TRACE_MSG$")
	then	return 8
	elseif	menu_item_key:match("^inf$")
	then	return 9
	end
end

function stacktrace_X(src_thr, orig_err_msg_file, orig_err_msg_line, orig_err_msg_text, orig_err_msg, p1, p2, p3)
	local tbl_lev_info = { }
	local tbl_lev_path = { }
	local level = -1 -- is_same and 0 or -1
	local own_lev_max = 0
	local is_err_msg, err_lev
	local tbl_params = {
		mode_upvals = 2,
		mode_params = 2,
		mode_vararg = 2,
		mode_locals = 2,
		orig_err_msg_file = orig_err_msg_file,
		orig_err_msg_line = orig_err_msg_line,
		orig_err_msg_text = orig_err_msg_text,
	}
	while true
	do	level = level + 1
		local src, src_inf, mod, upv, arg, var, lcl, inf
			= fnc_exec_time_info_read(src_thr, level, tbl_params)
		if not inf then break end
		inf._LEX_DISPLAY_NAME = "INF" inf._LEX_DISPNAME_SEP = ">"
		upv._LEX_DISPLAY_NAME = "UPV" upv._LEX_DISPNAME_SEP = ">"
		arg._LEX_DISPLAY_NAME = "ARG" var._LEX_DISPNAME_SEP = ">"
		var._LEX_DISPLAY_NAME = "VRG" var._LEX_DISPNAME_SEP = ">"
		lcl._LEX_DISPLAY_NAME = "LCL" lcl._LEX_DISPNAME_SEP = ">"
		local lev_inf = {
			_LEX_LEV_STK_FUNC = inf.name,
			_LEX_LEV_STK_WHAT = inf.what,
			_LEX_DISPLAY_NAME = inf.name == "?" and inf.what or inf.name,
			_LEX_DISPNAME_SEP = "|",
			_LEX_FNC_KEY_WEIGHT = fnc_menu_obj_inf_key_weight,
			["INFUNC:"..inf.what.."/"..inf.name] = inf,
			["UPVALS:"..inf.nups]	=  inf.nups	> 0 and upv or nil,
			["PARAMS:"..inf.nparams] = inf.nparams	> 0 and arg or nil,
			["VARARG:"..#var._LEX_VARARG_VALS] = #var._LEX_VARARG_VALS > 0 and var or nil,
			["LOCALS:".. lcl._LEX_VAR_MAP.cnt] =  lcl._LEX_VAR_MAP.cnt > 0 and lcl or nil,
			inf = inf.namewhat ~= "" and inf.namewhat or inf.short_src
		}
		if	orig_err_msg_file
		and	orig_err_msg_file == inf.short_src
		and	orig_err_msg_line
		and	orig_err_msg_line == inf.currentline
		and	orig_err_msg_text
		then	err_lev = level
			is_err_msg = true
			inf.ERROR = orig_err_msg_text
			lev_inf.ERROR_TEXT = orig_err_msg_text
			lev_inf.ERROR_FILE = orig_err_msg_file
			lev_inf.ERROR_LINE = orig_err_msg_line
		end
		if	string.find(inf.source, own_file_path, 2, true)
		then	own_lev_max = level
		end
		if	#tbl_lev_info > 0
		then	tbl_lev_info[#tbl_lev_info]["CALLER:"..inf.name] = lev_inf
		end
		tbl_lev_info[#tbl_lev_info + 1] = lev_inf
	end
	if	REM_OWN_LEV
	and	own_lev_max
	and(not is_err_msg
	or	own_lev_max < err_lev)
	and	own_lev_max < #tbl_lev_info
	then
		for ii = 1, own_lev_max + 1
		do table.remove(tbl_lev_info, 1)
		end
		own_lev_max = 0
	end
	if not	is_err_msg
	and	#tbl_lev_info > own_lev_max
	then	tbl_lev_info[own_lev_max + 1].TRACE_MSG = orig_err_msg
	end
	for	ii = 1, #tbl_lev_info
	do	tbl_lev_info[ii]["LEVELS:"..ii.."/"..#tbl_lev_info] = ii
	end
	return	tbl_lev_info[1], tbl_lev_path, tbl_lev_info
end -- of stacktrace_X

--[[ Public:
Collects a detailed stack trace, dumping locals, resolving function names when they're not available, etc.
This function is suitable to be used as an error handler with pcall or xpcall

@param thread	an optional thread whose stack is to be inspected (defaul is the current thread)
@param message	an optional error string or object.
@param level	an optional number telling at which level to start the traceback (default is 1)

Returns a string with the stack trace and a string with the original error.
--]]
function _M.fnc_stack_trace(thread, message, level, header_kind, outer_only, p6, p7, p8, p9, p10, p11, p12)
	local is_same_thr = -1
	local thr_curr = coroutine.running()
	if type(thread) == "thread"
	then	is_same_thr = thread == thr_curr and 2 or 0
	else 	is_same_thr = 1
		-- shift parameters left
		thread, message, level, header_kind,	outer_only,		p6, p7 =
		thr_curr,thread, message, level,	header_kind,	outer_only, p6
	end
	if type(level) == "string"
	then    if	level == "no_header"
		then	header_kind = 2
		else	header_kind = nil
		end
		level = nil
	end
	local	header_text
	if	header_kind == nil
	then	header_text = [[
Stack traceback
===============
]]	elseif	header_kind == 1
	then	header_text = [[
===============
]]	elseif	header_kind == 2
	then	header_text = nil
	end
	local	colon = header_kind and "$" or ":"
	level = level or 0
	local	dumper = Dumper.new(thread)
	if	header_text
	then	dumper:add(header_text)
	end
	if	type(message) == "table"
	then	dumper:add("an error object {\n")
		local	first = true
		for	k, v in pairs(message)
		do	if	first
			then	dumper:add(" ")
				first = false
			else    dumper:add(",\n")
			end
			dumper:add(string.gsub(safe_tostring(k), "%.lua:", ".lua"..colon))
			dumper:add(colon.." ")
			dumper:add(string.gsub(safe_tostring(v), "%.lua:", ".lua"..colon))
		end
		dumper:add("\n}")
	elseif
		type(message) == "string"
	then
		if not	header_kind
		then	dumper:add(string.gsub(message, "%.lua:", "%.lua"..colon))
			dumper:add("\n")
		else	-- do nothing
		end
	end
	local 	orig_err_msg_file,
		orig_err_msg_line,
		orig_err_msg_text
			= string.match(message or "", "^(.+):(%d+): (.+)$")
	if dumper.dump_same_thread then level = level + 1 end
	local	tbl_level_locals = { }
	local	level_to_show = 0
	while	true
	do
		local	source, src_test, src_is_file, what_prev, level_to_show_str,
			src_file_path_orig, src_file_path, src_file_type, src_func_name,
			src_func_name_guess, src_curr_line_guess
		local	info = dumper.getinfo(level, "nSlfu")
		if not	info then break end
		if	level == 0
		and	type(thread) == "thread"
		and not	dumper.dump_same_thread
		and	tonumber(orig_err_msg_line) >= 0
		and	info.linedefined	>= 0
		and	info.lastlinedefined	>= 0
		and	info.currentline	==-1
		and	info.what ~= "C"
		then	info.currentline = tonumber(orig_err_msg_line)
		end
		source = info.short_src
		src_test = info.source
		src_file_type = src_test and src_test:sub(1, 1)
		src_is_file = (
			src_file_type == "@" or
			src_file_type == "#"
				)
		if	src_is_file
		then	src_file_path_orig = src_test:sub(2)
			src_file_path = NORMALIZE_PATH and fnc_norm_script_path(src_file_path_orig)
				or src_file_path_orig
		else	src_file_path = src_file_path_orig
		end
		level_to_show		= level_to_show + 1
		level_to_show_str	= level_to_show == 1
			and "===ADDITIONAL=== "
			or string.format("%d#==", level_to_show)
		src_func_name = (
			info.what == "C" or
			info.what == "Lua"
				) and (
			m_user_known_functions[info.func] or
			m_syst_known_functions[info.func] or
			info.name
				)
		src_func_name_guess,
		src_curr_line_guess
			= fnc_guess_more_func_info(info)
		src_curr_line_guess = src_curr_line_guess and string.gsub(Xer0X.fnc_str_trim1(src_curr_line_guess), "\t", " ") or nil
		if
			info.what == "main"
		then
			if	src_file_type == "@"
			then	dumper:add_f("%smain chunk of <%s> at %d\n"		, level_to_show_str,	src_file_path,	info.currentline)
			else	dumper:add_f("%smain chunk of <%s> at %d\n"		, level_to_show_str,	info.short_src,	info.currentline)
			end
			if	src_curr_line_guess
			then	dumper:add_f("%s#%d:%s\n"				, level_to_show,	info.currentline, src_curr_line_guess)
			end
		elseif
			info.what == "C"
		then
			dumper:add_f("%s%s C func '%s'\n"				, level_to_show_str,	info.namewhat, src_func_name or tostring(info.func))
		--	dumper:add_f("%s%s = C %s\n", prefix, name, (m_syst_known_functions[value] and ("function:"..m_syst_known_functions[value]) or tostring(value)))
		elseif
			info.what == "tail" -- print("tail")
		then
			dumper:add_f("%stail call\n"					, level_to_show_str)
			tbl_level_locals[#tbl_level_locals + 1] = dumper:DumpLocals(level,level_to_show, header_kind)
		elseif
			info.what == "Lua"
		then	
			local	was_fnc_guess = false
			if not	src_func_name
			or	src_func_name == "?"
			then	src_func_name = src_func_name_guess
				was_fnc_guess = true
			end
			-- test if we have a file name
			local	func_type = info.namewhat == "" and "FNC" or info.namewhat
			if	src_file_type == "@"
			then	dumper:add_f("%s%s '%s' at <%s%s%d>%s\n"		, level_to_show_str,	func_type,	src_func_name, src_file_path, colon, info.currentline, was_fnc_guess and " (best guess)" or "")
			elseif	src_file_type == '#'
			then	dumper:add_f("%s%s '%s' at template '%s%s%d'%s\n"	, level_to_show_str,	func_type,	src_func_name, src_file_path, colon, info.currentline, was_fnc_guess and " (best guess)" or "")
			else	dumper:add_f("%s%s '%s' at %d of chunk <%s>\n"		, level_to_show_str,	func_type,	src_func_name, info.currentline, source)
			end
			if	src_curr_line_guess
			then	dumper:add_f("%s#%d:%s\n"				, level_to_show,	info.currentline, src_curr_line_guess)
			end
			tbl_level_locals[#tbl_level_locals + 1] =
			dumper:DumpLocals(level						, level_to_show,	header_kind)
			tbl_level_locals[#tbl_level_locals].SHOW_LEVEL = level_to_show
		else	dumper:add_f("%s!unknown frame %s\n"				, level_to_show_str,	info.what)
		end
		level = level + 1
	end
	local tbl_dbg_stack, tbl_dbg_path, tbl_dbg_info
		= stacktrace_X(
			thread,
			orig_err_msg_file,
			orig_err_msg_line and tonumber(orig_err_msg_line) or nil,
			orig_err_msg_text,
			message
				)
	table.insert(tbl_dbg_path, 1, "tbl_dbg_stack")
	local message_new = dumper:concat_lines()
	message_new = string.gsub(message_new, "%.Lua"	, ".lua")
	message_new = string.gsub(message_new, "%.LUA"	, ".lua")
	message_new = string.gsub(message_new, "%.lua:"	, ".lua$")
	message_new = string.gsub(message_new, "  "	, " ")
	message_new = string.gsub(message_new, "\r\n"	, "\n")
	return {
		message		= message,
		message_new	= message_new,
		level		= level,
		tbl_dbg_stack	= tbl_dbg_stack,
		tbl_dbg_path	= tbl_dbg_path,
		tbl_dbg_info	= tbl_dbg_info,
		tbl_lev_locals	= tbl_level_locals,
		thread		= thread,
		thread_is_same	= is_same_thr,
		user_known_tables = m_user_known_tables,
		syst_known_tables = m_syst_known_tables,
		_LEX_DISPNAME_SEP = ":",
		_LEX_FNC_KEY_WEIGHT = function(menu_item_key)
			if	menu_item_key:match("^message$")
			then	return 1
			elseif	menu_item_key:match("^message_new$")
			then	return 2
			elseif	menu_item_key:match("^level$")
			then	return 3
			elseif	menu_item_key:match("^tbl_dbg_path$")
			then	return 4
			elseif	menu_item_key:match("^tbl_dbg_info$")
			then	return 5
			elseif	menu_item_key:match("^tbl_dbg_stack$")
			then	return 6
			elseif	menu_item_key:match("^tbl_lev_locals$")
			then	return 7
			elseif	menu_item_key:match("^user_known_tables$")
			then	return 8
			elseif	menu_item_key:match("^syst_known_tables$")
			then	return 9
			elseif	menu_item_key:match("^thread$")
			then	return 10
			elseif	menu_item_key:match("^thread_is_same$")
			then	return 11
			end
		end
			}, tbl_dbg_path, tbl_dbg_info
end --	_M.fnc_stack_trace()

function _M.stacktrace(...)
	local tbl_info = _M.fnc_stack_trace(...)
	return tbl_info.message
end

local sz_err_dir = win.GetEnv("temp")
local level_up
function _M.traceback(...)
	local	tbl_args = { ... }
	local	is_sync = true
	if	#tbl_args == 0
	then	tbl_args[1] = "<NO-MSG>"
	else    local	arg_last = #tbl_args > 1 and tbl_args[#tbl_args]
		if	type(arg_last) == "table"
		and	arg_last.help_arg
		then	table.remove(tbl_args, #tbl_args)
			if	arg_last.is_sync ~= nil
			then	is_sync = arg_last.is_sync
			end
			if	arg_last.level_up ~= nil
			then	tbl_args[#tbl_args + 1] = arg_last.level_up
			end
		end
	end
	local err_rep_def =
		type(tbl_args[1]) == "thread"
		and	(debug.traceback__orig or debug.traceback)(unpack(tbl_args))
		or	(debug.traceback__orig or debug.traceback)(unpack(tbl_args), 2)
	Xer0X.fnc_file_text_save(sz_err_dir.."\\far_err_rpt_orig.txt", err_rep_def)
	-- "big red message of far error" header usage:
	tbl_args[#tbl_args + 1] = "no_header"
	-- show only outer to stack trace functions:
	tbl_args[#tbl_args + 1] = true
	local tbl_stack, tbl_path, tbl_info = _M.fnc_stack_trace(unpack(tbl_args))
	Xer0X.fnc_file_text_save(sz_err_dir.."\\far_err_rpt_plus.txt", tbl_stack.message_new)
	tbl_stack.tbl_args = tbl_args
        local tbl_hide_vals = fnc_hidden_props and fnc_hidden_props() or nil
	local fnc_launch_lex = function()
		LE(tbl_stack, is_sync and "DIAG" or "ERROR", nil, nil, tbl_stack.tbl_dbg_path, tbl_hide_vals)
	end
	if	is_sync
	then	-- supposed to be user call, show immediatly:
		fnc_launch_lex()
	else	-- supposed to be error callback, need to run in another thread:
		far.Timer(0, function(sender) sender.Enabled = false; sender:Close();
		fnc_launch_lex()
		end)
	end
	return err_rep_def.."\n"..tbl_stack.message_new
end

Xer0X.STP = _M
Xer0X.stp = _M
if	true
then
	if not	debug.traceback__orig
	and	debug.getinfo(debug.traceback, "S").what == "C"
	then	debug.traceback__orig = debug.traceback
	end
	debug.traceback = function(...)
		local tbl_args = { ... }
		tbl_args[#tbl_args + 1] = {
			help_arg= true,
			is_sync	= false,
			level_up= type(tbl_args[1]) == "thread" and 0 or 2
		}
		return _M.traceback(unpack(tbl_args))
	end
end

return _M

-- @@@@@

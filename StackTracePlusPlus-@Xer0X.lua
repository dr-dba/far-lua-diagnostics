--[[
if true then return end --]]

--[[	    @Xer0X CopyLeft 2020
	Православие или Смерть! Group

	#############################

	This script is modification,
	and, hopefuly, iprovement,
	of this great script:
	https://github.com/ignacio/StackTracePlus
	also see here:
	https://forum.farmanager.com/viewtopic.php?f=15&t=9611&p=130485&hilit=stacktrace#p130485

	DEPENDENCIES:

	Lua Explorer Advanced
	http://forum.farmanager.com/viewtopic.php?f=60&t=7988
]]

--[[ Установка:
See the _macroinit.lua file in the same repository
--]]

-- tables
local _G = _G, sz_mdl_own_file
local string, io, debug, coroutine = string, io, debug, coroutine

-- functions
local
	tostring, print, require, next, assert, pcall, type, pairs, ipairs, error =
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

local NORMALIZE_PATH = false

local fnc_source_info_get	= Xer0X.fnc_source_info_get
local fnc_file_whoami		= Xer0X.fnc_file_whoami
local fnc_norm_script_path	= Xer0X.fnc_norm_script_path


if not Xer0X then Xer0X = {} end


local mdl_intro = require("introspection-@Xer0X")

local m_user_known_tables = {}
local m_syst_known_functions = {}
local m_user_known_functions = {}

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
	assert(type(line) == "string")
	local match
	match = line:match("^%s*function%s+(%w+)")
	if match then
		return match
	end
	match = line:match("^%s*local%s+function%s+(%w+)")
	if match then
		return match
	end
	match = line:match("^%s*local%s+(%w+)%s+=%s+function")
	if match then
		return match
	end
	match = line:match("%s*function%s*%(")
	if match then
		return "(anonymous)"
	end
	return "(anonymous)"
end

--[[ Private:
Tries to guess a function's name when the debug info structure does not have it.
It parses either the file or the string where the function is defined.
Returns '?' if the line where the function is defined is not found
--]]
local function fnc_guess_more_func_info(info)
	local file, err, line_now, line_def
	if	type(info.source) == "string" and
		info.source:sub(1, 1) == "@"
	then
		file, err = io.open(info.source:sub(2), "r")
		if	file
		then 	if	true
			then	for	ii = 1, info.linedefined
				do	line_def = file:read("*l")
				end
			end
			if	info.currentline and
				info.currentline >=
				info.linedefined
			then	for	ii = info.linedefined + 1, info.currentline
				do	line_now = file:read("*l")
				end
			end
			file:close()
		else	print("file not found:"..tostring(err))
		end
	else
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
	if	not file
	then	print("file not found:"..tostring(err))
	elseif	not line_def
	then	print("line not found")
	end
	return line_def and ParseLine(line_def) or "?", line_now
end

-- Dumper instances are used to analyze stacks and collect its information.
local Dumper = {}

Dumper.new = function(thread)
	local t = { lines = {} }
	for k, v in pairs(Dumper) do t[k] = v end
	t.dumping_same_thread = thread == coroutine.running()
--[[	if a thread was supplied, bind it to debug.info and debug.get
	we also need to skip this additional level we are introducing in the callstack (only if we are running
	in the same thread we're inspecting) ]]
	if type(thread) == "thread"
	then
		t.getinfo = function(level, what)
			if t.dumping_same_thread and type(level) == "number"
			then level = level + 1
			end
			return debug.getinfo(thread, level, what)
		end
		t.getlocal = function(level, loc)
			if t.dumping_same_thread
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
	if type(tbl_locals) ~= "table" then
		header_kind = tbl_locals
		tbl_locals = {}
	end
	local test_info = self.getinfo(level, "nS")
	if  not test_info then return end
	if self.dumping_same_thread then level = level + 1 end
	prefix = header_kind and string.format("%s|", level_to_show) or "\t"
	local i = 0
	while	true
	do	i = i + 1
		local name, value = self.getlocal(level, i)
		if not name then break end
		tbl_locals[name] = value or "<NIL>"
		if	type(value) == "number"
		then	self:add_f("%s%s = NUM:%g\n", prefix, name, value)
		elseif	type(value) == "boolean"
		then	self:add_f("%s%s = BIN:%s\n", prefix, name, tostring(value))
		elseif	type(value) == "string"
		then	value = #value < _M.max_tbl_output_len and value or
				string.sub(value, 1, _M.max_tbl_output_len - 5).."..."
			value = string.gsub(value, "\r\n", "\n")
			value = string.gsub(value, "\n", "/N")
			self:add_f("%s%s = STR:%q\n", prefix, name, value)
		elseif	type(value) == "userdata"
		then	self:add_f("%s%s = %s\n", prefix, name, safe_tostring(value))
		elseif	type(value) == "nil"
		then	self:add_f("%s%s = nil\n", prefix, name)
		elseif	type(value) == "table"
		then	if	m_syst_known_tables[value]
			then	self:add_f("%s%s = %s\n", prefix, name, m_syst_known_tables[value])
			elseif	m_user_known_tables[value]
			then	self:add_f("%s%s = %s\n", prefix, name, m_user_known_tables[value])
			else	local txt = "{"
				local elem
				for k, v in pairs(value)
				do	v = safe_tostring(v)
					v = string.gsub(v, "^function: ",  "FNC:")
					v = string.gsub(v, "^table: ", "TBL:")
					if	#txt + #v > _M.max_tbl_output_len
					then	txt = txt.." (more..)"
						break
					else	txt = txt..safe_tostring(k)..":"..v
					end
					if next(value, k) then txt = txt.."," end
				end
				self:add_f("%s%s = %s%s\n", prefix, name, string.gsub(safe_tostring(value), "^table: ", "TBL:"), txt.."}")
			end
		elseif	type(value) == "function"
		then	local	info = self.getinfo(value, "nS")
			local	fun_name = info.name or m_syst_known_functions[value] or m_user_known_functions[value]
			if	info.what == "C"
			then	self:add_f("%s%s = C %s\n", prefix, name, string.gsub(fun_name and "function:"..fun_name or tostring(value), "^function: ",  "FNC:"))
			else	local	source = info.short_src
				--[[
				if	source:sub(2, 7) == "string"
				then	source = source:sub(9)	-- uno más, por el espacio que viene (string "Baragent.Main", por ejemplo)
				end --]]
			--	for k, v in pairs(info) do print(k, v) end
				fun_name = fun_name or fnc_guess_more_func_info(info)
				self:add_f("%s%s = func '%s' def at %d-%d of chunk <%s>\n", prefix, name, fun_name, info.linedefined, info.lastlinedefined, source)
			end
		elseif	type(value) == "thread"
		then	self:add_f("%sthread %q = %s\n", prefix, name, tostring(value))
		end
	end
	return tbl_locals
end

function stacktrace_X(src_thr, orig_err_msg_file, orig_err_msg_line, orig_err_msg_text, orig_err_msg)
	local tbl_lev_info = {}
	local tbl_lev_path = {}
	local level = -1 -- is_same and 0 or -1
	local is_outer = not orig_err_msg_file and true or false
	while true
	do	level = level + 1
		local src, src_inf, mod, lcl, upv, inf = fnc_source_info_get(src_thr, level, nil, 2, 2)
		if not inf then break end
		tbl_lev_info[#tbl_lev_info + 1] = {}
		tbl_lev_info[#tbl_lev_info].INF = inf
		tbl_lev_info[#tbl_lev_info].LCL = lcl
		tbl_lev_info[#tbl_lev_info].UPV = upv
		tbl_lev_info[#tbl_lev_info].inf = inf.what..":" and (inf.namewhat ~= "" and inf.namewhat or inf.short_src)
		if	orig_err_msg_file
		and	orig_err_msg_file == inf.short_src
		and	orig_err_msg_line == inf.currentline
		then	is_outer = true
			tbl_lev_info[#tbl_lev_info].ERROR_TEXT = orig_err_msg_text
			tbl_lev_info[#tbl_lev_info].ERROR_FILE = orig_err_msg_file
			tbl_lev_info[#tbl_lev_info].ERROR_LINE = orig_err_msg_line
			tbl_lev_info[#tbl_lev_info].INF.ERROR  = orig_err_msg_text
		end
		if not	is_outer
		then	tbl_lev_path[#tbl_lev_path + 1] = level..">"
		end
		if	#tbl_lev_info > 1
		then	tbl_lev_info[#tbl_lev_info - 1][(level - 1)..">"] = tbl_lev_info[#tbl_lev_info]
		end
	end
	return	tbl_lev_info[1], tbl_lev_path
end

--[[ Public:
Collects a detailed stack trace, dumping locals, resolving function names when they're not available, etc.
This function is suitable to be used as an error handler with pcall or xpcall

@param thread An optional thread whose stack is to be inspected (defaul is the current thread)
@param message An optional error string or object.
@param level An optional number telling at which level to start the traceback (default is 1)

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
	then    local header_type_str = level
		level = nil
		if header_type_str == "no_header"
		then	header_kind = 2
		else	header_kind = nil
		end
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
	local orig_err_msg_file, orig_err_msg_line, orig_err_msg_text = string.match(message or "", "^(.+):(%d+): (.+)$")
	if dumper.dumping_same_thread then level = level + 1 end
	local	tbl_level_locals = {}
	local	level_to_show = 0
	while	true
	do
		local info, source, src_test, src_is_file, what_prev, src_file_path_orig, src_file_path, src_file_type, src_func_name, level_to_show_str, src_func_name_guess, src_curr_line_guess
		info = dumper.getinfo(level, "nSlfu")
		if not info then break end
		source	= info.short_src
		src_test = info.source
		src_file_type = src_test and src_test:sub(1, 1)
		src_is_file = (
			src_file_type == "@" or
			src_file_type == "#"
				)
		src_file_path_orig = src_is_file and src_test:sub(2)
		src_file_path = NORMALIZE_PATH and fnc_norm_script_path(src_file_path_orig)
			or src_file_path_orig

	
		
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
		if
			info.what == "main"
		then
			if	src_file_type == "@"
			then	dumper:add_f("%smain chunk of <%s> at %d\n"		, level_to_show_str,	src_file_path,	info.currentline)
			else	dumper:add_f("%smain chunk of <%s> at %d\n"		, level_to_show_str,	info.short_src,	info.currentline)
			end
			if	src_curr_line_guess
			then	dumper:add_f("%s#%d:%s\n"				, level_to_show,	info.currentline, string.match(src_curr_line_guess, "^%s*(.-)%s*$"))
			end
		elseif
			info.what == "C"
		then
			dumper:add_f("%s%s C func '%s'\n"				, level_to_show_str,	info.namewhat, src_func_name or tostring(info.func))
		--	dumper:add_f("%s%s = C %s\n", prefix, name, (m_syst_known_functions[value] and ("function:"..m_syst_known_functions[value]) or tostring(value)))
		elseif
			info.what == "tail"
		then
			dumper:add_f("%stail call\n"					, level_to_show_str)
			tbl_level_locals[#tbl_level_locals + 1] = dumper:DumpLocals(level,level_to_show, header_kind)
		elseif
			info.what == "Lua"
		then
			if	source:sub(2, 7) == "string"
			then	source = source:sub(9)
			end
			local	was_fnc_guess = false
			if not	src_func_name or
				src_func_name == "?"
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
			then	dumper:add_f("%s#%d:%s\n"				, level_to_show,	info.currentline, string.match(src_curr_line_guess, "^%s*(.-)%s*$"))
			end
			tbl_level_locals[#tbl_level_locals + 1] =
				dumper:DumpLocals(level					, level_to_show,	header_kind)
			tbl_level_locals[#tbl_level_locals].SHOW_LEVEL = level_to_show
		else		dumper:add_f("%s!unknown frame %s\n"			, level_to_show_str,	info.what)
		end
		level = level + 1
	end
	local tbl_dbg_stack, tbl_dbg_path = stacktrace_X(thread, orig_err_msg_file, orig_err_msg_line and tonumber(orig_err_msg_line), orig_err_msg_text, message)
	table.insert(tbl_dbg_path, 1, "tbl_dbg_stack")
	local message_new = dumper:concat_lines()
	message_new = string.gsub(message_new, "%.Lua"	, ".lua")
	message_new = string.gsub(message_new, "%.LUA"	, ".lua")
	message_new = string.gsub(message_new, "%.lua:"	, ".lua$")
	message_new = string.gsub(message_new, "  "	, " ")
	message_new = string.gsub(message_new, "\r\n"	, "\n")
	return {
		thread = thread,
		message = message,
		level = level,
		thread_is_same = is_same_thr,
		message_new = message_new,
		tbl_dbg_stack = tbl_dbg_stack,
		tbl_lev_locals = tbl_level_locals,
		user_known_tables = m_user_known_tables,
		syst_known_tables = m_syst_known_tables
	}, tbl_dbg_path
end --	_M.fnc_stack_trace()

function _M.stacktrace(...)
	local tbl_info = _M.fnc_stack_trace(...)
	return tbl_info.message
end
local is_mdl, tbl_args, own_file_path, own_file_fold, own_file_name, own_file_extn
	= fnc_file_whoami(...)

return _M

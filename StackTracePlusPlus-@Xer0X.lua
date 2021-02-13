--[[		(c) Xer0X
	"Православие или Смерть!" group

	Tools for "introspection",
	aka live code analysis.
]]

if not	Xer0X
then	Xer0X = { }
end

Xer0X.fnc_definition_parse = function(line)
	assert(type(line) == "string")
	local match
	match = line:match("^%s*function%s+(%w+)")
	if match then return match end
	match = line:match("^%s*local%s+function%s+(%w+)")
	if match then return match end
	match = line:match("^%s*local%s+(%w+)%s+=%s+function")
	if match then return match end
	match = line:match("%s*function%s*%(")
	if match then return "(anonymous)" end
	return "(anonymous)"
end

Xer0X.fnc_func_name_guess = function(dbg_info)
	local	line_str, line_def, line_cur, src_code
	local	src_code = ""
	if	type(dbg_info.source) == "string" and dbg_info.source:sub(1, 1) == "@"
	then
		local	fileH, err = io.open(dbg_info.source:sub(2), "r")
		if not	fileH
		then	return "?"
		end
		for	line_num = 1, dbg_info.lastlinedefined
		do 	line_str = fileH:read("*l")
			if not	line_str then break end
			if	line_num == dbg_info.linedefined
			then	line_def = line_str
			end
			if	line_num == dbg_info.currentline
			then	line_cur = line_str
			end
			if	line_num >= dbg_info.linedefined and
				line_num <= dbg_info.lastlinedefined
			then	src_code = src_code..line_str..string.char(10)..string.char(13)
			end
		end
		fileH:close()
	else
		local	line_num = 0
		for	line_str in string.gmatch(dbg_info.source, "([^\n]+)\n-")
		do	line_num = line_num + 1
			if	line_num == dbg_info.linedefined
			then	line_def = line_str
			end
			if	line_num == dbg_info.currentline
			then	line_cur = line_str
			end
			if	line_num >= dbg_info.linedefined and
				line_num <= dbg_info.lastlinedefined
			then	src_code = src_code..line_str..string.char(10)..string.char(13)
			end
		end
	end
	if src_code == "" then src_code = nil end
	return line_def and Xer0X.fnc_definition_parse(line_def) or "?", src_code, line_cur
end

Xer0X.fnc_source_info_get = function(src_thr, src_lev, show_info, mode_locals, mode_upvals, with_nums, tbl_locals, tbl_upvals, tbl_meta, tbl_lcvrid)
	if	type(src_thr) ~= "thread" and
		type(src_thr) ~= "nil"
	then	src_thr, src_lev, show_info, mode_locals, mode_upvals, tbl_locals, tbl_upvals, tbl_meta, tbl_lcvrid = nil,
		src_thr, src_lev, show_info, mode_locals, mode_upvals, tbl_locals, tbl_upvals, tbl_meta, tbl_lcvrid
	end
	local dbg_info, LoadedMacros, mcr_src, mcr_inf
	if not tbl_upvals then tbl_upvals = {} end
	if not tbl_locals then tbl_locals = {} end
	if not tbl_locals._VAR_MAP then tbl_locals._VAR_MAP = { i2n = { }, n2i = { }, raw = { } } end
	local dbg_props = nil
	local is_same_thr = not src_thr or coroutine.running() == src_thr
	if	is_same_thr
	then	dbg_info = debug.getinfo(src_lev, dbg_props)
	else	dbg_info = debug.getinfo(src_thr, src_lev, dbg_props)
	end
	if	dbg_info
	then	local fnc_name, fnc_code, curr_line = Xer0X.fnc_func_name_guess(dbg_info)
		if not	dbg_info.name
		then	dbg_info.name = fnc_name
                end
		dbg_info.currentline_txt = curr_line
		dbg_info.source_code = fnc_code
		mcr_src = dbg_info.source:sub(2)
		mcr_inf = win.GetFileInfo(mcr_src)
		if	mode_upvals and mode_upvals > 0
		then	for	ii = 1, dbg_info.nups
			do	local n, v = debug.getupvalue(dbg_info.func, ii)
				if band(mode_upvals, 2) == 2
				then tbl_upvals[with_nums and "["..ii.."] "..n or n] = v or "<NIL>"
				end
			end
		end
		if	mode_locals and mode_locals > 0
		then	local	ii = 0
			while	true
			do	ii = ii + 1
				local n, v
				if	is_same_thr
				then	n, v = debug.getlocal(src_lev, ii)
				else	n, v = debug.getlocal(src_thr, src_lev, ii)
				end
				v = type(v) == "nil" and "<NIL>" or v
			        if not n then break end
				if band(mode_locals, 2) == 2
				then tbl_locals[with_nums and "["..ii.."] "..n or n] = v
				end
				tbl_locals._VAR_MAP.raw[n] = v
				tbl_locals._VAR_MAP.n2i[n] = ii
				tbl_locals._VAR_MAP.i2n[ii]= n
			end
		end
	elseif	type(src_lev) == "string"
	then	mcr_inf = win.GetFileInfo(src_lev)
		if mcr_inf then mcr_src = src_lev end
	end
	if mcr_src then mcr_src = far.ConvertPath(mcr_src, 1) end
	return mcr_src, mcr_inf, mcr_inf and mcr_inf.LastWriteTime, tbl_locals, tbl_upvals, dbg_info
end


local all_the_stuff = {}
for ii = 0, 1000 do
	local mcr_src, mcr_inf, LastWriteTime, tbl_locals, tbl_upvals, dbg_info = Xer0X.fnc_source_info_get(nil, ii, nil, 2, 2, false)
	if not dbg_info then break end
	all_the_stuff[ii + 1] = { mcr_src = mcr_src, mcr_inf = mcr_inf, LastWriteTime = LastWriteTime, tbl_locals = tbl_locals, tbl_upvals = tbl_upvals, dbg_info = dbg_info }
	if	tbl_upvals.LoadedMacros
	then    Xer0X.utils = tbl_upvals
		Xer0X.utils_local = tbl_locals
	elseif	tbl_upvals.KeyMacro
	then	Xer0X.key_mcr_inf = tbl_upvals
		Xer0X.key_mcr_inf_loc = tbl_locals
	end
end
_G.Xer0X.internals = all_the_stuff

local tbl_macrolist_marks = {}
local tbl_mcr_items_marks = {}
Xer0X.fnc_find_macrolist = function(mark_id)
	mark_id = mark_id and ":"..mark_id or ":?"
	local ii_from, all_the_stuff
	local	tbl_mcr_lst_upv,
		tbl_mcr_lst_loc,
		tbl_mcr_items_U,
		tbl_mcr_items_L
	local tbl_mcr_lst_upv_idx = tbl_macrolist_marks["UPV"..mark_id]
	local tbl_mcr_lst_loc_idx = tbl_macrolist_marks["LOC"..mark_id]
	local tbl_mcr_items_L_idx = tbl_mcr_items_marks["LOC"..mark_id]
	local found =
		tbl_mcr_lst_upv_idx or
		tbl_mcr_lst_loc_idx
	local	var_key, var_val, dbg_inf
	if	tbl_mcr_lst_loc_idx
	then	dbg_inf = debug.getinfo(tbl_mcr_lst_loc_idx - 1)
		if	dbg_inf
		then	var_key, var_val = debug.getlocal(tbl_mcr_lst_loc_idx - 1, Xer0X.env_mcr_lst_loc._VAR_MAP.n2i.macrolist)
		end
		if	var_key == "macrolist"
		then	tbl_mcr_lst_loc = var_val
			found = true
		else	found = false
		end
		if	found
		then	var_key, var_val = debug.getlocal(tbl_mcr_items_L_idx - 1, Xer0X.env_mcr_items_L._VAR_MAP.n2i.menuitems)
			if	var_key == "menuitems"
			then	tbl_mcr_items_L = var_val
				goto end_of_proc
			else	found = false
			end
		end
	end
	ii_from = found and (tbl_mcr_lst_loc_idx or tbl_mcr_lst_upv_idx) or 1
	all_the_stuff = {}
	for ii = ii_from, 1000 do
		local mcr_src, mcr_inf, LastWriteTime, tbl_locals, tbl_upvals, dbg_inf
			= Xer0X.fnc_source_info_get(nil, ii, nil, 2, 2, false)
		if not dbg_inf then break end
		all_the_stuff[ii] = { mcr_src = mcr_src, mcr_inf = mcr_inf, LastWriteTime = LastWriteTime, tbl_locals = tbl_locals, tbl_upvals = tbl_upvals, dbg_inf = dbg_inf }
		if	tbl_upvals.macrolist
		then
			Xer0X.env_mcr_lst_upv = tbl_upvals
			Xer0X.env_mcr_lst_upv.LEVEL = ii
			Xer0X.env_mcr_lst_upv_loc = tbl_locals
			Xer0X.env_mcr_lst_upv_idx = ii
			tbl_mcr_lst_upv = tbl_upvals.macrolist
			tbl_macrolist_marks["UPV"..mark_id] = ii
			found = true
		end
		if	tbl_locals.macrolist
		then
			Xer0X.env_mcr_lst_loc = tbl_locals
			Xer0X.env_mcr_lst_loc_upv = tbl_upvalues
			Xer0X.env_mcr_lst_loc.LEVEL = ii
			Xer0X.env_mcr_lst_loc_idx = ii
			tbl_mcr_lst_loc = tbl_locals.macrolist
			tbl_macrolist_marks["LOC"..mark_id] = ii
			found = true
		end
		if	tbl_locals.menuitems
		then
			Xer0X.env_mcr_items_L = tbl_locals
			Xer0X.env_mcr_items_L_upv = tbl_upvalues
			Xer0X.env_mcr_items_L.LEVEL = ii
			Xer0X.env_mcr_items_L_idx = ii
			tbl_mcr_items_L = tbl_locals.menuitems
			tbl_mcr_items_marks["LOC"..mark_id] = ii
		end
	end
	if	found
	then	Xer0X.env_mcr_lst_full_stack = all_the_stuff
	end
	::end_of_proc::
	return found, tbl_mcr_lst_upv, tbl_mcr_lst_loc, tbl_mcr_items_L
end

Xer0X.fnc_mcr_src_tbl_clean = function(mcr_src, tbl_mcr)
	mcr_src = string.lower(mcr_src)
	local tbl_rebind_guids = Xer0X.ReBind and Xer0X.ReBind.GUIDs
	local mcr_cnt = #tbl_mcr
	local mcr_rem = 0
	local ii_mcr, ii_skip, ii_guid
	for	ii = 1, mcr_cnt
	do	ii_mcr = tbl_mcr[ii]
		if  ii_mcr and type(ii_mcr) == "table"
		then
			ii_skip = 0
			if
				ii_mcr.FileName
			and	mcr_src == string.lower(ii_mcr.FileName)
			then	mcr_rem = mcr_rem + 1
				if	tbl_rebind_guids
				then	tbl_rebind_guids[ii_mcr.id or ii_mcr.guid] = nil
				end
				ii_skip = 1
			end
			if	mcr_rem > 0
			then	tbl_mcr[ii - mcr_rem + ii_skip] = tbl_mcr[ii]
				tbl_mcr[ii] = nil
			end
		end
	end
	for	ii, ii_mcr in pairs(tbl_mcr)
	do	if	type(ii_mcr) == "table"
		and	mcr_src == string.lower(ii_mcr.FileName or "<NO-FILE-SOURCE-FOR-THIS-MACRO>")
		then	if	tbl_rebind_guids
			then	ii_guid = tbl_mcr[ii].id or tbl_mcr[ii].guid
				if	ii_guid
				and	tbl_rebind_guids[ii_guid]
				then	tbl_rebind_guids[ii_guid] = nil
				end
			end
			tbl_mcr[ii] = nil
		end
	end
end

Xer0X.fnc_mcr_src_agg_clean = function(mcr_src, tbl_agg)
	mcr_src = string.lower(mcr_src)
	for	ii_one, tbl_one in pairs(tbl_agg)
	do	for	ii_mcr, tbl_mcr in pairs(tbl_one)
		do      if tbl_mcr.FileName
			then	Xer0X.fnc_mcr_src_tbl_clean(mcr_src, tbl_one)
				break
			else 	Xer0X.fnc_mcr_src_tbl_clean(mcr_src, tbl_mcr)
			end
		end
		for	ii_mcr = 1, #tbl_one
		do      if not tbl_one[ii_mcr].FileName then
				Xer0X.fnc_mcr_src_tbl_clean(mcr_src, tbl_one[ii_mcr])
			end
		end
	end
	for	ii_agg = 1, #tbl_agg
	do	for	ii_mcr, tbl_mcr in pairs(tbl_agg[ii_agg])
		do      if tbl_mcr.FileName
			then	Xer0X.fnc_mcr_src_tbl_clean(mcr_src, tbl_agg[ii_agg])
				break
			else	Xer0X.fnc_mcr_src_tbl_clean(mcr_src, tbl_mcr)
			end
		end
		for	ii_mcr = 1, #tbl_agg[ii_agg]
		do      if not tbl_agg[ii_agg][ii_mcr].FileName then
				Xer0X.fnc_mcr_src_tbl_clean(mcr_src, tbl_agg[ii_agg][ii_mcr])
			end
		end
	end
end

Xer0X.fnc_mcr_src_fnc_clean__v0 = function(mcr_src, tbl_fnc)
	mcr_src = string.lower(mcr_src)
	local fnc_scr_path, cc_to_del
	local rem_cnt = 0
	for	ii_cc = 1, #tbl_fnc
	do      for	ii_ccf, ccf in pairs(tbl_fnc[ii_cc - rem_cnt])
		do	if type(ccf) == "function" then
				fnc_scr_path = string.match(debug.getinfo(ccf, "S").source, "^@(.+)")
				if mcr_src == string.lower(fnc_scr_path)
				then	tbl_fnc[ii_cc - rem_cnt][ii_ccf] = nil
				end
			end
		end
		if	next(tbl_fnc[ii_cc - rem_cnt]) == nil
		then	table.remove(tbl_fnc, ii_cc - rem_cnt)
			rem_cnt = rem_cnt + 1
		end
	end
end

Xer0X.fnc_mcr_src_fnc_clean = function(mcr_src, tbl_fnc)
	mcr_src = string.lower(mcr_src)
	local fnc_scr_path
	local rem_cnt = 0
	for	ii_cc = 1, #tbl_fnc - rem_cnt
	do      for	ii_ccf, ccf in pairs(tbl_fnc[ii_cc - rem_cnt])
		do	if type(ccf) == "function" then
				fnc_scr_path = string.match(debug.getinfo(ccf, "S").source, "^@(.+)")
				if mcr_src == string.lower(fnc_scr_path)
				then	table.remove(tbl_fnc, ii_cc - rem_cnt)
					rem_cnt = rem_cnt + 1
					break
				end
			end
		end
	end
end

Xer0X.fnc_mcr_src_all_clean = function(mcr_src, tbl_what)
	if not tbl_what or tbl_what.macro then Xer0X.fnc_mcr_src_tbl_clean(mcr_src, Xer0X.utils.LoadedMacros)	end
	if not tbl_what or tbl_what.pnlmd then Xer0X.fnc_mcr_src_tbl_clean(mcr_src, Xer0X.utils.LoadedPanelModules)end
	if not tbl_what or tbl_what.cmdln then Xer0X.fnc_mcr_src_tbl_clean(mcr_src, Xer0X.utils.AddedPrefixes)	end
	if not tbl_what or tbl_what.mnitm then Xer0X.fnc_mcr_src_tbl_clean(mcr_src, Xer0X.utils.AddedMenuItems)	end
	if not tbl_what or tbl_what.macro then Xer0X.fnc_mcr_src_agg_clean(mcr_src, Xer0X.utils.Areas)          end
	if not tbl_what or tbl_what.event then Xer0X.fnc_mcr_src_agg_clean(mcr_src, Xer0X.utils.Events)		end
	if not tbl_what or tbl_what.cncol then Xer0X.fnc_mcr_src_fnc_clean(mcr_src, Xer0X.utils.ContentColumns) end
end

Xer0X.fnc_macro_one_load = function(mcr_info, mcr_path)
	mcr_path = far.ConvertPath(mcr_path, 1)
	Xer0X.fnc_mcr_src_all_clean(mcr_path)
	collectgarbage("collect")
	local	fnc, msg = loadfile(mcr_path)
	if not	fnc
	then	far.Message(msg, "Error loading macro file", nil, "w") return
	end
	local tbl_env_mcr_ini = {
		Macro		= function(srctable) Xer0X.utils.AddRegularMacro(	srctable, mcr_path) end,
		Event		= function(srctable) Xer0X.utils.AddEvent(		srctable, mcr_path) end,
		MenuItem	= function(srctable) Xer0X.utils.AddMenuItem(		srctable, mcr_path) end,
	        CommandLine	= function(srctable) Xer0X.utils.AddPrefixes(		srctable, mcr_path) end,
		PanelModule	= function(srctable) Xer0X.utils.AddPanelModule(	srctable, mcr_path) end,
		ContentColumns	= function(srctable) Xer0X.utils.AddContentColumns(	srctable, mcr_path) end,
		LoadRegularFile	= Xer0X.utils_local.LoadRegularFile
			}
	local fnc_dummy = function() end
	local tbl_no_fnc_list = { "NoMacro", "NoEvent", "NoMenuItem", "NoCommandLine", "NoPanelModule", "NoContentColumns" }
	local mt_g = { __index = _G }
	for _, fnc_name in ipairs(tbl_no_fnc_list) do tbl_env_mcr_ini[fnc_name] = fnc_dummy; end
	setmetatable(tbl_env_mcr_ini, mt_g)
	setfenv(fnc, tbl_env_mcr_ini)
	local	ret, msg = pcall(fnc, mcr_path)
	if not	ret
	then	far.Message(msg, "Error loading macro func (introspection-@Xer0X)", nil, "w")
	end
end

Xer0X.fnc_macro_dir_load = function(src_dir)
	far.RecursiveSearch(
		src_dir ,
		"*.lua",
		Xer0X.fnc_macro_one_load,
		0
			)
end

-- @@@@@ End of the trick with ad-hoc loading macros  @@@@@ ?

Xer0X.fnc_file_whoami = function(inp_args, from_level, details)
	local	dbg_info = debug.getinfo(from_level or 2, details or "S")
	local	own_file_path, own_file_fold, own_file_name, own_file_extn
	if 	dbg_info.source
	then	if	string.match(dbg_info.short_src, "^%[.*%]$")
		then	own_file_path = dbg_info.short_src
		else	own_file_path = dbg_info.source:match("^@(.+)")
			own_file_fold, own_file_name, own_file_extn = string.match(own_file_path, "(.-)([^/\\]+)([.][^.]+)$")
		end
	end
	local	own_mdl_head, own_mdl_tail, as_module
	local	the_mdl_name = type(inp_args) == "table" and #inp_args > 0 and inp_args[1] or inp_args
	if	the_mdl_name
	and	type(the_mdl_name) == "string"
	then	own_mdl_head = the_mdl_name:match("^(.*)%.")
		own_mdl_tail = the_mdl_name:match("[.](.*)$")
		as_module = (
			the_mdl_name == "load_as_module" or
			the_mdl_name == own_file_name or
			own_mdl_head and
			the_mdl_name:match("%.("..own_file_name..")$") == own_file_name and
			own_file_fold:match("\\("..own_mdl_head..")\\$")==own_mdl_head
				)
	end
	return as_module, inp_args, own_file_path, own_file_fold, own_file_name, own_file_extn, dbg_info.short_src, dbg_info.name, dbg_info.currentline, dbg_info
end

Xer0X.fnc_mcr_src_reload = function(mcr_src, mcr_src_inf_mod, force)
	local dt_inf_new = win.GetFileInfo(mcr_src)
	if not force and dt_inf_new.LastWriteTime == mcr_src_inf_mod then return end
	Xer0X.fnc_load_macro_one(nil, mcr_src)
	Xer0X.fnc_trans_msg("\n"..Xer0X.fnc_norm_script_path(mcr_src).."\n", "Macro reloaded, please rerun the action", "w", "pers")
	return true
end

return {
	fnc_mcr_src_agg_clean	= fnc_mcr_src_agg_clean,
	fnc_mcr_src_all_clean	= fnc_mcr_src_all_clean,
	fnc_mcr_src_fnc_clean	= fnc_mcr_src_fnc_clean,
	fnc_mcr_src_tbl_clean	= fnc_mcr_src_tbl_clean,
	fnc_source_info_get	= fnc_source_info_get,
	fnc_func_name_guess	= fnc_func_name_guess,
	fnc_macro_dir_load	= fnc_macro_dir_load,
	fnc_macro_one_load	= fnc_macro_one_load,
	fnc_definition_parse	= fnc_definition_parse,
}

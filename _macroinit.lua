require("Lib-Common-@Xer0X")
local ok, err_msg, le = Xer0X.fnc_safe_require("LuaExplorer-@Xer0X")
if ok then _G.LE = le; _G.le = le; end
if not	debug.traceback__orig
and	debug.getinfo(debug.traceback, "S").what == "C"
then	debug.traceback__orig = debug.traceback
end
local mdl_intro = require("introspection-@Xer0X")
local ok, err_msg, stp = Xer0X.fnc_safe_require("StackTracePlusPlus-@Xer0X")
Xer0X.STP = stp
local sz_err_dir = win.GetEnv("temp")
debug.traceback = function(...)
	require("zbs.init-far").fnc_attach()
	local err_rep_1, err_rep_2, err_rep_3, err_rep_4, err_rep_5 = debug.traceback__orig(...)
	Xer0X.fnc_file_text_save(sz_err_dir.."\\far_err_rpt_orig.txt", err_rep_1)
	local tbl_args = { ... }
	tbl_args[#tbl_args + 1] = 1
	tbl_args[#tbl_args + 1] = "no_header"
	tbl_args[#tbl_args + 1] = true		
	local tbl_stack, tbl_path = stp.fnc_stack_trace(unpack(tbl_args))
	Xer0X.fnc_file_text_save(sz_err_dir.."\\far_err_rpt_plus.txt", tbl_stack.message_new)
	far.Timer(1, function(sender)
		sender.Enabled = false; sender:Close();
		LE(tbl_stack, "EXEC ", nil, nil, tbl_path)
	end)
	return err_rep_1.."\n"..tbl_stack.message_new
end


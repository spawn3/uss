-module(edog_trace).
-compile(export_all).

-define(MANAGER_TRACE_PORT, 9502).
-define(AGENT_TRACE_PORT, 9503).

trace_manager() ->
    case edog_conf:debug_level() of
        trace ->
            cclib_dbg:start(?MANAGER_TRACE_PORT),
            %cclib_dbg:tpl(edog_master),
            %cclib_dbg:tpl(edog_mensia),
            cclib_dbg:tpl(edog_select),
            %cclib_dbg:tpl(edog_load),
            %cclib_dbg:tpl(edog_vm),
            %cclib_dbg:tpl(edog_libvirt),
            %cclib_dbg:tpl(edog_iscsi),
            ok;
        _ ->
            ok
    end.

trace_agent() ->
    case edog_conf:debug_level() of
        trace ->
            cclib_dbg:start(?AGENT_TRACE_PORT),
            %cclib_dbg:tpl(edog_master),
            %cclib_dbg:tpl(edog_mensia),
            cclib_dbg:tpl(edog_slaves),
            cclib_dbg:tpl(edog_libvirt),
            cclib_dbg:tpl(edog_iscsi);
        _ ->
            ok
    end.

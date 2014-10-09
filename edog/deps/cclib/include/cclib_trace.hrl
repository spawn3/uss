-ifndef(__CCLIB_TRACE_HRL__).
-define(__CCLIB_TRACE_HRL__, true).

-ifdef(use_trace).
-define(start_trace(Port), cclib_dbg:start(Port)).
-else.
-define(start_trace(Port), false).
-endif.

%%%%%%%%%%%%
% END
-endif.

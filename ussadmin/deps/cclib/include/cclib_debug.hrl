-ifndef(__CCLIB_DEBUG_HRL__).
-define(__CCLIB_DEBUG_HRL__, true).

-define(tty_report(Tip, X),
    io:format("*~s* {~p,~p,~p}: ~p~n", [Tip, node(), ?MODULE, ?LINE, X])
).

%% info_report|warning_report|error_report
-define(std_report(ReportType, X),
    error_logger:ReportType([{where, {node(), ?MODULE, ?LINE}}, {reason, X}])
).

-ifdef(debug).

-define(RPC_REPORT(X),   begin ?std_report(info_report, X) end).
-define(INFO_REPORT(X),  begin ?std_report(info_report, X) end).
-define(WARN_REPORT(X),  begin ?std_report(warning_report, X) end).
-define(ERROR_REPORT(X), begin ?std_report(error_report, X) end).
-define(TTY_REPORT(X),   begin ?tty_report("TTY", X) end).
-define(REPLY(X),        begin ?tty_report("REPLY", X), X end).

-else.

-define(RPC_REPORT(X),   true).
-define(INFO_REPORT(X),  true).
-define(WARN_REPORT(X),  true).
-define(ERROR_REPORT(X), true).
-define(TTY_REPORT(X),   true).
-define(REPLY(X),        X).

-endif.

-define(TTY_INFO_REPORT(X),
    begin
        ?tty_report("TTY", X),
        ?std_report(info_report, X)
    end).
-define(TTY_WARN_REPORT(X),
    begin
        ?tty_report("WARN", X),
        ?std_report(warning_report, X)
    end).
-define(TTY_ERROR_REPORT(X),
    begin
        ?tty_report("ERROR", X),
        ?std_report(error_report, X)
    end).

-define(os_cmd(Cmd),
    begin
        ?INFO_REPORT([cmd, iolist_to_binary(lists:flatten(Cmd))]),
        _Result = iolist_to_binary(os:cmd(lists:flatten(Cmd))),
        ?INFO_REPORT([result, iolist_to_binary(_Result)]),
        _Result
    end).

-define(os_cmd_no_report(Cmd),
    begin
        ?INFO_REPORT([cmd, iolist_to_binary(lists:flatten(Cmd))]),
        iolist_to_binary(os:cmd(lists:flatten(Cmd)))
    end).

-endif.

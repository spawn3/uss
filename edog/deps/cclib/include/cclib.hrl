-ifndef(__CCLIB_HRL__).
-define(__CCLIB_HRL__, true).

-include_lib("eunit/include/eunit.hrl").
-include("cclib_log.hrl").
-include("cclib_init.hrl").
-include("cclib_const.hrl").
-include("cclib_utils.hrl").
-include("cclib_trace.hrl").
-include("cclib_network.hrl").
-include("cclib_rpc.hrl").
-include("cclib_db.hrl").
-include("cclib_json.hrl").
-include("cclib_profile.hrl").

%% ------------------------------------------------------
-record(callback_cb, {
        from,
        context,
        result
    }).

-record(async_event, {
        catagory = 'user',
        level,
        event,
        time,
        info
    }).

-define(ALERT(Catagory, Level, Event, Info),
    ?EVENT(#async_event{
            catagory=Catagory,
            event=Event,
            level=Level,
            time=cclib_utils:now_to_string(),
            info=cclib_utils:to_binary(Info)})
    ).
-define(ALERT_INFO(Catagory, Event, Info), ?ALERT(Catagory, info, Event, Info)).
-define(ALERT_WARN(Catagory, Event, Info), ?ALERT(Catagory, warn, Event, Info)).
-define(ALERT_ERR (Catagory, Event, Info), ?ALERT(Catagory, error, Event, Info)).

-define(SEND_TRAP(Type, N, Event, OldStatus, NewStatus, Info),
    edog_master:notify({{?MODULE, ?LINE}, #async_event{
            catagory=user,
            level='info',
            event=Event,
            time=cclib_utils:now_to_string(),
            info=io_lib:format("[~p/~B@~s]: ~p", [Type, N, cclib_utils:get_ip(node()), NewStatus])
        }})).

-define(IF_COND(Cond, Value, Default),
    if
        Cond -> Value;
        true -> Default
    end).

-define(EXECUTE(Cmd),
    begin
        Cmd1 = lists:flatten(Cmd),
        ?INFO([cmd, iolist_to_binary(Cmd1)]),
        _Result = os:cmd(Cmd1),
        ?INFO([result, iolist_to_binary(lists:flatten(_Result))]),
        _Result
    end).

-define(os_cmd(Cmd),
    begin
        ?INFO([cmd, iolist_to_binary(lists:flatten(Cmd))]),
        _Result = iolist_to_binary(os:cmd(lists:flatten(Cmd))),
        ?INFO([result, iolist_to_binary(_Result)]),
        _Result
    end).

-define(os_cmd_no_report(Cmd),
    begin
        ?INFO([cmd, iolist_to_binary(lists:flatten(Cmd))]),
        iolist_to_binary(os:cmd(lists:flatten(Cmd)))
    end).

-define(BYTE_TO_KB(Bytes), ((Bytes) div 1024)).

-define(REPLY(X), X).

-record(cmd_info, {
        code       = 0,
        timeout    = 10000,
        is_timeout = false,
        data       = []
    }).

%%%%%%%%%%%%
% END
-endif.

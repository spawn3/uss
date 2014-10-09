-module(uss_app).
-export([start/2, stop/1]).
-compile(export_all).

-behaviour(application).

-include("uss_common.hrl").

prepare() ->
    try
        {ok, Managers} = clib_utils:get_managers(),
        ?TTY_INFO_REPORT({"wait...", Managers}),
        ?PROFILE_BEGIN(),
        cclib_nodes:wait_nodes(Managers),
        uss_mnesia:start(),
        ?PROFILE_END({mnesia, start}),
        ?TTY_INFO_REPORT("mnesia started")
    catch
        Class:Exception ->
            ?TTY_ERROR_REPORT({Class, Exception, erlang:get_stacktrace()}),
            init:stop()
    end.

start(_Type, _Args) ->
    try
        start2(_Type, _Args)
    catch
        Class:Exception ->
            ?TTY_ERROR_REPORT({Class, Exception, _Type, _Args,
                    erlang:get_stacktrace()}),
            init:stop()
    end.

start2(_Type, _Args) ->
    ?TTY_INFO_REPORT({start, _Type, _Args}),
    case _Type of
        {failover, _Node} ->
            Sleep =
            case cclib_utils:ping(_Node) of
                true  -> 3;
                false -> net_kernel:get_net_ticktime()
            end,
            ?TTY_INFO_REPORT({sleep, Sleep}),
            timer:sleep(Sleep * 1000),
            case uss_fence:is_fence_passed() of
                true ->
                    ok;
                false ->
                    ?TTY_ERROR_REPORT({fence_failed, _Type, _Args}),
                    init:stop()
            end;
        {takeover, _Node} ->
            rpc:multicall(init, restart, []);
        normal ->
            ok;
        _ -> ok
    end,

    init_specific(),
    %
    application:load(cclib),
    %
    {ok, Pid} = uss_sup:start_link(_Args),
    ensure_plugins(),
    {ok, Pid}.

stop(_State) ->
    ?TTY_INFO_REPORT({stop, _State}),
    stop_specific(),
    ok.

ensure_plugins() ->
    Root = code:lib_dir(?APPLICATION),
    case file:read_file(filename:join([Root, "conf/plugins.tpl"])) of
        {ok, B} ->
            L = string:tokens(binary_to_list(B), " \t\n"),
            lists:foreach(fun load_plugin/1, L);
        Other ->
            ?TTY_INFO_REPORT(Other)
    end.

load_plugin(Plugin) ->
    Root = code:lib_dir(?APPLICATION),
    code:add_patha(lists:concat([Root, "/plugins/", Plugin, "/ebin"])),
    case application:start(cclib_utils:to_atom(Plugin)) of
        ok ->
            ?TTY_INFO_REPORT({plugin, Plugin, "started!"});
        {error, Reason} ->
            ?TTY_INFO_REPORT({plugin, Plugin, "start failed:", Reason})
    end.

is_manager() ->
    case application:get_env(?APPLICATION, asmaster) of
        {ok, 1} -> true;
        _ -> false
    end.

init_specific() ->
    case is_manager() of
        true ->
            cclib_app:ensure_started(kernel),
            cclib_app:ensure_started(sasl),
            cclib_app:ensure_started(crypto),
            cclib_app:ensure_started(mnesia),
            cclib_app:ensure_started(inets),
            cclib_app:ensure_started(mochiweb),
            application:set_env(webmachine, webmachine_logger_module, webmachine_logger),
            cclib_app:ensure_started(webmachine),

            start_web(),
            ok;
        false ->
            ok
    end.

stop_specific() ->
    case is_manager() of
        true ->
            stop_web(),
            ok;
        false ->
            ok
    end.

start_web() ->
    ?TTY_INFO_REPORT({start_web}),
    SrcDir = code:lib_dir(?APPLICATION),
    Cmd = io_lib:format("bash ~s/script/webctl.sh start", [SrcDir]),
    cclib_utils:cmd(Cmd).

stop_web() ->
    ?TTY_INFO_REPORT({stop_web}),
    SrcDir = code:lib_dir(?APPLICATION),
    Cmd = io_lib:format("bash ~s/script/webctl.sh stop", [SrcDir]),
    cclib_utils:cmd(Cmd).

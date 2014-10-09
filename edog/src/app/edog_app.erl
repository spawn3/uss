-module(edog_app).
-export([start/2, stop/1]).
-export([is_manager/0]).
-export([stop_web/0]).
-export([stop_specific/0]).

-behaviour(application).

-include("edog_common.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(_Type, _Args) ->
    try
        case is_manager() of
            true  -> ok;
            false -> cclib:start_logger(false)
        end,
        ?PROFILE_BEGIN(),
        Res = start2(_Type, _Args),
        ?PROFILE_END(?MODULE),
        Res
    catch
        Class:Exception ->
            ?ERROR({
                    Class, Exception, _Type, _Args,
                    erlang:get_stacktrace()}),
            ?INIT_STOP()
    end.

stage({fence_test, Node, Info}) ->
    case edog_fence:is_fence_passed() of
        true ->
            ?INFO({fence_ok, Node}),
            ok;
        false ->
            edog_fence:do_after(Info)
    end;
stage({sleep, Node}) ->
    Sleep =
    case cclib_node:ping(Node) of
        true  -> edog_conf:manager_failover_wait_factor() * net_kernel:get_net_ticktime();
        false -> edog_conf:manager_failover_wait_factor() * net_kernel:get_net_ticktime()
    end,
    ?INFO({sleep, Sleep}),
    timer:sleep(Sleep * 1000);
stage({ping, Node}) ->
    case net_adm:ping(Node) of
        pong ->
            ?INIT_RESTART();
        pang ->
            ok
    end.

%% normal
%% {failover, Node}
%% {takeover, Node}
start2(_Type, _Args) ->
    ?INFO({start, _Type, _Args}),
    case _Type of
        normal ->
            % TODO
            Flag = cclib_app:check_flag(?APPLICATION),
            ?INFO({check_flag, Flag}),
            ok;
        {failover, _Node} ->
            stage({fence_test, _Node, {_Type, _Args}}),
            stage({sleep, _Node}),
            stage({ping, _Node});
        {takeover, _Node} ->
            rpc:multicall(init, restart, []);
        _ ->
            ok
    end,
    init_specific(),

    Res = edog_sup:start_link([_Type|_Args]),
    ?INFO(Res),
    timer:sleep(1000),
    case _Type of
        {failover, MasterNode} ->
            do_notify_nodedown(MasterNode);
        _ ->
            ok
    end,
    case Res of
        {ok, Pid} when is_pid(Pid) ->
            Res;
        {ok, Pid, _} when is_pid(Pid) ->
            Res;
        _ ->
            ?INIT_STOP()
    end.

do_notify_nodedown(MasterNode) ->
    Ip = cclib_node:get_ip(MasterNode),
    Node = edog_common:get_node(Ip),
    ?INFO({nodedown, Node}),
    ?INFO({global, global:whereis_name(?DIST_NAME)}),
    edog_master:notify_nodedown(Node).

stop(_State) ->
    ?INFO({stop, _State}),
    stop_specific(),
    ok.

is_manager() ->
    case application:get_env(?APPLICATION, asmaster) of
        {ok, 1} ->
            true;
        _ ->
            false
    end.

init_specific() ->
    cclib_app:ensure_started(os_mon),
    cclib_app:ensure_started(cluster_info),
    case is_manager() of
        true ->
            cclib_app:create_flag(?APPLICATION),
            cclib_app:ensure_started(crypto),
            cclib_app:ensure_started(public_key),
            cclib_app:ensure_started(ssl),
            cclib_app:ensure_started(xmerl),
            %cclib_app:ensure_started(compiler),
            %cclib_app:ensure_started(syntax_tools),
            %cclib_app:ensure_started(inets),
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
            cclib_app:delete_flag(?APPLICATION);
        false ->
            ok
    end.

start_web() ->
    ?INFO({start_web}),
    SrcDir = code:lib_dir(?APPLICATION),
    Cmd = io_lib:format("bash ~s/script/webctl.sh start", [SrcDir]),
    cclib_utils:cmd(Cmd).

stop_web() ->
    ?INFO({stop_web}),
    SrcDir = code:lib_dir(?APPLICATION),
    Cmd = io_lib:format("bash ~s/script/webctl.sh stop", [SrcDir]),
    cclib_utils:cmd(Cmd).

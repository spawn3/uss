-module(uss_manager_rt).
-behaviour(gen_fsm).
-export([
        start_link/0,
        stop/0,
        init/1,
        handle_event/3,
        handle_sync_event/4,
        handle_info/3,
        terminate/3,
        code_change/4]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("uss_common.hrl").

-define(EDOG_TIMEOUT, 10000).

%% -----------------------------------------------------------------
%% API
%% -----------------------------------------------------------------

%% -----------------------------------------------------------------
%% Callbacks
%% -----------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    ok.

init([]) ->
    {ok, loop_it, [], ?EDOG_TIMEOUT}.

handle_event(_Event, _StateName, _StateData) -> ok.
handle_sync_event(_, _, _, _) -> ok.
handle_info(_Info, _StateName, _StateData) -> ok.
terminate(_Reason, _StateName, _StateData) -> ok.
code_change(_OldVsn, _StateName, _StateData, _Extra) -> ok.

%% states
loop_it(timeout, _StateData) ->
    %% Ensure agents are alive
    cclib_app:ensure_started(mnesia),
    rpc:multicall(nodes(), application, start, [?APPLICATION]),
    do_routine(),
    {next_state, loop_it, _StateData, ?EDOG_TIMEOUT}.

do_routine() ->
    uss_mnesia:queue_run(),
    check_pm(),
    ok.

check_pm() ->
    NodeList = uss_mnesia:all_nodes(),
    {ok, L} = uss_pm:cluster_status(NodeList),
    CannotSsh = [Ip || {Ip, cannot_ssh} <- L],
    CanSsh    = [Ip || {Ip, can_ssh} <- L],
    Alive     = [Ip || {Ip, alive} <- L],
    NotAlive  = [Ip || {Ip, Status} <- L, Status =/= alive],
    lists:foreach(
        fun(R) -> uss_mnesia:pm_update_status(R, running) end,
        Alive),
    lists:foreach(
        fun(R) -> uss_mnesia:pm_update_status(R, unavailable) end,
        NotAlive),
    case CannotSsh of
        [] ->
            ok;
        _ ->
            ?TTY_WARN_REPORT({cannot_ssh, CannotSsh})
    end,
    case CanSsh of
        [] ->
            ok;
        _ ->
            uss_pm:cluster_deploy(CanSsh, all),
            uss_pm:cluster_start(CanSsh),
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            F = fun(Ip) ->
                uss_mnesia:queue_push(#uss_queue_t{
                        et='service',
                        event='start',
                        info=Ip})
            end,
            lists:foreach(F, CanSsh)
    end.

ping(Ip) when is_list(Ip) ->
    Node = clib_utils:get_node(Ip),
    ping(Node);
ping(Node) when is_atom(Node) ->
    case net_adm:ping(Node) of
        pong ->
            erase(Node);
        pang ->
            case get(Node) of
                undefined ->
                    put(Node, 1);
                Cnt when Cnt < 3 ->
                    put(Node, Cnt+1);
                _Cnt ->
                    %% FIXME
                    erase(Node),
                    ?ERROR_REPORT({nodedown, Node, _Cnt}),
                    global:send(?USS_MANAGER, {nodedown, Node, [ping]})
            end
    end.

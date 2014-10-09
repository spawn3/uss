-module(edogdb_cluster).
-behaviour(gen_server).
-export([
        start_link/0,
        init/1,
        terminate/2,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3]).
-compile(export_all).

-include("edogdb.hrl").

-define(APP_SRC, "/sysy/yfs/ussadmin/edog").
-define(APP_DIST_NAME, edog_master).

-record(state, {
        interval
    }).

%% -----------------------------------------------------------------
%% API
%% -----------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% -----------------------------------------------------------------
%% Callbacks
%% -----------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    Interval = ?EDOGDB_CLUSTER_LOOP_INTERVAL,
    State = #state{interval=Interval},
    {ok, State, ?EDOGDB_CLUSTER_START_INTERVAL}.

terminate(_Reason, _StateData) ->
    ok.

handle_call(_Msg, _From, #state{interval=Interval} = _StateData) ->
    {reply, ok, _StateData, Interval}.

handle_cast(_Msg, _State) ->
    do_noreply(_State).

handle_info(timeout, #state{interval=Interval} = _StateData) ->
    try
        {T, _NewStateData} = timer:tc(?MODULE, do_routine, [_StateData]),
        T1 = T div 1000,
        if
            T1 >= Interval ->
                {noreply, _NewStateData, 1000};
            true ->
                {noreply, _NewStateData, Interval - T1}
        end
    catch
        Class:Exception ->
            ?WARN({Class, Exception, erlang:get_stacktrace()}),
            do_noreply(_StateData)
    end;
handle_info({'DOWN', _Mref, _, _, _Result} = Msg, _StateData) ->
    ?INFO(Msg),
    case _Result of
        {error, _Reason} ->
            ok;
        _ ->
            ok
    end,
    do_noreply(_StateData#state{});
handle_info(_Info, _StateData) ->
    ?WARN({_Info, _StateData}),
    do_noreply(_StateData).

code_change(_OldVsn, _StateData, _Extra) ->
    {ok, _StateData}.

%% --------------------------------------------------------------------------
%% DO
%% --------------------------------------------------------------------------
do_noreply(#state{interval=Interval} = _StateData) ->
    {noreply, _StateData, Interval}.

do_routine(StateData) ->
    try
        cclib_app:ensure_started(mnesia),
        do(StateData)
    catch
        _:_ ->
            StateData
    end.

do(#state{} = StateData) ->
    do_backup(),
    check_managers(),
    web_gc(),
    StateData.

-define(DB_BACKUP_INTERVAL_MIN, 600).

do_backup() ->
    case edogdb_config:db_backup_interval() of
        Interval when Interval >= ?DB_BACKUP_INTERVAL_MIN ->
            edogdb_backup:backup_db();
        _ ->
            ?ERROR({db_backup_interval, [
                        {too_small, edogdb_config:db_backup_interval()},
                        {min, ?DB_BACKUP_INTERVAL_MIN}
                    ]})
    end.

check_managers() ->
    {ok, Managers} = get_managers(),
    F = fun(Manager) -> spawn(?MODULE, check_and_start_manager, [Manager]) end,
    lists:foreach(F, Managers).

check_and_start_manager(Manager) when Manager =:= node() ->
    ok;
check_and_start_manager(Manager) ->
    case net_adm:ping(Manager) of
        pong ->
            ok;
        _ ->
            {ok, Managers} = get_managers(),
            case alive_nodes(Managers) of
                Managers ->
                    start_manager(Manager);
                Alives ->
                    ?ERROR({pm_not_up, Managers -- Alives}),
                    ok
            end
    end.

alive_nodes(Nodes) when is_list(Nodes) ->
    [X || X <- Nodes, cclib_node:ping(X)].

start_manager(Node) when Node =:= node() ->
    ok;
start_manager(Node) when is_atom(Node) ->
    start_manager(cclib_node:get_ip(Node));
start_manager(Ip) ->
    Cmd = io_lib:format("ssh root@~s ~s/bin/appctl manager start ~s", [Ip, ?APP_SRC, Ip]),
    ?INFO({iolist_to_binary(Cmd), [node()|nodes()]}),
    cclib_cmd:run(Cmd, 5000).

get_managers() ->
    case application:get_env(kernel, edog_masters) of
        {ok, Managers} ->
            {ok, Managers}
    end.

web_gc() ->
    case global:whereis_name(?APP_DIST_NAME) of
        Pid when is_pid(Pid) ->
            if
                erlang:node(Pid) =:= node() ->
                    ok;
                true ->
                    stop_web()
            end;
        _ ->
            ok
    end.

stop_web() ->
    Cmd = io_lib:format("bash ~s/script/webctl.sh stop", [?APP_SRC]),
    cclib_cmd:exec(Cmd).

-module(edogdb_monitor).
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

-define(MONITOR_APP, edog).
-define(MONITOR_GLOBAL, edog_master).
-define(MONITOR_INTERVAL, 120000).

-record(state, {
        interval,
        timer=undefined,
        first=false
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
    Interval = ?EDOGDB_MONITOR_LOOP_INTERVAL,
    State = #state{interval=Interval},
    {ok, State, ?EDOGDB_MONITOR_START_INTERVAL}.

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
handle_info({timeout, monitor_app_down}=_Info, _StateData) ->
    ?WARN({_Info, _StateData}),
    case is_monitor_app_ready() of
        true  -> ok;
        false ->
            ?WARN({monitor_app_down, "111"}),
            Res = init:restart(),
            ?WARN({monitor_app_down, "222", Res})
    end,
    do_noreply(_StateData);
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
        do(StateData)
    catch
        _:_ ->
            StateData
    end.

do(#state{first=false} = StateData) ->
    case global:whereis_name(?MONITOR_GLOBAL) of
        Pid when is_pid(Pid) ->
            StateData#state{first=true};
        undefined ->
            StateData
    end;
do(#state{timer=Timer} = StateData) ->
    case is_monitor_app_ready() of
        true ->
            StateData#state{timer=clear_timer(Timer)};
        false ->
            init_timer(StateData)
    end.

init_timer(#state{timer=Timer} = StateData) when is_reference(Timer) ->
    case erlang:read_timer(Timer) of
        false ->
            StateData#state{timer=init_timer_2()};
        Remaining ->
            ?INFO({timer, Remaining div 1000}),
            StateData
    end;
init_timer(#state{} = StateData) ->
    StateData#state{timer=init_timer_2()}.

init_timer_2() ->
    T = ?MONITOR_INTERVAL,
    ?INFO({start_timer, T div 1000}),
    erlang:send_after(T, ?MODULE, {timeout, monitor_app_down}).

clear_timer(Timer) when is_reference(Timer) ->
    erlang:cancel_timer(Timer),
    undefined;
clear_timer(_Timer) ->
    undefined.

is_monitor_app_ready() ->
    case global:whereis_name(?MONITOR_GLOBAL) of
        Pid when is_pid(Pid) ->
            true;
        undefined ->
            ?ERROR(dump()),
            Apps = application:info(),
            {running, Running} = lists:keyfind(running, 1, Apps),
            case lists:keyfind(?MONITOR_APP, 1, Running) of
                {?MONITOR_APP, {distributed, _Node}} ->
                    false;
                {?MONITOR_APP, UssPid} when is_pid(UssPid) ->
                    false;
                _ ->
                    false
            end
    end.

dump() ->
    [
        {dump,  erlang:get_stacktrace()},
        {nodes, [node()|nodes()]},
        {apps,  application:info()},
        {edogdbdb, whereis(edogdbdb)},
        {edogdbdb_monitor, whereis(edogdbdb_monitor)},
        {edog_master, global:whereis_name(edog_master)},
        {edog_master_rt, whereis(edog_master_rt)}
    ].

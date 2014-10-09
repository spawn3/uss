-module(edog_target_gc).
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

-include_lib("stdlib/include/qlc.hrl").
-include("edog_common.hrl").

-define(ETS_TABLE, ?MODULE).
-define(SH_LOCK, read).
-define(EX_LOCK, write).

-record(state, {
        targets = [],
        interval
    }).

-record(lock, {
        target,
        type,
        refcount,
        time
    }).

%% -----------------------------------------------------------------
%% API
%% -----------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

rdlock(Target) ->
    call_wrapper({rdlock, Target}).

wrlock(Target) ->
    call_wrapper({wrlock, Target}).

unlock(Target) ->
    gen_server:cast(?MODULE, {unlock, Target}).

%%
call_wrapper(Request) ->
    call_wrapper(Request, ?TO_CALL).

call_wrapper(Request, Timeout) ->
    try
        gen_server:call(?MODULE, Request, Timeout)
    catch
        Class:Exception ->
            ?INFO({Class, Exception, erlang:get_stacktrace()}),
            {error, Class}
    end.

%% -----------------------------------------------------------------
%% Callbacks
%% -----------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    _ = ets:new(?ETS_TABLE, [set, named_table, protected, {keypos, #lock.target}]),
    Interval = edog_conf:target_gc_interval(),
    State = #state{interval=Interval},
    ?INFO({?MODULE, "inited"}),
    {ok, State, 10000}.

terminate(_Reason, _StateData) ->
    ok.

handle_call({rdlock, Target}, _From, _State) ->
    ?INFO({rdlock, Target}),
    Reply = do_rdlock(Target),
    {reply, Reply, _State, _State#state.interval};
handle_call({wrlock, Target}, _From, _State) ->
    ?INFO({wrlock, Target}),
    Reply = do_wrlock(Target),
    {reply, Reply, _State, _State#state.interval};
handle_call(_Msg, _From, #state{interval=Interval} = _StateData) ->
    {reply, ok, _StateData, Interval}.

handle_cast({unlock, Target}, _State) ->
    ?INFO({unlock, Target}),
    do_unlock(Target),
    do_noreply(_State);
handle_cast(_Msg, _State) ->
    do_noreply(_State).

handle_info(timeout, #state{interval=Interval, targets=Targets} = _StateData) ->
    try
        case length(Targets) of
            0 ->
                case all_targets() of
                    [] ->
                        {noreply, _StateData#state{targets=[]}, Interval};
                    NewTargets ->
                        {noreply, _StateData#state{targets=NewTargets}, 1}
                end;
            1 ->
                [Target|_] = Targets,
                do_gc(Target),
                {noreply, _StateData#state{targets=[]}, Interval};
            _N ->
                [Target|T] = Targets,
                do_gc(Target),
                {noreply, _StateData#state{targets=T}, 1}
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

%% --------------------------------------------------------------------------
%% TARGET GC
%% --------------------------------------------------------------------------
-define(RPC_TIMEO, 3000).

do_gc(Target) ->
    case do_wrlock(Target) of
        ok ->
            do_gc_1(Target),
            do_unlock(Target),
            ok;
        {error, Reason} ->
            ?WARN({lock, Target, Reason}),
            {error, Reason}
    end.

do_gc_1(Target) ->
    Nodes = all_nodes(),
    %?PROFILE_BEGIN(),
    lists:foreach(fun(Node) -> do_gc_2(Target, Node) end, Nodes),
    %?PROFILE_END({Target, Nodes}),
    ok.

do_gc_2(Target, Node) ->
    SkipNodes = edog_disk:disk_on_node(Target),
    Ip = cclib_node:get_ip(Node),
    case lists:member(Ip, SkipNodes) of
        true -> ok;
        false ->
            case rpc:call(Node, edog_iscsi, target_gc, [Target], ?RPC_TIMEO) of
                {badprc, Reason} ->
                    {error, Reason};
                Res ->
                    Res
            end
    end.

all_targets() ->
    [Target || #disk_t{disk_id=Target} <- cclib_mnesia:i(disk_t)].

all_nodes() ->
    Pms = cclib_mnesia:i(pm_t),
    [edog_common:get_node(Ip) || #pm_t{pm_id=Ip} <- Pms].

%% --------------------------------------------------------------------------
%% LOCK
%% --------------------------------------------------------------------------
do_rdlock(Target) ->
    case ets:lookup(?ETS_TABLE, Target) of
        [] ->
            ets:insert(?ETS_TABLE, #lock{target=Target, type=?SH_LOCK,
                    refcount=1, time=erlang:now()}),
            ok;
        [#lock{type=?SH_LOCK, refcount=RefCount}=Lock] ->
            ets:insert(?ETS_TABLE, Lock#lock{refcount=RefCount+1, time=erlang:now()}),
            ok;
        [#lock{type=?EX_LOCK}] ->
            {error, ?EX_LOCK}
    end.

do_wrlock(Target) ->
    case ets:lookup(?ETS_TABLE, Target) of
        [] ->
            ets:insert(?ETS_TABLE, #lock{target=Target, type=?EX_LOCK,
                    refcount=1, time=erlang:now()}),
            ok;
        [#lock{type=Type}] ->
            {error, Type}
    end.

do_unlock(Target) ->
    case ets:lookup(?ETS_TABLE, Target) of
        [] ->
            ok;
        [#lock{refcount=RefCount}] when RefCount =< 1 ->
            ets:delete(?ETS_TABLE, Target),
            ok;
        [#lock{refcount=RefCount} = Lock] ->
            ets:insert(?ETS_TABLE, Lock#lock{refcount=RefCount-1}),
            ok
    end.

is_locked(_Target) ->
    true.

%% --------------------------------------------------------------------------
%% MISC
%% --------------------------------------------------------------------------
tpl() ->
    cclib_dbg:start(),
    ok.

mock_all_targets() ->
    ["tgt1", "tgt2", "tgt3"].

lock_test() ->
    Target1 = "tgt1",
    ok = rdlock(Target1),
    ok = rdlock(Target1),

    unlock(Target1),
    {error, ?SH_LOCK} = wrlock(Target1),

    unlock(Target1),
    ok = wrlock(Target1),

    {error, ?EX_LOCK} = rdlock(Target1),
    {error, ?EX_LOCK} = wrlock(Target1),

    unlock(Target1),

    ok = rdlock(Target1),
    unlock(Target1),

    ok.

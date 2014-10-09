-module(edog_load).
-behaviour(gen_fsm).
-export([
        start_link/0,
        init/1,
        handle_event/3,
        handle_sync_event/4,
        handle_info/3,
        terminate/3,
        code_change/4,

        loop_it/2,
        select_node/1,
        select_node/2
    ]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("edog_common.hrl").

-define(TIMEOUT, 10000).

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, loop_it, [], ?TIMEOUT}.

handle_event(_Event, _SN, _SD) ->
    ok.

handle_sync_event(_, _, _, _) ->
    ok.

handle_info(_Info, _SN, _SD) ->
    ok.

terminate(_Reason, _SN, _SD) ->
    ok.

code_change(_OldVsn, _SN, _SD, _Extra) ->
    ok.

%% states
loop_it(timeout, _SD) ->
    {next_state, loop_it, _SD, ?TIMEOUT}.

%% ---------------------------------------------------------------------------
%%
%% ---------------------------------------------------------------------------
get_load_2(Node) ->
    case ets:lookup(edog_master, {slaves, Node}) of
        [{{slaves, Node}, Time, Data}] ->
            Avg1  = edog_common:keyfind(avg1, 1, Data, 0),
            Avg5  = edog_common:keyfind(avg5, 1, Data, 0),
            Avg15 = edog_common:keyfind(avg15, 1, Data, 0),

            Mem      = edog_common:keyfind(mem, 1, Data, []),
            Free     = edog_common:keyfind(free_memory, 1, Mem, 0),
            Cached   = edog_common:keyfind(cached_memory, 1, Mem, 0),
            Buffered = edog_common:keyfind(buffered_memory, 1, Mem, 0),

            Useful = ?BYTE_TO_KB((Free + (Cached + Buffered) div 2)) ,
            #load_info{node=Node,
                time=Time,
                avg1=Avg1,
                avg5=Avg5,
                avg15=Avg15,
                free=Useful};
        _ ->
            false
    end.

load_balance(#load_policy{} = LoadPolicy) ->
    Slaves = get_agents(),
    L = [get_load_2(Node) || Node <- Slaves],
    L1 =
    case LoadPolicy#load_policy.flag of
        avg1 ->
            [{Load, Node, Free} || #load_info{avg1=Load, node=Node, free=Free} <- L];
        avg15 ->
            [{Load, Node, Free} || #load_info{avg15=Load, node=Node, free=Free} <- L];
        _ ->
            [{Load, Node, Free} || #load_info{avg5=Load, node=Node, free=Free} <- L]
    end,
    Sorted = lists:reverse(lists:keysort(1, L1)),
    try_migrate(Sorted, LoadPolicy).

try_migrate([], _LP) ->
    ok;
try_migrate([{Load, Node, _Free} | T] = _L, #load_policy{min=LoadMin} = _LP) ->
    if
        Load >= LoadMin ->
            PmID = cclib_node:get_ip(Node),
            F = fun() ->
                    Q = qlc:q([Vm || Vm <- mnesia:table(vm_t), Vm#vm_t.pm_id =:= PmID]),
                    qlc:e(Q)
            end,
            case mnesia:transaction(F) of
                {atomic, []} ->
                    try_migrate(T, _LP);
                {atomic, [#vm_t{}]} ->
                    try_migrate(T, _LP);
                {atomic, [Vm|_]} ->
                    try_migrate({Vm, Load}, lists:reverse(T), _LP);
                {aborted, _Reason} ->
                    {error, _Reason}
            end;
        true ->
            ok
    end.

try_migrate({#vm_t{}, _Load}, [], _LP) ->
    ok;
try_migrate({#vm_t{vm_mem=Mem} = Vm, FromLoad}, [{ToLoad, ToNode, ToFree}|T],
    #load_policy{diff=LoadDiff} = _LP) ->
    if
        FromLoad - ToLoad >= LoadDiff, ToFree >= Mem ->
            ToIP = cclib_node:get_ip(ToNode),
            ?INFO({load_balance, Vm, ToIP}),
            edog_master:vm_migrate(Vm#vm_t.vm_id, ToIP);
        FromLoad - ToLoad >= LoadDiff ->
            try_migrate({Vm, FromLoad}, T, _LP);
        true ->
            ok
    end.

%% ---------------------------------------------------------------------------
%% SELECT NODE
%% ---------------------------------------------------------------------------
get_agents() ->
    edog_master:slave_get_all().

select_node(Args) ->
    select_node(Args, []).

%% TODO
%% - memory_policy
%% - try_node/node
%%
%% memory: KB
select_node({max_memory, _ReqMemory, _ReqCpu}, Options) ->
    Loads = get_loads(),
    PreservedMemory = edog_conf:system_preserved_memory() * 1024,
    L = [{get_useful_memory(X) - PreservedMemory, X} || #load_info{} = X <- Loads],
    try
        NodeFreeList = [{Free, Node} || {Free, #load_info{node=Node}} <- L],
        ?INFO({NodeFreeList, _ReqMemory, Options}),
        case previous_or_max(NodeFreeList, _ReqMemory, Options) of
            {ok, {Node, NewMemory}} ->
                ?INFO({Node, NewMemory}),
                {ok, [{ip, cclib_node:get_ip(Node)}, {memory, NewMemory}]};
            {error, Reason} ->
                ?ERROR({Reason, L, _ReqMemory, Options}),
                {error, Reason}
        end
    catch
        Class:Pattern ->
            ?WARN({Class, Pattern, erlang:get_stacktrace()}),
            {error, memory_not_enough}
    end;
select_node({least_load, Flag, _ReqMemory, _ReqCpu}, _Options) ->
    Loads = get_loads(),
    L =
    case Flag of
        avg1  -> [{Load, Node} || #load_info{node=Node, free=Free, avg1=Load} <- Loads, Free > _ReqMemory];
        avg15 -> [{Load, Node} || #load_info{node=Node, free=Free, avg15=Load} <- Loads, Free > _ReqMemory];
        _     -> [{Load, Node} || #load_info{node=Node, free=Free, avg5=Load} <- Loads, Free > _ReqMemory]
    end,
    try lists:min(L) of
        {_Load, Node} ->
            {ok, cclib_node:get_ip(Node)}
    catch
        _:_ ->
            {error, least_load}
    end.

lower_bound(ReqMemory, Options) ->
    case proplists:get_value(memory_policy, Options, raw) of
        fixed ->
            edog_conf:vm_restart_memory() * 1024;   % KB
        max_available ->
            edog_conf:vm_restart_memory() * 1024;   % KB
        _     ->
            ReqMemory
    end.

upper_bound(ReqMemory, Options) ->
    case proplists:get_value(memory_policy, Options, raw) of
        fixed ->
            edog_conf:vm_restart_memory() * 1024;   % KB
        max_available ->
            ReqMemory;
        _     ->
            ReqMemory
    end.

is_memory_enough(Free, ReqMemory, Options) ->
    Upper = upper_bound(ReqMemory, Options),
    Lower = lower_bound(ReqMemory, Options),
    if
        Free >= Upper ->
            {true, Upper};
        true ->
            {Free >= Lower, Lower}
    end.

check_previous(L, ReqMemory, Options) ->
    case proplists:get_value(node, Options) of
        Previous when is_atom(Previous) ->
            L1 = [X || {_Free, Node} = X <- L, Node =:= Previous],
            case L1 of
                [{Free, _Node}|_] ->
                    case is_memory_enough(Free, ReqMemory, Options) of
                        {true, NewMemory} ->
                            {ok, {Previous, NewMemory}};
                        {false, _} ->
                            {error, memory_not_enough}
                    end;
                [] ->
                    {error, no_such_node}
            end;
        _ ->
            {error, badarg}
    end.

check_max([], _ReqMemory, _Options) ->
    {error, no_nodes};
check_max(L, ReqMemory, Options) when is_list(L) ->
    {MaxFree, Node} = lists:max(L),
    case is_memory_enough(MaxFree, ReqMemory, Options) of
        {true, NewMemory} ->
            {ok, {Node, NewMemory}};
        {false, _} ->
            {error, memory_not_enough}
    end.

previous_or_max(L, MemoryConf, Options) when is_list(L) ->
    case proplists:get_bool(try_node, Options) of
        true ->
            case check_previous(L, MemoryConf, Options) of
                {ok, Value} ->
                    {ok, Value};
                _ ->
                    check_max(L, MemoryConf, Options)
            end;
        false ->
            check_max(L, MemoryConf, Options)
    end.

get_loads() ->
    Slaves = get_agents(),
    ?INFO({get_load, Slaves}),
    ProcessingDict = edog_vm:get_processing_dict(),
    F = fun(Node) -> get_load(Node, ProcessingDict) end,
    [X || #load_info{}=X <- cclib_pmap:pmap(F, Slaves)].

get_load(Node, Dict) ->
    case get_load(Node) of
        {ok, #load_info{}=Load} ->
            Ip = cclib_node:get_ip(Node),
            Processing =
            case dict:find(Ip, Dict) of
                {ok, {_Cpu, Mem}} -> Mem;
                error             -> 0
            end,
            Free2 = erlang:max(Load#load_info.free - Processing, 0),
            Load#load_info{free=Free2};
        {error, _Reason} ->
            false
    end.

get_load(Ip) when is_list(Ip) ->
    get_load(edog_common:get_node(Ip));
get_load(Node) when is_atom(Node) ->
    case rpc:call(Node, cclib_os, get_load, []) of
        {badrpc, _Reason} ->
            ?WARN({get_load, _Reason}),
            {error, _Reason};
        #load_info{} = Load ->
            {ok, Load}
    end.

get_useful_memory(#load_info{node=Node, free=Free, mem=Mem}) ->
    TotalMemory = ?to_kb(proplists:get_value(system_total_memory, Mem)),
    {AllocedMemory, _AllocedCpu} = edog_table_vm:vm_resource_alloced(Node),
    Res = erlang:min(Free, TotalMemory - AllocedMemory),
    ?INFO({Node, [{free, Free}, {total, TotalMemory},
                {alloc, AllocedMemory}, {useful, Res}]}),
    Res.

%% -----------------------------------------------------------------------
%% TEST
%% -----------------------------------------------------------------------
tpl() ->
    cclib_dbg:tpl(?MODULE, select_node),
    cclib_dbg:tpl(?MODULE, get_loads),
    cclib_dbg:tpl(?MODULE, get_useful_memory),
    ok.

-define(M0512, 1024*512).
-define(M1024, 1024*1024).
-define(M2048, 1024*2048).
-define(M3164, 1024*3164).
-define(M4096, 1024*4096).
-define(M8192, 1024*8096).

%
normal_test() ->
    L = [{?M1024,a},{?M2048,b}, {?M4096,c}],

    {ok, {c, ?M1024}} = previous_or_max(L, ?M1024, []),
    {ok, {c, ?M2048}} = previous_or_max(L, ?M2048, []),
    {ok, {c, ?M3164}} = previous_or_max(L, ?M3164, []),
    {error, memory_not_enough} = previous_or_max(L, ?M8192, []),

    ok.

previous_test() ->
    L = [{?M1024,a},{?M2048,b}, {?M4096,c}],

    {ok, {b, ?M1024}} = previous_or_max(L, ?M1024, [try_node, {node, b}]),
    {ok, {b, ?M2048}} = previous_or_max(L, ?M2048, [try_node, {node, b}]),
    {ok, {c, ?M3164}} = previous_or_max(L, ?M3164, [try_node, {node, b}]),
    {error, memory_not_enough} = previous_or_max(L, ?M8192, [try_node, {node, b}]),

    ok.

%
fixed_test() ->
    L = [{?M1024,a},{?M2048,b}, {?M4096,c}],

    {ok, {c, ?M1024}} = previous_or_max(L, ?M1024, [{memory_policy, fixed}]),
    {ok, {c, ?M1024}} = previous_or_max(L, ?M2048, [{memory_policy, fixed}]),
    {ok, {c, ?M1024}} = previous_or_max(L, ?M3164, [{memory_policy, fixed}]),
    {ok, {c, ?M1024}} = previous_or_max(L, ?M8192, [{memory_policy, fixed}]),

    ok.

previous_fixed_test() ->
    L = [{?M1024,a},{?M2048,b}, {?M4096,c}],

    {ok, {b, ?M1024}} = previous_or_max(L, ?M1024, [try_node, {node, b}, {memory_policy, fixed}]),
    {ok, {b, ?M1024}} = previous_or_max(L, ?M2048, [try_node, {node, b}, {memory_policy, fixed}]),
    {ok, {b, ?M1024}} = previous_or_max(L, ?M3164, [try_node, {node, b}, {memory_policy, fixed}]),
    {ok, {b, ?M1024}} = previous_or_max(L, ?M8192, [try_node, {node, b}, {memory_policy, fixed}]),

    ok.

%
max_available_test() ->
    L = [{?M1024,a},{?M2048,b}, {?M4096,c}],

    {ok, {c, ?M1024}} = previous_or_max(L, ?M1024, [{memory_policy, max_available}]),
    {ok, {c, ?M2048}} = previous_or_max(L, ?M2048, [{memory_policy, max_available}]),
    {ok, {c, ?M3164}} = previous_or_max(L, ?M3164, [{memory_policy, max_available}]),
    {ok, {c, ?M1024}} = previous_or_max(L, ?M8192, [{memory_policy, max_available}]),

    ok.

previous_max_available_test() ->
    L = [{?M1024,a},{?M2048,b}, {?M4096,c}],

    {ok, {b, ?M1024}} = previous_or_max(L, ?M1024, [try_node, {node, b}, {memory_policy, max_available}]),
    {ok, {b, ?M2048}} = previous_or_max(L, ?M2048, [try_node, {node, b}, {memory_policy, max_available}]),
    {ok, {b, ?M1024}} = previous_or_max(L, ?M3164, [try_node, {node, b}, {memory_policy, max_available}]),
    {ok, {b, ?M1024}} = previous_or_max(L, ?M8192, [try_node, {node, b}, {memory_policy, max_available}]),

    ok.

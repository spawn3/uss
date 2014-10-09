-module(edogdb).
-behaviour(gen_server).
-export([
        start_link/0,
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
    ]).

-include("edogdb.hrl").

-define(RPC_TIMEO, 3000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% -----------------------------------------------------------
%% Callback
%% -----------------------------------------------------------
init(_Args) ->
    process_flag(trap_exit, true),
    erlang:send_after(0, self(), bottom_half),
    ?INFO({?MODULE, inited}),
    {ok, []}.

terminate(_Reason, _State) ->
    {noreply, _State}.

handle_call(_Msg, _From, _State) ->
    ?WARN({unknown, _Msg, _From, _State}),
    {reply, ok, _State}.

handle_cast(_Msg, _State) ->
    ?WARN({unknown, _Msg, _State}),
    {noreply, _State}.

handle_info(bottom_half, _State) ->
    ?INFO(bottom_half),
    try
        start_db(),
        mnesia:subscribe(system)
    catch
        Error:Exception ->
            ?ERROR({Error, Exception, erlang:get_stacktrace()}),
            timer:sleep(3000),
            ?INIT_RESTART()
    end,
    {noreply, _State};
handle_info({mnesia_system_event, _Reason} = _Msg, _State) ->
    ?WARN(_Msg),
    {noreply, _State};
handle_info(_Msg, _State) ->
    ?WARN(_Msg),
    {noreply, _State}.

code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.

%% -----------------------------------------------------------
%% DO Method
%% -----------------------------------------------------------
start_db() ->
    {ok, NodeList} = get_managers(),
    ?INFO({managers, NodeList}),
    create_schema(NodeList),
    rpc:multicall(NodeList, mnesia, start, [], ?RPC_TIMEO),
    create_tables(NodeList),
    wait_for_tables(),
    ?INFO({"mnesia started", node()}).

create_schema(NodeList) ->
    Dir = mnesia:system_info(directory),
    ?INFO({dir, Dir}),

    case cclib_mnesia:is_schema_inited(Dir) of
        false ->
            ?INFO({create_schema, NodeList, Dir}),
            wait_nodes(NodeList),
            rpc:multicall(NodeList, filelib, ensure_dir, [Dir], ?RPC_TIMEO),
            rpc:multicall(NodeList, mnesia, stop, [], ?RPC_TIMEO),
            mnesia:create_schema(NodeList),
            mnesia:start(),
            mnesia:stop(),
            timer:sleep(3000);
        true ->
            ok
    end.

create_tables(NodeList) ->
    ?INFO({create_tables, NodeList}),
    ok = mnesia:wait_for_tables([schema], 10000),

    %F = fun(Node) -> ?INFO({Node, rpc:call(Node, mnesia, system_info, [all])}) end,
    %lists:foreach(F, NodeList),

    ?DEBUG({system_info, mnesia:system_info(all)}),
    ?DEBUG({table_info, mnesia:table_info(schema, all)}),
    ?DEBUG({system_info, mnesia:system_info(db_nodes)}),
    ?DEBUG({nodes, [node()|nodes()]}),
    Tables = mnesia:system_info(tables),

    ?CREATE_RAM_TABLE(Tables, NodeList, pm_t,     ordered_set),
    ?CREATE_RAM_TABLE(Tables, NodeList, option_t, ordered_set),

    ?CREATE_TABLE(Tables, NodeList, cust_t,    ordered_set),
    ?CREATE_TABLE(Tables, NodeList, stddisk_t, ordered_set),
    ?CREATE_TABLE(Tables, NodeList, disk_t,    ordered_set),
    ?CREATE_TABLE(Tables, NodeList, vm_t,      ordered_set),
    ?CREATE_TABLE(Tables, NodeList, disk_vm_t, ordered_set),

    ?DEBUG({table_info, mnesia:table_info(schema, all)}),
    ok.

wait_for_tables() ->
    ?PROFILE_BEGIN(),
    Result = wait_for_tables(get_all_tables()),
    ?PROFILE_END(wait_for_tables),
    ?INFO({wait_for_tables, Result}).

wait_for_tables(Tables) ->
    case mnesia:wait_for_tables(Tables, 10000) of
        {timeout, RemainingTabs} ->
            mnesia_panic(RemainingTabs),
            {timeout, RemainingTabs};
        ok ->
            ok
    end.

mnesia_panic(RemainingTabs) ->
    ?WARN({force_load_table, RemainingTabs}),
    F = fun(Tab) -> yes = mnesia:force_load_table(Tab) end,
    lists:foreach(F, RemainingTabs).

get_all_tables() ->
    [cust_t, stddisk_t, vm_t, disk_t, disk_vm_t].

%% ------------------------------------------------------------------
%% ------------------------------------------------------------------
get_managers() ->
    case application:get_env(kernel, edog_masters) of
        {ok, Managers} ->
            {ok, Managers}
    end.

-define(WAIT_MASTERS_TIMEOUT, 30).

wait_nodes(NodeList) ->
    cclib_node:wait_nodes(NodeList, ?WAIT_MASTERS_TIMEOUT).

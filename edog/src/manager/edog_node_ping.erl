-module(edog_node_ping).
-behaviour(gen_server).
-export([
        start_link/1,
        init/1,
        terminate/2,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3]).
-compile(export_all).

-include("edog_common.hrl").

-define(ETS_TABLE, ?MODULE).
-define(START_INTERVAL, 1000).
-define(LOOP_INTERVAL, 10000).

-record(state, {
        interval=?LOOP_INTERVAL
    }).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% --------------------------------------------------------------------------
%% CALLBACK
%% --------------------------------------------------------------------------
init(_Args) ->
    process_flag(trap_exit, true),
    _ = ets:new(?ETS_TABLE, [set, named_table, public]),
    {ok, #state{}, ?START_INTERVAL}.

terminate(_Reason, _StateData) ->
    ok.

handle_call(_Msg, _From, #state{interval=Interval} = _StateData) ->
    {reply, ok, _StateData, Interval}.

handle_cast(_Msg, _State) ->
    do_noreply(_State).

handle_info(timeout, #state{} = _StateData) ->
    try
        check_agents(),
        ping_agents()
    catch
        Error:Exception ->
            ?WARN({Error, Exception, erlang:get_stacktrace()})
    end,
    do_noreply(_StateData);
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

%%
check_agents() ->
    Agents = edog_cluster:get_agents(),
    F = fun(Agent) -> spawn(?MODULE, check_and_start_agent, [Agent]) end,
    lists:foreach(F, Agents).

check_and_start_agent(Ip) when is_list(Ip) ->
    check_and_start_agent(edog_common:get_node(Ip));
check_and_start_agent(Node) when is_atom(Node) ->
    case net_adm:ping(Node) of
        pong -> ok;
        pang ->
            case cclib_node:ping(Node) of
                true -> start_agent(Node);
                false -> ok
            end
    end.

start_agent(Node) when is_atom(Node) ->
    Ip = cclib_node:get_ip(Node),
    case edog_shell:start_agent(Ip) of
        {0, _Data} ->
            ok;
        {_Code, _Value} ->
            %?WARN({start_agent, Code, Value})
            ok
    end.

ping_agents() ->
    Vms = edog_table_vm:all(),
    Pms1 = lists:usort([PmID || #vm_t{pm_id = PmID} <- Vms, is_list(PmID), length(PmID) > 0]),
    Pms2 = edog_mnesia:get_nodes(),
    Pms  = lists:usort(Pms1 ++ Pms2),
    do_check_nodes(Pms).

%%
do_check_nodes(Pms) ->
    F = fun(PmId) -> spawn(?MODULE, pm_ping, [PmId]) end,
    lists:foreach(F, Pms).

-define(MAX_PING, 2).

pm_ping(Node) when is_atom(Node) ->
    case net_adm:ping(Node) of
        pong ->
            ets:delete(?ETS_TABLE, Node);
        pang ->
            case ets:lookup(?ETS_TABLE, Node) of
                [] ->
                    %?WARN({pm_ping, Node, 0}),
                    ets:insert(?ETS_TABLE, {Node, 1});
                [{Node, Cnt}] when Cnt < ?MAX_PING ->
                    %?WARN({pm_ping, Node, Cnt}),
                    ets:insert(?ETS_TABLE, {Node, Cnt+1});
                [{Node, Cnt}] ->
                    %% FIXME
                    ?WARN({pm_ping, Node, Cnt}),
                    ets:delete(?ETS_TABLE, Node),
                    edog_master:notify_nodedown(Node)
            end
    end;
pm_ping(PmID) ->
    Node = edog_common:get_node(PmID),
    pm_ping(Node).

-module(edog_slaves).
-behaviour(gen_server).
-export([start_link/0]).
-export([
        procs/0,

        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
     ]).
-compile(export_all).

-include("edog_common.hrl").

-define(ETS_TABLE,    edog_slaves).

-record(state, {
        timer=null,
        nodedown_delay=60
    }).

%% -----------------------------------------------------------
%% API
%% -----------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_option() ->
    gen_server:call(?MODULE, {get_option}).

get_option(Key) ->
    gen_server:call(?MODULE, {get_option, Key}).

get_option(Key, Default) ->
    gen_server:call(?MODULE, {get_option, Key, Default}).

set_master_info() ->
    gen_server:cast(?MODULE, set_master_info).

%% -----------------------------------------------------------
%% Callback
%% -----------------------------------------------------------
init(_Args) ->
    case edog_linux:is_ready() of
        true -> ok;
        false ->
            ?WARN({system_check_error, kill_all_vms}),
            edog_libvirt:domain_clear(),
            ?INIT_STOP()
    end,
    process_flag(trap_exit, true),
    edog_trace:trace_agent(),

    ?PROFILE_BEGIN(),
    edog_os:check_prerequired(),

    _ = ets:new(?ETS_TABLE, [set, named_table, protected]),

    Managers = edog_cluster:get_manager_nodes(),
    _ = ets:insert(?ETS_TABLE, {managers, Managers}),
    connect_to_master(),
    do_set_master_info(),
    do_set_option(),

    net_kernel:monitor_nodes(true, [nodedown_reason]),

    %?INFO({fence_ring, edog_fence:fence_ring()}),
    ?INFO({options, do_get_option()}),
    ?PROFILE_END(agent_init),
    ?INFO({?MODULE, inited}),

    erlang:send_after(0, self(), bottom_half),

    {ok, #state{}}.

%%
handle_call({get_option}, _From, _State) ->
    Reply = do_get_option(),
    {reply, Reply, _State};
handle_call({get_option, Key}, _From, _State) ->
    Reply = do_get_option(Key),
    {reply, Reply, _State};
handle_call({get_option, Key, Default}, _From, _State) ->
    Reply = do_get_option(Key, Default),
    {reply, Reply, _State};
handle_call(_Msg, _From, _State) ->
    ?INFO({_Msg, _From, _State}),
    Reply = _Msg,
    {reply, Reply, _State}.

%%
handle_cast(set_master_info, _State) ->
    do_set_master_info(),
    {noreply, _State};
handle_cast(_Msg, _State) ->
    {noreply, _State}.

%%
handle_info(bottom_half, _State) ->
    spawn(fun() -> edog_iscsi:target_discover() end),
    {noreply, _State};
handle_info({nodedown, _Node, _InfoList} = _Msg, _State) ->
    ?WARN({_Msg, _State}),
    NewState =
    case edog_cluster:node_rule(_Node) of
        manager ->
            do_set_master_info(),
            do_manager_nodedown(_Msg, _State);
        _ ->
            _State
    end,
    {noreply, NewState};
handle_info({nodeup, _Node, _InfoList} = _Msg, #state{timer=Timer} = _State) ->
    ?INFO(_Msg),
    NewState =
    case edog_cluster:node_rule(_Node) of
        manager  ->
            connect_to_master(),
            do_set_master_info(),
            case Timer of
                null ->
                    _State;
                _ ->
                    ?WARN({cancel_timer, _Node, _InfoList, Timer}),
                    erlang:cancel_timer(Timer),
                    _State#state{timer=null}
            end;
        _ ->
            _State
    end,
    {noreply, NewState};
handle_info({timeout, _Timer, {nodedown, _Node, _InfoList}} = _Msg, _State) ->
    %% TODO
    do_set_master_info(),
    case check_network_partition() of
        true ->
            ?INFO({network_is_ok});
        false ->
            ?WARN({fence_test, _Msg, _State}),
            %% kill all virtual machines
            edog_libvirt:domain_clear(),
            edog_fence:do_after({_Msg, _State})
    end,
    {noreply, _State#state{timer=null}};
handle_info(_Msg, _State) ->
    {noreply, _State}.

terminate(_Reason, _State) ->
    {noreply, _State}.

code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.

%% ------------------------------------------------------------
%%
%% ------------------------------------------------------------
do_manager_nodedown({nodedown, _Node, _InfoList} = _Msg, _State) ->
    case check_network_partition() of
        true ->
            _State;
        false ->
            Timeout = edog_conf:agent_kill_vm_factor()*net_kernel:get_net_ticktime()*1000,
            ?WARN({start_timer, Timeout}),
            Timer = erlang:start_timer(Timeout, self(), _Msg),
            _State#state{timer=Timer}
    end.

check_network_partition() ->
    L = do_get_option(fence_ring),
    case edog_fence:is_fence_passed(L) of
        true ->
            true;
        false ->
            false
    end.

%% ------------------------------------------------------------
%% process function
%% ------------------------------------------------------------
do_set_master_info() ->
    try
        NewMasterInfo = edog_master:master_info(),
        case ets:lookup(?ETS_TABLE, master_info) of
            [] ->
                ?INFO({set_master_info, NewMasterInfo}),
                ets:insert(?ETS_TABLE, {master_info, NewMasterInfo});
            [{master_info, OldMasterInfo}] ->
                OldNode = proplists:get_value(node, OldMasterInfo),
                NewNode = proplists:get_value(node, NewMasterInfo),
                if
                    NewNode =:= OldNode ->
                        ok;
                    true ->
                        ?WARN({master_changed, [
                                    {old, OldMasterInfo},
                                    {new, NewMasterInfo}]})
                end
        end,
        {ok, NewMasterInfo}
    catch
        Error:Exception ->
            ?ERROR({master_is_down, Error, Exception, erlang:get_stacktrace()}),
            ets:delete(?ETS_TABLE, master_info),
            {error, Exception}
    end.

current_master() ->
    case ets:lookup(?ETS_TABLE, master_info) of
        [{master_info, MasterInfo}] ->
            proplists:get_value(node, MasterInfo);
        _ ->
            undefined
    end.

%% ------------------------------------------------------------
%%
%% ------------------------------------------------------------
connect_to_master() ->
    case edog_cluster:is_manager_alive() of
        true ->
            edog_master:join(node());
        false ->
            Managers = edog_cluster:get_manager_nodes(),
            ?WARN({Managers, not_ready}),
            ping_masters(Managers),
            timer:sleep(3000),
            connect_to_master()
    end.

ping_masters(Managers) ->
    lists:map(fun net_adm:ping/1, Managers),
    global:sync().

procs() ->
    L = [edog_slaves, edog_slaves_rt, edog_libvirt],
    [{X, erlang:whereis(X)} || X <- L].

%% ----------------------------------------------------------------
%% Agent Options
%% ----------------------------------------------------------------
do_set_option() ->
    Options = edog_master:get_agent_options(),
    ets:insert(?ETS_TABLE, {options, Options}).

do_get_option() ->
    case ets:lookup(?ETS_TABLE, options) of
        []                   -> [];
        [{options, Options}] -> Options
    end.

do_get_option(Key) ->
    Options = do_get_option(),
    proplists:get_value(Key, Options).

do_get_option(Key, Default) ->
    Options = do_get_option(),
    proplists:get_value(Key, Options, Default).

%% ----------------------------------------------------------------
%%
%% ----------------------------------------------------------------
call_wrapper(Request, Timeout) ->
    try
        gen_server:call(?MODULE, Request, Timeout)
    catch
        Class:Exception ->
            ?ERROR({Class, Exception,
                    Request, Timeout,
                    erlang:get_stacktrace()}),
        {error, Exception}
    end.

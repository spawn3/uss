%% ------------------------------------------------------------
%% @author Dongguanjun <dongguanjun@meidisen.com>
%% @copyright 2010 Meidisen Co.,Ltd
%%
%% @doc
%% - user
%% - log
%% @todo
%% - event manager
%% - send mail
%% - audit
%% @end
%% ------------------------------------------------------------
-module(uss_manager).
-behaviour(gen_server).
-export([
        start_link/0,
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3,

        % Erlang cluster
        master_is_alive/0,
        info/0,
        agent_join/1,
        agent_get_all/0,

        cluster_restart/0,
        cluster_stop/0,
        cluster_deploy/0,
        cluster_check/0,
        hostnames/0,
        get_conf/0,

        % Node
        pm_add/1,
        pm_delete/1,
        pm_info/1,

        % Service
        yfs_add/1,
        yfs_delete/1,
        yfs_start/0,
        yfs_start/1,
        yfs_stop/0,
        yfs_stop/1,

        yfs_get_log/2,
        yfs_clean_log/0,
        yfs_dump_log/0,

        lvm_create/3,
        lvm_resize/3,
        lvm_list/1,

        notify/1
    ]).

-include("uss_common.hrl").

-define(REGNAME,  {global, ?MODULE}).
-define(RPCNAME,  ?REGNAME).
-define(GVAR,     ?MODULE).

%% -----------------------------------------------------------
%% API
%% -----------------------------------------------------------
cluster_restart() ->
    rpc:multicall(init, restart, []).

cluster_stop() ->
    rpc:multicall(init, stop, []).

agent_get_all() ->
    L = ets:match(?GVAR, {{agents, '$1'}, '_', '_'}),
    lists:flatten(L).

master_is_alive() ->
    case global:whereis_name(uss_manager) of
        Pid when is_pid(Pid) ->
            true;
        undefined ->
            global:sync(),
            false
    end.

hostnames() ->
    DataList = ets:match(?GVAR, {{agents, '$1'}, '$2', '$3'}),
    F = fun([Node, Time, Data]) ->
        Host = Data#pm_info.hostname,
        {Node, Time, Host}
    end,
    [F(N) || N <- DataList].

get_conf() ->
    L = uss_yfs:get_conf(),
    {ok, [uss_json:eterm_to_json(R) || R <- L]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cluster_deploy() ->
    gen_server:cast(?RPCNAME, cluster_deploy).

notify(Event) ->
    gen_server:cast(?RPCNAME, {'NOTIFY', Event}).

agent_join(NodeReport) when is_tuple(NodeReport) ->
    gen_server:cast(?RPCNAME, {agent_join, NodeReport}).

cluster_check() ->
    call_wrapper(cluster_check).

info() ->
    call_wrapper({info}).

pm_add(Ip) ->
    call_wrapper({pm_add, Ip}).

pm_delete(Id) ->
    call_wrapper({pm_delete, Id}).

pm_info(Id) ->
    call_wrapper({pm_info, Id}).

yfs_add({Ip, Type, Num}) ->
    call_wrapper({yfs_add, {Ip, Type, Num}}).

yfs_delete({Ip, Type, Num}) ->
    call_wrapper({yfs_delete, {Ip, Type, Num}}).

yfs_start() ->
    yfs_start(all).

yfs_start({_Ip, Type, _N} = Event) when is_atom(Type) ->
    call_wrapper({yfs_start, Event});
yfs_start(Type) when is_atom(Type) ->
    call_wrapper({yfs_start, Type}).

yfs_stop() ->
    yfs_stop(all).

yfs_stop({_Ip, Type, _N} = Sid) when is_atom(Type) ->
    call_wrapper({yfs_stop, Sid});
yfs_stop(Type) when is_atom(Type) ->
    call_wrapper({yfs_stop, Type}).

%% -----------------------------------------------------------
yfs_get_log({Ip, Type, N}, Options) ->
    call_wrapper({yfs_get_log, Ip, Type, N, Options}).

yfs_clean_log() ->
    call_wrapper({yfs_clean_log}).

yfs_dump_log() ->
    call_wrapper({yfs_dump_log}).

%% -----------------------------------------------------------
lvm_create(Name, Size, Options) ->
    call_wrapper({lvm_create, Name, Size, Options}).

lvm_resize(Name, Size, Options) ->
    call_wrapper({lvm_resize, Name, Size, Options}).

lvm_list(Options) ->
    call_wrapper({lvm_list, Options}).

%% -----------------------------------------------------------
%% Callback
%% -----------------------------------------------------------
start_link() ->
    gen_server:start_link(?REGNAME, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    try
        % Global variables
        _ = ets:new(?GVAR, [set, named_table, protected]),

        % http
        inets:start(),
        {ok, LocalPort} = application:get_env(?APPLICATION, manager_web_port),
        uss_web:start(LocalPort),

        % DB
        %mnesia:start(),
        %uss_mnesia:start(),
        mnesia:subscribe(system),
        uss_mnesia:option_init(),

        net_kernel:monitor_nodes(true, [nodedown_reason]),

        gen_event:add_handler(uss_event, uss_event_logger, []),
        gen_event:add_handler(uss_event, uss_event, []),

        ?TTY_INFO_REPORT("uss_manager inited")
    catch
        Error:Exception ->
            ?ERROR_REPORT({Error, Exception, erlang:get_stacktrace()}),
            init:stop()
    end,
    {ok, []}.

%% CLUSTER
handle_call({info} = _Msg, _From, _State) ->
    Reply = [
        {node, node()}
    ],
    {reply, Reply, _State};
handle_call(cluster_check, _From, _State) ->
    Reply = uss_mnesia:cluster_check(),
    {reply, Reply, _State};

%% RACK

%% NODE
handle_call({pm_add, Ip} = _Msg, _From, _State) ->
    Reply = uss_mnesia:pm_add(Ip),
    {reply, Reply, _State};
handle_call({pm_delete, Id} = _Msg, _From, _State) ->
    Reply = uss_mnesia:pm_delete(Id),
    {reply, Reply, _State};
handle_call({pm_info, Id} = _Msg, _From, _State) ->
    %?INFO_REPORT(_Msg),
    Reply = uss_mnesia:pm_info(Id),
    {reply, Reply, _State};

%% YFS
handle_call({yfs_add, {Ip, Type, Num}} = _Msg, _From, _State) ->
    Reply = uss_mnesia:yfs_add({Ip, Type, Num}),
    {reply, Reply, _State};
handle_call({yfs_delete, {Ip, Type, Num}} = _Msg, _From, _State) ->
    Reply = uss_mnesia:yfs_delete({Ip, Type, Num}),
    {reply, Reply, _State};
handle_call({yfs_start, {_Ip, _Type, _N} = _Event} = _Msg, _From, _State) ->
    Reply = uss_mnesia:yfs_start(_Event),
    {reply, Reply, _State};
handle_call({yfs_start, Type} = _Msg, _From, _State) ->
    Reply = uss_mnesia:yfs_start(Type),
    {reply, Reply, _State};
handle_call({yfs_stop, {_Ip, _Type, _N}=Sid} = _Msg, _From, _State) ->
    Reply = uss_mnesia:yfs_stop(Sid),
    {reply, Reply, _State};
handle_call({yfs_stop, Type} = _Msg, _From, _State) ->
    Reply = uss_mnesia:yfs_stop(Type),
    {reply, Reply, _State};

% log
handle_call({yfs_get_log, Ip, Type, N, Options} = _Msg, _From, _State) ->
    Reply =
    case uss_mnesia:is_valid_node(Ip) of
        true ->
            uss_yfs:get_log({Ip, Type, N}, Options);
        false ->
            {error, invalid_node}
    end,
    {reply, Reply, _State};
handle_call({yfs_clean_log} = _Msg, _From, _State) ->
    Reply =
    case uss_mnesia:yfs_services(all, true) of
        [] ->
            ok;
        IPL ->
            lists:foreach(fun(Ip) -> uss_yfs_log:clean_log(Ip, []) end, IPL)
    end,
    {reply, Reply, _State};
handle_call({yfs_dump_log} = _Msg, _From, _State) ->
    Reply =
    case uss_mnesia:yfs_services(all, true) of
        [] ->
            ok;
        IPL ->
            % TODO timeout
            Options = [{ts, cclib_utils:time_string()}],
            lists:foreach(fun(Ip) -> uss_yfs_log:dump_log(Ip, Options) end, IPL)
    end,
    {reply, Reply, _State};

% LVM
handle_call({lvm_create, Name, Size, Options} = _Msg, _From, _State) ->
    Reply =
    case uss_mnesia:any_node() of
        [] -> {error, no_node};
        Node -> uss_yfs:lvm_create(Node, Name, Size, Options)
    end,
    {reply, Reply, _State};
handle_call({lvm_resize, Name, Size, Options} = _Msg, _From, _State) ->
    Reply =
    case uss_mnesia:any_node() of
        [] -> {error, no_node};
        Node -> uss_yfs:lvm_resize(Node, Name, Size, Options)
    end,
    {reply, Reply, _State};
handle_call({lvm_list, Options} = _Msg, _From, _State) ->
    Reply =
    case uss_mnesia:any_node() of
        [] -> {error, no_node};
        Node -> uss_yfs:lvm_list(Node, Options)
    end,
    {reply, Reply, _State};

%%
handle_call(_Msg, _From, _State) ->
    Reply = {error, {unknown, _Msg}},
    {reply, Reply, _State}.

%% -----------------------------------------------------------
handle_cast(cluster_deploy, _State) ->
    F = fun() -> uss_mnesia:cluster_deploy() end,
    spawn(F),
    {noreply, _State};
handle_cast({'NOTIFY', Event}, _State) ->
    F = fun() -> ?EVENT(Event) end,
    spawn(F),
    {noreply, _State};
handle_cast({agent_join, NodeReport}, _State) ->
    do_join(NodeReport),
    {noreply, _State};
handle_cast(_Msg, _State) ->
    ?ERROR_REPORT({unknown, _Msg}),
    {noreply, _State}.

%% -----------------------------------------------------------
handle_info({report, {Node, Time, Data}} = _Msg, _State) ->
    do_report(Node, Time, Data),
    {noreply, _State};
handle_info({nodeup, Node, _InfoList} = _Msg, _State) ->
    F = fun() ->
        ?EVENT(_Msg),
        case is_agent(Node) of
            true ->
                Ip = cclib_utils:get_ip(Node),
                uss_mnesia:do_agent_join(Ip);
            false ->
                ok
        end
    end,
    spawn(F),
    {noreply, _State};
handle_info({nodedown, Node, InfoList} = _Msg, _State) ->
    F = fun() ->
        ?EVENT(_Msg),
        case uss_fence:is_fence_passed() of
            true ->
                case is_agent(Node) of
                    true ->
                        do_leave(Node),
                        uss_mnesia:do_agent_leave(Node, InfoList);
                    false ->
                        ok
                end;
            false ->
                ?TTY_ERROR_REPORT({fence_failed, Node, InfoList}),
                init:stop()
        end
    end,
    spawn(F),
    {noreply, _State};
handle_info({'EXIT', _From, _Reason} = _Msg, _State) ->
    ?INFO_REPORT(_Msg),
    {noreply, _State};
handle_info({mnesia_system_event, _Reason} = _Msg, _State) ->
    ?WARN_REPORT(_Msg),
    {noreply, _State};
handle_info(_Msg, _State) ->
    ?WARN_REPORT({unknown, _Msg}),
    {noreply, _State}.

terminate(_Reason, _State) ->
    {noreply, _State}.

code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.

%% -----------------------------------------------------------
%% DO Method
%% -----------------------------------------------------------
is_agent(Node) ->
    case cclib_utils:get_pair(Node) of
        {?NODE_NAME, _Ip} ->
            true;
        _ ->
            false
    end.

do_join(Node) when is_atom(Node) ->
    do_join({Node, 0, {[],[]}});
do_join({Node, Time, {Data, Services}}) when is_atom(Node) ->
    ets:insert(?GVAR, {{agents, Node}, Time, Data, Services}),
    uss_mnesia:pm_update(Node, Time, Data),
    uss_mnesia:yfs_update({Node, Services}).

do_leave(Node) ->
    case ets:lookup(?GVAR, {agents, Node}) of
        [_] ->
            ets:delete(?GVAR, {agents, Node}),
            ok;
        [] ->
            {error, no_entry}
    end.

-spec do_report(atom(), integer(), {#pm_info{}, list()}) -> any().
do_report(Node, Time, {Data, Services}) ->
    ets:insert(?GVAR, {{agents, Node}, Time, Data, Services}),
    uss_mnesia:pm_update(Node, Time, Data),
    uss_mnesia:yfs_update({Node, Services}),

    ok.

is_profile({info}) ->
    false;
is_profile({pm_info, _}) ->
    false;
is_profile(_) ->
    true.

call_wrapper(Request) ->
    call_wrapper(Request, ?TO_CALL).

call_wrapper(Request, Timeout) ->
    ?PROFILE_BEGIN(),
    Reply =
    try
        gen_server:call(?RPCNAME, Request, Timeout)
    catch
        Class:Exception ->
            ?TTY_ERROR_REPORT({Class, Exception,
                    Request, Timeout,
                    erlang:get_stacktrace()}),
        {error, Class}
    end,
    case is_profile(Request) of
        true -> ?PROFILE_END(Request);
        false -> ok
    end,
    Reply.

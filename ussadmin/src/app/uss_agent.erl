-module(uss_agent).
-behaviour(gen_server).
-export([start_link/0]).
-export([
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
     ]).
-compile(export_all).

-include("uss_common.hrl").

-define(REGNAME, {local, ?MODULE}).
-define(RPCNAME, ?MODULE).
-define(GVAR,    uss_agent).

%% -----------------------------------------------------------
%% API
%% -----------------------------------------------------------
connect_to_master() ->
    case global:whereis_name(?USS_MANAGER) of
        Pid when is_pid(Pid) ->
            uss_manager:agent_join(uss_agent_rt:get_report());
        undefined ->
            {ok, Masters} = clib_utils:get_managers(),
            ?INFO_REPORT({Masters, not_ready}),
            ping_managers(Masters),
            timer:sleep(1000),
            connect_to_master()
    end.

ping_managers(Managers) ->
    lists:map(fun net_adm:ping/1, Managers),
    global:sync().

%% -----------------------------------------------------------
%% Callback
%% -----------------------------------------------------------
start_link() ->
    gen_server:start_link(?REGNAME, ?MODULE, [], []).

init(_Args) ->
    ?TTY_INFO_REPORT("uss_agent init ..."),
    process_flag(trap_exit, true),
    try
        uss_nif:start(),

        %{ok, Port} = clib_utils:get_env(agent_trace_port),
        %?start_trace(Port),

        Root = code:lib_dir(?APPLICATION),
        Logs = lists:flatten(io_lib:format("~s/../data/logs_agent", [Root])),
        cclib_utils:ensure_dir(Logs),

        cclib_os:check_prerequired(),

        application:start(os_mon),

        {ok, Masters} = clib_utils:get_managers(),
        connect_to_master(),

        _ = ets:new(?GVAR, [set, named_table, protected]),
        ets:insert(?GVAR, {managers, Masters}),

        ?TTY_INFO_REPORT("uss_agent inited")
    catch
        Error:Exception ->
            ?TTY_INFO_REPORT({Error, Exception, erlang:get_stacktrace()}),
            init:stop()
    end,

    {ok, []}.

handle_call(_Msg, _From, _State) ->
    ?INFO_REPORT({_Msg, _From, _State}),
    Reply = _Msg,
    {reply, Reply, _State}.

handle_cast(_Msg, _State) ->
    {noreply, _State}.

handle_info(_Msg, _State) ->
    {noreply, _State}.

terminate(_Reason, _State) ->
    {noreply, _State}.

code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


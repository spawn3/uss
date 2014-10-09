-module(edog_slaves_rt).
-behaviour(gen_fsm).
-export([
        start_link/0,
        init/1,
        handle_event/3,
        handle_sync_event/4,
        handle_info/3,
        terminate/3,
        code_change/4]).
-compile(export_all).

-include("edog_common.hrl").

-record(state, {
        manager_pings=0,
        interval = ?EDOG_AGENT_LOOP_INTERVAL
    }).

%% -----------------------------------------------------------------
%% API
%% -----------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%% -----------------------------------------------------------------
%% Callbacks
%% -----------------------------------------------------------------
init(_Args) ->
    process_flag(trap_exit, true),
    Interval = ?EDOG_AGENT_LOOP_INTERVAL,
    State = #state{interval=Interval},
    ?INFO({?MODULE, inited}),
    {ok, loop_it, State, ?EDOG_AGENT_START_INTERVAL}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

handle_event(_Event, _StateName, _StateData) ->
    {next_state, _StateName, _StateData, _StateData#state.interval}.

handle_sync_event(_Event, _From, _StateName, _StateData) ->
    {next_state, _StateName, _StateData, _StateData#state.interval}.

handle_info(_Info, _StateName, _StateData) ->
    {next_state, _StateName, _StateData, _StateData#state.interval}.

code_change(_OldVsn, _StateName, _StateData, _Extra) ->
    {ok, _StateName, _StateData}.

%% states
loop_it(timeout, #state{interval=Interval} = _StateData) ->
    try
        case edog_cluster:is_manager_alive() of
            true ->
                edog_slaves:set_master_info(),
                report();
            false ->
                Managers = edog_cluster:get_manager_nodes(),
                ?WARN({Managers, not_ready}),
                edog_slaves:ping_masters(Managers)
        end
    catch
        Error:Exception ->
            ?ERROR({Error, Exception, erlang:get_stacktrace()})
    end,
    {next_state, loop_it, _StateData, Interval}.

report() ->
    Data = get_report(),
    Time = cclib_utils:now_to_integer(),
    global:send(edog_master, {report, node(), Time, Data}).

get_report() ->
    Uptime = cclib_os:uptime(),
    [
        {hostname, net_adm:localhost()},
        {libvirtd, cclib_utils:is_process_exists("libvirtd")},
        %{proxy,    cclib_utils:is_process_exists("proxy_server")},
        {avg1,     Uptime#uptime_info.avg1},
        {avg5,     Uptime#uptime_info.avg5},
        {avg15,    Uptime#uptime_info.avg15},
        {util,     cpu_sup:util([per_cpu])},
        {mem,      memsup:get_system_memory_data()},
        {disk,     disksup:get_disk_data()},
        {domain,   edog_libvirt:domain_info()},
        {bridge,   cclib_network:info_bridge()},
        {netflow,  cclib_network:netflow()}
    ].

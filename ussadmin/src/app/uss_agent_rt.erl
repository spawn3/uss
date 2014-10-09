-module(uss_agent_rt).
-behaviour(gen_fsm).
-export([
        start_link/0,
        init/1,
        handle_event/3,
        handle_sync_event/4,
        handle_info/3,
        terminate/3,
        code_change/4,

        get_report/0
    ]).
-compile(export_all).

-include("uss_common.hrl").

-define(REGNAME, {local, ?MODULE}).
-define(RPCNAME, ?MODULE).

-record(statedata, {
        timeout=?EDOG_AGENT_TIMEOUT,
        atime=0,
        net_info=[],
        network,
        netmask
    }).

%% -----------------------------------------------------------------
%% API
%% -----------------------------------------------------------------

%% -----------------------------------------------------------------
%% Callbacks
%% -----------------------------------------------------------------
start_link() ->
    gen_fsm:start_link(?REGNAME, ?MODULE, [], []).

init([]) ->
    %{ok, Timeout} = clib_utils:get_env(edog_agent_timeout, ?EDOG_AGENT_TIMEOUT),
    Timeout = 10,

    File = filename:join([?YFS_PREFIX, "etc/yfs.conf"]),
    Network2 =
    case uss_yfs_conf:get_value(File, "network") of
        {ok, Network} -> Network;
        {error, _} -> undefined
    end,
    Netmask2 =
    case uss_yfs_conf:get_value(File, "mask") of
        {ok, Netmask} -> Netmask;
        {error, _} -> undefined
    end,

    {ok, loop_it, #statedata{
            timeout=Timeout*1000,
            network=Network2,
            netmask=Netmask2},
        Timeout}.

handle_event(_Event, _StateName, _StateData) -> ok.
handle_sync_event(_, _, _, _) -> ok.
handle_info(_Info, _StateName, _StateData) -> ok.
terminate(_Reason, _StateName, _StateData) -> ok.
code_change(_OldVsn, _StateName, _StateData, _Extra) -> ok.

%% states
loop_it(timeout, #statedata{timeout=Timeout} = StateData) ->
    case global:whereis_name(?USS_MANAGER) of
        Pid when is_pid(Pid) ->
            NewStateData = report(StateData),
            {next_state, loop_it, NewStateData, Timeout};
        undefined ->
            {ok, Masters} = clib_utils:get_managers(),
            lists:map(fun net_adm:ping/1, Masters),
            global:sync(),
            {next_state, loop_it, StateData, Timeout}
    end.

report(StateData) ->
    case global:whereis_name(?USS_MANAGER) of
        Pid when is_pid(Pid) ->
            {NewStateData, Report} = get_report(StateData),
            global:send(?USS_MANAGER, {report, Report}),
            NewStateData;
        undefined ->
            StateData
            %{error, master_not_ready}
    end.

update_net_info({OldTime, OldNetInfo}, {NewTime, NewNetInfo}) ->
    Diff = NewTime - OldTime,
    F = fun(#if_info{mac=Mac, rx_bytes=NewRx, tx_bytes=NewTx} = NewR) ->
            case lists:keyfind(Mac, 3, OldNetInfo) of
                false -> NewR;
                #if_info{rx_bytes=OldRx, tx_bytes=OldTx} ->
                    NewR#if_info{
                        rx_bytes=(NewRx-OldRx) div Diff,
                        tx_bytes=(NewTx-OldTx) div Diff
                    }
            end
    end,
    [F(R) || R <- NewNetInfo].

% for edog_agent
get_report() ->
    element(2, get_report(#statedata{})).

get_report(#statedata{network=Network, netmask=Netmask} = _StateData) ->
    Time = cclib_utils:now_to_integer(),

    %NewNetInfo = cclib_network:info(),
    %NetInfo = update_net_info({_OldTime, _OldNetInfo}, {Time, NewNetInfo}),
    %NewStateData = #statedata{atime=Time, net_info=NewNetInfo},

    Uptime = cclib_os:uptime(),
    Data = #pm_info{
        network = cclib_network:info([{type, ethernet}, {network, Network}, {netmask, Netmask}]),
        hostname = net_adm:localhost(),
        uptime=#uptime{
            time=Uptime#uptime_info.time,
            user=Uptime#uptime_info.user},
        cpu = [
            {load, [
                {avg1, Uptime#uptime_info.avg1},
                {avg5, Uptime#uptime_info.avg5},
                {avg15, Uptime#uptime_info.avg15}]},
            %{used, cclib_os:info(util)},
            {used, cpu_sup:util()/100},
            {spec, cclib_os:info(cpu)}
        ],
        mem = cclib_os:info(mem),
        disk = cclib_os:get_disk_data()
    },
    Services = uss_yfs_service:info(),
    {_StateData, {node(), Time, {Data, Services}}}.

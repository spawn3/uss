-module(edog_master_rt).
-behaviour(gen_fsm).
-export([
        start_link/0,
        stop/0,
        init/1,
        handle_event/3,
        handle_sync_event/4,
        handle_info/3,
        terminate/3,
        code_change/4
    ]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("edog_common.hrl").

-record(state, {
        count,
        is_first_check_done
    }).

%% -----------------------------------------------------------------
%% API
%% -----------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(Args) ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
    ok.


%% -----------------------------------------------------------------
%% Callbacks
%% -----------------------------------------------------------------
init(Args) ->
    process_flag(trap_exit, true),
    State =
    case is_normal(Args) of
        true  -> #state{count=0, is_first_check_done=false};
        false -> #state{count=0, is_first_check_done=true}
    end,
    ?INFO({Args, State}),
    ?INFO({?MODULE, "inited"}),
    {ok, loop_it, State, ?EDOG_MASTER_START_INTERVAL}.

is_normal(Args) ->
    proplists:get_bool(normal, Args).

terminate(_Reason, _StateName, _StateData) ->
    ok.

handle_event(_Event, _StateName, _StateData) ->
    {next_state, _StateName, _StateData, ?EDOG_MASTER_LOOP_INTERVAL}.

handle_sync_event(_Event, _From, _StateName, _StateData) ->
    {next_state, _StateName, _StateData, ?EDOG_MASTER_LOOP_INTERVAL}.

handle_info(_Info, _StateName, _StateData) ->
    {next_state, _StateName, _StateData, ?EDOG_MASTER_LOOP_INTERVAL}.

code_change(_OldVsn, _StateName, _StateData, _Extra) ->
    {ok, _StateName, _StateData}.

%% states
loop_it(timeout, #state{count=Count} = _StateData) ->
    NewStateData =
    try
        do_routine(_StateData)
    catch
        Error:Exception ->
            ?ERROR({Error, Exception, erlang:get_stacktrace()}),
            _StateData
    end,
    {next_state, loop_it, NewStateData#state{count=Count+1},
        ?EDOG_MASTER_LOOP_INTERVAL}.

%% --------------------------------------------------------------------------
%% DO
%% --------------------------------------------------------------------------
-define(WAIT_FOR_NODES, 2).

do_routine(#state{count=Count, is_first_check_done=FirstDone} = StateData) ->
    if
        Count >= ?WAIT_FOR_NODES ->
            vm_check(FirstDone),
            StateData#state{is_first_check_done=true};
        true ->
            ?INFO(StateData),
            StateData
    end.

%%
vm_check(FirstDone) ->
    Vms = edog_table_vm:all(),
    lists:foreach(fun(Vm) -> do_vm_check(Vm, FirstDone) end, Vms).

do_vm_check(#vm_t{vm_id=VmID, vm_name=VmName, pm_id=PmID, port=Port, status=State} = Vm,
    FirstDone) ->
    if
        State =:= ?VM_STATE_RUNNING orelse State =:= ?VM_STATE_PAUSED,
        length(PmID) > 0, Port > 0 ->
            case edog_libvirt:vm_state(PmID, VmID, VmName) of
                {ok, State} ->
                    ok;
                {ok, NewState} ->
                    ?WARN({vm_check, Vm, State, NewState, FirstDone}),
                    % TODO
                    case FirstDone of
                        true ->
                            global:send(edog_master, {vm_check, Vm, PmID, NewState});
                        false ->
                            edog_table_vm:update_state(VmID, ?VM_STATE_SHUTOFF)
                    end;
                {error, _Reason} ->
                    % {error, nodedown}
                    {error, _Reason}
            end;
        State =:= ?VM_STATE_SHUTOFF, length(PmID) =:= 0, Port =:= -1 ->
            case edog_table_vm:get_restart(Vm) of
                true ->
                    do_vm_restart(Vm);
                false ->
                    ok
            end;
        State =:= ?VM_STATE_CREATING,
        length(PmID) =:= 0 orelse PmID =:= undefined, Port =:= -1 ->
            ok;
        true ->
            ?ERROR({Vm, FirstDone}),
            ok
    end.

do_vm_restart(#vm_t{vm_id=VmId} = Vm) ->
    Opts = [
        {wait_time, 3},
        {memory_policy, edog_conf:vm_restart_memory_policy()}
    ],
    ?WARN({oops, restart, Vm, Opts}),
    case is_vm_running(Vm) of
        true ->
            ok;
        false ->
            edog_select:push({vm_start, VmId, Opts}),
            edog_table_vm:set_restart(VmId)
    end.

is_vm_running(#vm_t{}=Vm) ->
    F = fun(Ip) ->
        Res = edog_libvirt:vm_state(Ip, Vm#vm_t.vm_id, Vm#vm_t.vm_name),
        case Res of
            {ok, ?VM_STATE_PAUSED} ->
                ?WARN({oops, Ip, Vm, Res}),
                true;
            {ok, ?VM_STATE_RUNNING} ->
                ?WARN({oops, Ip, Vm, Res}),
                true;
            _ ->
                false
        end
    end,
    Pms = edog_mnesia:get_nodes(),
    L = cclib_pmap:pmap(F, Pms),
    lists:any(fun(X) -> X=:=true end, L).

%% ------------------------------------------------------------------
%% TEST
%% ------------------------------------------------------------------
vms_test() ->
    Vms = edog_table_vm:all(),
    lists:foreach(fun is_vm_running/1, Vms).

tpl() ->
    cclib_dbg:start(),
    cclib_dbg:tpl(?MODULE, loop_it),
    cclib_dbg:tpl(?MODULE, vm_check),
    cclib_dbg:tpl(?MODULE, do_routine),
    cclib_dbg:tpl(?MODULE, do_vm_check),
    ok.

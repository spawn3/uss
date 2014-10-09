-module(edog_vm).
-behaviour(gen_fsm).
-export([
        start_link/2,
        init/1,
        handle_event/3,
        handle_sync_event/4,
        handle_info/3,
        terminate/3,
        code_change/4]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("edog_common.hrl").

-define(reply_errror(Op, Key, Reason),
    begin
        _Reply = {error, Reason},
        edog_fg:notify(#notify_spec{op=Op, key=Key, reply=_Reply}),
        io:format("**REPLY** {~p, ~p,~p}: ~p~n", [node(), ?MODULE, ?LINE, _Reply]),
        _Reply
    end).

-define(ETS_TABLE, edog_vm).
-define(DONE, '$done').

-record(vm_info, {
        vmid,
        vmname,
        ip,
        status,
        port
    }).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------
% from report
vm_check(Node, #domain{uuid=VmId, status=NewState} = _Domain) when is_atom(Node) ->
    case edog_table_vm:lookup(VmId) of
        [Vm] ->
            vm_check(Vm, Node, NewState);
        [] ->
            ?ERROR({not_in_db, _Domain}),
            {error, not_in_db}
    end.

% from edog_master_rt
vm_check(#vm_t{} = Vm, IP, NewState) when is_list(IP) ->
    Node = edog_common:get_node(IP),
    vm_check(Vm, Node, NewState);
vm_check(#vm_t{vm_id=VmId} = Vm, Node, NewState) when is_atom(Node) ->
    case ensure_actor(vm_check, VmId) of
        {ok, Pid} when is_pid(Pid) ->
            gen_fsm:send_all_state_event(Pid, {vm_check, Vm, Node, NewState});
        {error, _Reason} ->
            {error, _Reason}
    end.

%% vm_create | vm_update
vm_action({Op, #vm_t{vm_id=VmId} = _Vm, _Opts} = _Event) ->
    case ensure_actor(Op, VmId) of
        {ok, Pid} when is_pid(Pid) ->
            Reply = sync_send_event_wrapper(Pid, _Event),
            case Reply of
                {ok, _} when Op =:= vm_update ->
                    edog_fg:notify(#notify_spec{op=Op, key=VmId, reply=Reply}),
                    ?REPLY(ok);
                {ok, _} ->
                    edog_fg:notify(#notify_spec{op=Op, key=VmId, reply=Reply}),
                    ?REPLY(ok);
                {error, _Reason} ->
                    ?reply_errror(Op, VmId, _Reason)
            end;
        {error, _Reason} ->
            {error, _Reason}
    end;
%% vm_start | vm_stop | vm_migrate | vm_setvcpus | vm_setmem
vm_action({Op, VmId, _Opts} = _Event) ->
    case ensure_actor(Op, VmId) of
        {ok, Pid} when is_pid(Pid) ->
            Reply = sync_send_event_wrapper(Pid, _Event),
            case Reply of
                {ok, _} ->
                    ?REPLY(ok);
                {error, _Reason} ->
                    ?reply_errror(Op, VmId, _Reason)
            end;
        {error, _Reason} ->
            {error, _Reason}
    end;
%% vm_start|vm_migrate|vm_stop|vm_pause|vm_resume|vm_destroy
vm_action({Op, VmId} = _Event) ->
    case ensure_actor(Op, VmId) of
        {ok, Pid} when is_pid(Pid) ->
            Reply = sync_send_event_wrapper(Pid, _Event),
            case Reply of
                {ok, _} when Op =:= vm_destroy ->
                    supervisor:delete_child(edog_vmsup, cclib_utils:to_atom(VmId)),
                    edog_fg:notify(#notify_spec{op=Op, key=VmId, reply=Reply}),
                    ?REPLY(ok);
                {ok, _} ->
                    ?REPLY(ok);
                {error, _Reason} ->
                    ?reply_errror(Op, VmId, _Reason)
            end;
        {error, _Reason} ->
            {error, _Reason}
    end.

stop(VmId) ->
    case ensure_actor(vm_dummy, VmId) of
        {ok, Pid} when is_pid(Pid) ->
            sync_send_all_state_event_wrapper(Pid, stop);
        {error, _Reason} ->
            {error, _Reason}
    end.

notify(#notify_spec{op=Op, key=_VmId, reply=Reply}) ->
    case ensure_actor(Op, _VmId) of
        {ok, Pid} when is_pid(Pid) ->
            gen_fsm:send_event(Pid, {?DONE, Op, Reply});
        {error, _Reason} ->
            {error, _Reason}
    end.

get_statusdata(VmId) when is_atom(VmId) ->
    get_statusdata(atom_to_list(VmId));
get_statusdata(VmId) ->
    case ensure_actor(vm_dummy, VmId) of
        {ok, Pid} when is_pid(Pid) ->
            sync_send_all_state_event_wrapper(Pid, get_statusdata);
        {error, _Reason} ->
            {error, _Reason}
    end.

get_status(VmId) when is_atom(VmId) ->
    get_status(atom_to_list(VmId));
get_status(VmId) ->
    case ensure_actor(vm_dummy, VmId) of
        {ok, Pid} when is_pid(Pid) ->
            sync_send_all_state_event_wrapper(Pid, get_status);
        {error, _Reason} ->
            {error, _Reason}
    end.

get_status() ->
    L = supervisor:which_children(edog_vmsup),
    lists:foreach(
        fun({Actor, _Pid, _Type, _Mod}) ->
                Status = sys:get_status(Actor),
                ?INFO(Status)
        end, L).

is_processing(VmId) ->
    case get_status(VmId) of
        ?VM_STATE_PROCESSING -> true;
        _          -> false
    end.

processings() ->
    L = ets:tab2list(?ETS_TABLE),
    F = fun(Actor) ->
        case get_statusdata(Actor) of
            {?VM_STATE_PROCESSING, Data} -> Data#vm_state.context;
            _                  -> false
        end
    end,
    [X || X <- [F(Actor) || {Actor, _StateData} <- L], X =/= false].

% Ip:{Cpu, Memory}
get_processing_dict() ->
    L = processings(),
    ?INFO({processing, L}),
    get_processing_dict(L, dict:new()).

get_processing_dict([], Dict) ->
    Dict;
get_processing_dict([{vm_start, ContextOpts}|T], Dict) ->
    Vm     = proplists:get_value(vm, ContextOpts),
    DestIp = proplists:get_value(dest_ip, ContextOpts),

    Cpu = Vm#vm_t.vm_cpu,
    Mem = edog_table_vm:get_real_memory(Vm),
    {NewCpu, NewMem} =
    case dict:find(DestIp, Dict) of
        {ok, {OldCpu, OldMem}} -> {OldCpu+Cpu, OldMem+Mem};
        _                      -> {Cpu, Mem}
    end,
    NewDict = dict:store(DestIp, {NewCpu, NewMem}, Dict),
    get_processing_dict(T, NewDict);
get_processing_dict([{vm_migrate, ContextOpts}|T], Dict) ->
    Vm     = proplists:get_value(vm, ContextOpts),
    DestIp = proplists:get_value(dest_ip, ContextOpts),

    Cpu = Vm#vm_t.vm_cpu,
    Mem = edog_table_vm:get_real_memory(Vm),
    {NewCpu, NewMem} =
    case dict:find(DestIp, Dict) of
        {ok, {OldCpu, OldMem}} -> {OldCpu+Cpu, OldMem+Mem};
        _                      -> {Cpu, Mem}
    end,
    NewDict = dict:store(DestIp, {NewCpu, NewMem}, Dict),
    get_processing_dict(T, NewDict);
get_processing_dict([_H|T], Dict) ->
    get_processing_dict(T, Dict).

trace(Flag) ->
    L = supervisor:which_children(edog_vmsup),
    lists:foreach(fun({Actor, _Pid, _Type, _Mod}) -> sys:trace(Actor, Flag) end, L).

list_status() ->
    L = supervisor:which_children(edog_vmsup),
    [{Actor, get_statusdata(Actor)} || {Actor, _Pid, _Type, _Mod} <- L].

list_status(Status) when is_atom(Status) ->
    L = supervisor:which_children(edog_vmsup),
    [{Actor, Data#vm_state.context} || {Actor, _Pid, _Type, _Mod} <- L,
        begin
            {Status1, Data} = get_statusdata(Actor),
            Status1 =:= Status
        end].

delete_child(VmId) ->
    supervisor:terminate_child(edog_vmsup, VmId),
    supervisor:delete_child(edog_vmsup, VmId).

%% -----------------------------------------------------------------
%% SYNC
%% -----------------------------------------------------------------
sync_send_event_wrapper(Pid, Event) ->
    ?PROFILE_BEGIN(),
    Reply = gen_fsm:sync_send_event(Pid, Event, ?TO_CALL),
    ?PROFILE_END(Event),
    Reply.

sync_send_all_state_event_wrapper(Pid, Msg) ->
    try
        gen_fsm:sync_send_all_state_event(Pid, Msg, ?TO_CALL)
    catch
        Class:Pattern ->
            ?WARN({Class, Pattern, erlang:get_stacktrace(), Pid, Msg, erlang:process_info(Pid)}),
            {error, Pattern}
    end.

%% -----------------------------------------------------------------
%% Callbacks
%% -----------------------------------------------------------------
fsm_reply(Reply, CurrState, NextState, #vm_state{actor=Actor} = StateData) ->
    if
        CurrState =/= ?VM_STATE_PROCESSING, NextState =:= ?VM_STATE_PROCESSING ->
            ets:insert(?ETS_TABLE, {Actor, StateData});
        CurrState =:= ?VM_STATE_PROCESSING, NextState =/= ?VM_STATE_PROCESSING ->
            ets:delete(?ETS_TABLE, Actor);
        true ->
            ok
    end,
    NewStateData = StateData#vm_state{prev_state=CurrState},
    {reply, Reply, NextState, NewStateData}.

fsm_next_state(CurrState, NextState, #vm_state{actor=Actor} = StateData) ->
    if
        CurrState =/= ?VM_STATE_PROCESSING, NextState =:= ?VM_STATE_PROCESSING ->
            ets:insert(?ETS_TABLE, {Actor, StateData});
        CurrState =:= ?VM_STATE_PROCESSING, NextState =/= ?VM_STATE_PROCESSING ->
            ets:delete(?ETS_TABLE, Actor);
        true ->
            ok
    end,
    NewStateData = StateData#vm_state{prev_state=CurrState},
    {next_state, NextState, NewStateData}.

fsm_reply(Reply, NextState, StateData) ->
    fsm_reply(Reply, NextState, NextState, StateData).

fsm_next_state(NextState, StateData) ->
    fsm_next_state(NextState, NextState, StateData).

fsm_stop(Reason, #vm_state{actor=Actor} = StateData) ->
    ets:delete(?ETS_TABLE, Actor),
    {stop, Reason, StateData}.

fsm_stop_reply(Reason, Reply, StateData) ->
    {stop, Reason, Reply, StateData}.

start_link(Op, VmId) ->
    Actor = cclib_utils:to_atom(VmId),
    StateData = #vm_state{actor=Actor, key=VmId, init_op=Op, context=[]},
    gen_fsm:start_link({local, Actor}, ?MODULE, StateData, []).

init(#vm_state{actor=_Actor, key=VmId, init_op=Op} = StateData) ->
    process_flag(trap_exit, true),

    %% determine vm_state
    case cclib_mnesia:lookup(vm_t, VmId) of
        [#vm_t{status=Status}] ->
            {ok, Status, StateData};
        [] when Op =:= vm_create ->
            {ok, ?VM_STATE_CREATING, StateData};
        [] ->
            {stop, Op}
    end.

terminate(_Reason, _StateName, #vm_state{} = _StateData) ->
    ok.

%% -----------------------------------------------------------------
%% creating
%% -----------------------------------------------------------------
creating({vm_create, #vm_t{cust_id=CustId, vm_id=VmId, vm_name=VmName, vm_pcis=Pcis, vm_imgid=BootDisk} = Vm,
        Opts}, _From, _StateData) ->
    Disks    = proplists:get_value(disks, Opts),
    IoDriver = proplists:get_value(iodriver, Opts),

    DevPrefix = edog_table_vm:device_prefix(IoDriver),
    DevFirst = edog_table_vm:device_first(IoDriver),

    DiskNum = length(Disks),
    Disks1 = lists:zip(Disks, lists:seq(1, DiskNum)),
    Disks2 = [X#disk_t{target=DevPrefix ++ [$a+Y]} || {X,Y} <- Disks1],

    F = fun() ->
        {Sync, NewDisk} =
        case mnesia:read({disk_t, BootDisk}) of
            [Disk] ->
                {true, Disk#disk_t{status=?DISK_STATE_CREATED}};
            [] ->
                case mnesia:read({stddisk_t, BootDisk}) of
                    [#stddisk_t{path=Src, size=Size, os_type=_Type, os_version=_Ver}] ->
                        Ns =
                        case mnesia:read({cust_t, CustId}) of
                            [#cust_t{cust_home=Home}] -> Home;
                            [] -> mnesia:abort(no_such_user)
                        end,
                        {GlobalDiskId, NewDiskId} = edog_storage_util:diskid(Ns, VmName),
                        case edog_disk:lookup(GlobalDiskId) of
                            [] ->
                                case edog_storage:copy_file(Src, Ns, NewDiskId) of
                                    {ok, _} ->
                                        {false, #disk_t{
                                            cust_id=CustId,
                                            disk_id=GlobalDiskId,
                                            disk_alias=VmName,
                                            path=edog_storage_util:disk_to_path(Ns, NewDiskId),
                                            size=Size,
                                            canboot=[boot],
                                            status=?VM_STATE_CREATING,
                                            createtime=cclib_utils:now_to_integer()
                                        }};
                                    {error, _Reason} ->
                                        mnesia:abort(_Reason)
                                end;
                            [#disk_t{}] ->
                                mnesia:abort(disk_already_exists)
                        end;
                    [] ->
                        mnesia:abort(boot_disk_not_exists)
                end
        end,
        % update system disk
        % update data disks
        % TODO
        mnesia:write(NewDisk#disk_t{vm_id=VmId}),
        case edog_disk:add_owner([NewDisk#disk_t{target=DevFirst}|Disks2], VmId, [reset]) of
            {atomic, Value} ->
                Value;
            {aborted, Reason} ->
                mnesia:abort(Reason)
        end,

        %% TODO
        %% Bridge = edog_master:gvar_get(bridge, "virbr0"),
        %% Pcis1 = [X#nic_t{mac=cclib_uuid:to_mac(Mac)} || #nic_t{mac=Mac} = X <- Pcis],
        Status = case Sync of
            true  -> ?VM_STATE_SHUTOFF;
            false -> ?VM_STATE_CREATING
        end,
        mnesia:write(Vm#vm_t{vm_pcis=Pcis, vm_imgid=NewDisk#disk_t.disk_id,
                status=Status, extra=[{iodriver, IoDriver}]}),
        Sync
    end,
    case mnesia:transaction(F) of
        {atomic, true} ->
            fsm_reply({ok, true}, ?VM_STATE_CREATING, ?VM_STATE_SHUTOFF, _StateData);
        {atomic, false} ->
            NewStateData = _StateData#vm_state{context={vm_create}},
            fsm_reply({ok, true}, ?VM_STATE_CREATING, ?VM_STATE_PROCESSING, NewStateData);
        {aborted, _Reason} ->
            fsm_reply({error, _Reason}, ?VM_STATE_CREATING, _StateData)
    end;
creating({vm_destroy, VmId}, _From, _StateData) ->
    case do_vm_destroy(VmId) of
        {ok, _} ->
            fsm_stop_reply(normal, {ok, true}, _StateData);
        {error, _Reason} ->
            fsm_reply({error, _Reason}, ?VM_STATE_CREATING, _StateData)
    end;
creating(_Event, _From,  #vm_state{} = _StateData) ->
    ?ERROR({_Event, _From, _StateData}),
    fsm_reply({error, ?VM_STATE_CREATING}, ?VM_STATE_CREATING, _StateData).

%% -----------------------------------------------------------------
%% shutoff
%% -----------------------------------------------------------------
shutoff({vm_update, #vm_t{vm_id=VmId, vm_pcis=Pcis, vm_imgid=BootDisk} = Vm, Opts},
    _From, _StateData) ->
    Disks    = proplists:get_value(disks, Opts),
    IoDriver = proplists:get_value(iodriver, Opts),

    DevPrefix = edog_table_vm:device_prefix(IoDriver),
    DevFirst  = edog_table_vm:device_first(IoDriver),

    DiskNum = length(Disks),
    Disks1 = lists:zip(Disks, lists:seq(1, DiskNum)),
    Disks2 = [ X#disk_t{target=DevPrefix ++ [$a+Y]} || {X,Y} <- Disks1 ],

    F = fun() ->
        case mnesia:read(vm_t, VmId) of
            [#vm_t{status=?VM_STATE_SHUTOFF} = OldVm] ->
                case edog_disk:add_owner([#disk_t{disk_id=BootDisk, target=DevFirst}|Disks2],
                        VmId, [reset]) of
                    {atomic, Value} ->
                        Value;
                    {aborted, Reason} ->
                        mnesia:abort(Reason)
                end,
                %% TODO
                %% Bridge = edog_master:gvar_get(bridge, "virbr0"),
                %% Pcis1 = [X#nic_t{mac=cclib_uuid:to_mac(Mac)} || #nic_t{mac=Mac} = X <- Pcis],
                mnesia:write(OldVm#vm_t{
                        vm_name  = Vm#vm_t.vm_name,
                        vm_cpu   = Vm#vm_t.vm_cpu,
                        vm_mem   = Vm#vm_t.vm_mem,
                        vm_imgid = Vm#vm_t.vm_imgid,
                        vm_pcis  = Pcis,
                        status   = ?VM_STATE_SHUTOFF,
                        extra    = [{iodriver, IoDriver}]
                    });
            [#vm_t{}] ->
                mnesia:abort(vm_is_not_shutoff);
            [] ->
                mnesia:abort(no_such_vm)
        end
    end,
    case mnesia:transaction(F) of
        {atomic, _} ->
            fsm_reply({ok, true}, ?VM_STATE_SHUTOFF, _StateData);
        {aborted, _Reason} ->
            fsm_reply({error, _Reason}, ?VM_STATE_SHUTOFF, _StateData)
    end;
shutoff({vm_destroy, VmId}, _From, _StateData) ->
    case do_vm_destroy(VmId) of
        {ok, _} ->
            fsm_stop_reply(normal, {ok, true}, _StateData);
        {error, _Reason} ->
            fsm_reply({error, _Reason}, ?VM_STATE_SHUTOFF, _StateData)
    end;
shutoff({vm_start, VmId, Options}, _From, _StateData) ->
    case do_vm_start(VmId, Options) of
        {ok, ContextOpts} ->
            NewStateData = _StateData#vm_state{context={vm_start, ContextOpts}},
            fsm_reply({ok, true}, ?VM_STATE_SHUTOFF, ?VM_STATE_PROCESSING, NewStateData);
        {error, _Reason} ->
            fsm_reply({error, _Reason}, ?VM_STATE_SHUTOFF, _StateData)
    end;
shutoff(_Event, _From, _StateData) ->
    ?ERROR({_Event, _From, _StateData}),
    fsm_reply({error, ?VM_STATE_SHUTOFF}, ?VM_STATE_SHUTOFF, _StateData).

%% -----------------------------------------------------------------
%% running
%% -----------------------------------------------------------------
running({vm_stop, VmId, Opts}, _From, _StateData) ->
    case do_vm_stop(VmId, Opts) of
        {ok, _} ->
            NewStateData = _StateData#vm_state{context={vm_stop}},
            fsm_reply({ok, true}, ?VM_STATE_RUNNING, ?VM_STATE_PROCESSING, NewStateData);
        {error, _Reason} ->
            fsm_reply({error, _Reason}, ?VM_STATE_RUNNING, _StateData)
    end;
running({vm_pause, _VmId}, _From, _StateData) ->
    case cclib_mnesia:lookup(vm_t, _VmId) of
        [#vm_t{vm_name=VmName, pm_id=Ip}] ->
            edog_libvirt:vm_pause(Ip, _VmId, VmName)
    end,
    NewStateData = _StateData#vm_state{context={vm_pause}},
    fsm_reply({ok, true}, ?VM_STATE_RUNNING, ?VM_STATE_PROCESSING, NewStateData);
running({vm_migrate, VmId, DestIp}, _From, _StateData) ->
    case check_before_migrate(DestIp) of
        {error, Reason} ->
            fsm_reply({error, Reason}, ?VM_STATE_RUNNING, _StateData);
        {ok, _} ->
            case do_vm_migrate(VmId, DestIp) of
                {ok, ContextOpts} ->
                    NewStateData = _StateData#vm_state{context={vm_migrate, ContextOpts}},
                    fsm_reply({ok, true}, ?VM_STATE_RUNNING,
                        ?VM_STATE_PROCESSING, NewStateData);
                {error, _Reason} ->
                    fsm_reply({error, _Reason}, ?VM_STATE_RUNNING, _StateData)
            end
    end;
running(_Event, _From, #vm_state{} = _StateData) ->
    ?ERROR({_Event, _From, _StateData}),
    Reply =
    case is_restart(_StateData) of
        true  -> {ok, true};
        false -> {error, ?VM_STATE_RUNNING}
    end,
    fsm_reply(Reply, ?VM_STATE_RUNNING, _StateData).

%% -----------------------------------------------------------------
%% paused
%% -----------------------------------------------------------------
paused({vm_resume, _VmId}, _From, _StateData) ->
    case cclib_mnesia:lookup(vm_t, _VmId) of
        [#vm_t{vm_name=VmName, pm_id=Ip}] ->
            edog_libvirt:vm_resume(Ip, _VmId, VmName)
    end,
    NewStateData = _StateData#vm_state{context={vm_resume}},
    fsm_reply({ok, true}, ?VM_STATE_PAUSED, ?VM_STATE_PROCESSING, NewStateData);
paused(_Event, _From, _StateData) ->
    ?ERROR({_Event, _From, _StateData}),
    fsm_reply({error, ?VM_STATE_PAUSED}, ?VM_STATE_PAUSED, _StateData).

%% -----------------------------------------------------------------
%% processing
%% -----------------------------------------------------------------
processing({?DONE, vm_create, _Reply}, #vm_state{key=_VmId} = _StateData) ->
    case _Reply of
        {ok, _} ->
            edog_table_vm:update_state(_VmId, ?VM_STATE_SHUTOFF),
            fsm_next_state(?VM_STATE_PROCESSING, ?VM_STATE_SHUTOFF, _StateData);
        {error, _Reason} ->
            fsm_stop(normal, _StateData)
    end;
processing({?DONE, vm_destroy, _Reply}, _StateData) ->
    case _Reply of
        {ok, _} ->
            fsm_stop(normal, _StateData);
        {error, _Reason} ->
            fsm_next_state(?VM_STATE_PROCESSING, ?VM_STATE_SHUTOFF, _StateData)
    end;
processing({?DONE, vm_start, _Reply}, #vm_state{key=_VmId, context={vm_start, ContextOpts}} = _StateData) ->
    Disks = proplists:get_value(disks, ContextOpts),
    unlock_disks(Disks),
    DestIp = proplists:get_value(dest_ip, ContextOpts),
    case _Reply of
        {ok, Port} when is_integer(Port) ->
            edog_table_vm:update_state(_VmId, ?VM_STATE_RUNNING),
            edog_table_vm:update_pmid(_VmId, DestIp, Port),
            edog_table_vm:clear_restart(_VmId),
            fsm_next_state(?VM_STATE_PROCESSING, ?VM_STATE_RUNNING, _StateData);
        %{error, nodedown} ->
        %    Vm = proplists:get_value(vm, ContextOpts),
        %    Node = edog_common:get_node(DestIp),
        %    ?WARN({restart_again, Vm, Node}),
        %    do_vm_restart(Vm, Node),
        %    fsm_next_state(?VM_STATE_PROCESSING, ?VM_STATE_SHUTOFF, _StateData);
        {error, _Reason} ->
            ?ERROR({oops, _Reason}),
            %% FIXME
            edog_table_vm:update_state(_VmId, ?VM_STATE_SHUTOFF),
            fsm_next_state(?VM_STATE_PROCESSING, ?VM_STATE_SHUTOFF, _StateData)
    end;
processing({?DONE, vm_stop, _Reply}, #vm_state{key=_VmId} = _StateData) ->
    case _Reply of
        {ok, _} ->
            edog_table_vm:update_state(_VmId, ?VM_STATE_SHUTOFF),
            fsm_next_state(?VM_STATE_PROCESSING, ?VM_STATE_SHUTOFF, _StateData);
        {error, _Reason} ->
            fsm_next_state(?VM_STATE_PROCESSING, ?VM_STATE_RUNNING, _StateData)
    end;
processing({?DONE, vm_migrate, _Reply}, #vm_state{key=_VmId, context={vm_migrate, ContextOpts}} = _StateData) ->
    Disks = proplists:get_value(disks, ContextOpts),
    unlock_disks(Disks),
    case _Reply of
        {ok, {PmId, Port}} ->
            edog_table_vm:update_pmid(_VmId, PmId, Port),
            fsm_next_state(?VM_STATE_PROCESSING, ?VM_STATE_RUNNING, _StateData);
        {ok, {PmId, Port}, _Diff} ->
            edog_table_vm:update_pmid(_VmId, PmId, Port),
            fsm_next_state(?VM_STATE_PROCESSING, ?VM_STATE_RUNNING, _StateData);
        {error, _Reason} ->
            _Vm     = proplists:get_value(vm, ContextOpts),
            _SrcIP  = proplists:get_value(src_ip, ContextOpts),
            _DestIP = proplists:get_value(dest_ip, ContextOpts),
            % TODO
            ?ERROR(_Reason),
            ?ERROR({vm_vncport, _Vm}),
            timer:sleep(3),
            %
            RemoteInfo = get_vm_info(_DestIP, _VmId, _Vm#vm_t.vm_name),
            LocalInfo  = get_vm_info(_SrcIP, _VmId, _Vm#vm_t.vm_name),
            ?INFO({"*****************", LocalInfo, RemoteInfo}),
            NextState = do_migrate_error(LocalInfo, RemoteInfo),
            % TODO
            edog_common:flush_msg(),
            fsm_next_state(?VM_STATE_PROCESSING, NextState, _StateData)
    end;
processing({?DONE, vm_pause, _Reply}, #vm_state{key=_VmId} = _StateData) ->
    case _Reply of
        {ok, _} ->
            edog_table_vm:update_state(_VmId, ?VM_STATE_PAUSED),
            fsm_next_state(?VM_STATE_PROCESSING, ?VM_STATE_PAUSED, _StateData);
        {error, _Reason} ->
            fsm_next_state(?VM_STATE_PROCESSING, ?VM_STATE_RUNNING, _StateData)
    end;
processing({?DONE, vm_resume, _Reply}, #vm_state{key=_VmId} = _StateData) ->
    case _Reply of
        {ok, _} ->
            edog_table_vm:update_state(_VmId, ?VM_STATE_RUNNING),
            fsm_next_state(?VM_STATE_PROCESSING, ?VM_STATE_RUNNING, _StateData);
        {error, ?VM_STATE_UNDEFINED} ->
            fsm_stop(normal, _StateData);
        {error, ?VM_STATE_PAUSED} ->
            fsm_stop(normal, _StateData);
        {error, _Reason} ->
            fsm_next_state(?VM_STATE_PROCESSING, ?VM_STATE_PAUSED, _StateData)
    end;
processing(timeout, _StateData) ->
    fsm_next_state(?VM_STATE_PROCESSING, _StateData);
%% TODO
processing({vm_destroy, VmId}, _StateData) ->
    case do_vm_destroy(VmId) of
        {ok, _} ->
            fsm_stop(normal, _StateData);
        {error, _Reason} ->
            fsm_stop(normal, _StateData)
    end;
processing(_Event, _StateData) ->
    ?ERROR({_Event, _StateData}),
    fsm_next_state(?VM_STATE_PROCESSING, _StateData).

do_migrate_error(#vm_info{vmid=_VmId, vmname=_VmName, status=?VM_STATE_UNDEFINED}=_Local,
    #vm_info{status=?VM_STATE_RUNNING} = _Remote) ->
    edog_table_vm:update_pmid(_VmId, _Remote#vm_info.ip, _Remote#vm_info.port),
    ?VM_STATE_RUNNING;
do_migrate_error(#vm_info{vmid=_VmId, vmname=_VmName, status=?VM_STATE_UNDEFINED}=_Local,
    #vm_info{status=?VM_STATE_PAUSED} = _Remote) ->
    edog_libvirt:vm_resume(_Remote#vm_info.ip, _VmId, _VmName),
    edog_table_vm:update_pmid(_VmId, _Remote#vm_info.ip, _Remote#vm_info.port),
    ?VM_STATE_RUNNING;
%
do_migrate_error(#vm_info{vmid=_VmId, vmname=_VmName, status=?VM_STATE_SHUTOFF}=_Local,
    #vm_info{status=?VM_STATE_RUNNING} = _Remote) ->
    edog_table_vm:update_pmid(_VmId, _Remote#vm_info.ip, _Remote#vm_info.port),
    ?VM_STATE_RUNNING;
do_migrate_error(#vm_info{vmid=_VmId, vmname=_VmName, status=?VM_STATE_SHUTOFF}=_Local,
    #vm_info{status=?VM_STATE_PAUSED} = _Remote) ->
    edog_libvirt:vm_resume(_Remote#vm_info.ip, _VmId, _VmName),
    edog_table_vm:update_pmid(_VmId, _Remote#vm_info.ip, _Remote#vm_info.port),
    ?VM_STATE_RUNNING;
%
do_migrate_error(#vm_info{vmid=_VmId, vmname=_VmName, status=?VM_STATE_RUNNING}=_Local,
    #vm_info{status=?VM_STATE_RUNNING} = _Remote) ->
    edog_libvirt:vm_stop(_Remote#vm_info.ip, _VmId, _VmName),
    ?VM_STATE_RUNNING;
do_migrate_error(#vm_info{vmid=_VmId, vmname=_VmName, status=?VM_STATE_RUNNING}=_Local,
    #vm_info{status=?VM_STATE_PAUSED} = _Remote) ->
    edog_libvirt:vm_stop(_Remote#vm_info.ip, _VmId, _VmName),
    ?VM_STATE_RUNNING;
do_migrate_error(#vm_info{vmid=_VmId, vmname=_VmName, status=?VM_STATE_RUNNING}=_Local,
    #vm_info{status=?VM_STATE_UNDEFINED} = _Remote) ->
    ?VM_STATE_RUNNING;
%
do_migrate_error(#vm_info{vmid=_VmId, vmname=_VmName, status=?VM_STATE_PAUSED}=_Local,
    #vm_info{status=?VM_STATE_RUNNING} = _Remote) ->
    edog_libvirt:vm_stop(_Local#vm_info.ip, _VmId, _VmName),
    edog_table_vm:update_pmid(_VmId, _Remote#vm_info.ip, _Remote#vm_info.port),
    ?VM_STATE_RUNNING;
do_migrate_error(#vm_info{vmid=_VmId, vmname=_VmName, status=?VM_STATE_PAUSED}=_Local,
    #vm_info{status=?VM_STATE_PAUSED} = _Remote) ->
    edog_libvirt:vm_stop(_Remote#vm_info.ip, _VmId, _VmName),
    edog_libvirt:vm_resume(_Local#vm_info.ip, _VmId, _VmName),
    ?VM_STATE_RUNNING;
% TODO
do_migrate_error(#vm_info{vmid=_VmId, vmname=_VmName, status=_}=_Local,
    #vm_info{status=_} = _Remote) ->
    oops.

%% -------------------------------------------------------------------
%% Disallowed
%% -------------------------------------------------------------------
creating(_Event, _StateData) ->
    ?ERROR({_Event, _StateData}),
    fsm_next_state(?VM_STATE_CREATING, _StateData).

shutoff(_Event, _StateData) ->
    ?ERROR({_Event, _StateData}),
    fsm_next_state(?VM_STATE_SHUTOFF, _StateData).

running(_Event, _StateData) ->
    ?ERROR({_Event, _StateData}),
    fsm_next_state(?VM_STATE_RUNNING, _StateData).

paused(_Event, _StateData) ->
    ?ERROR({_Event, _StateData}),
    fsm_next_state(?VM_STATE_PAUSED, _StateData).

processing(_Event, _From, _StateData) ->
    ?ERROR({_Event, _From, _StateData}),
    Reply =
    case is_restart(_StateData) of
        true  -> {ok, true};
        false -> {error, ?VM_STATE_PROCESSING}
    end,
    fsm_reply(Reply, ?VM_STATE_PROCESSING, _StateData).

is_restart(#vm_state{context={vm_start, ContextOpts}} = _StateData) ->
    Vm = proplists:get_value(vm, ContextOpts),
    edog_table_vm:get_restart(Vm);
is_restart(_) ->
    false.

%% -------------------------------------------------------------------
%%
%% -------------------------------------------------------------------
handle_sync_event(get_status, _From, _StateName, _StateData) ->
    {reply, _StateName, _StateName, _StateData};
handle_sync_event(get_statusdata, _From, _StateName, _StateData) ->
    {reply, {_StateName, _StateData}, _StateName, _StateData};
handle_sync_event(stop, _From, _StateName, _StateData) ->
    {stop, normal, _StateName, _StateData};
handle_sync_event(_Event, _From, _StateName, _StateData) ->
    {reply, _StateName, _StateName, _StateData}.

handle_event({vm_check, #vm_t{vm_id=_VmID} = Vm, Node, NewState},
    _StateName, _StateData) when is_atom(Node) ->
    do_vm_check(Vm, Node, NewState, _StateName, _StateData);
handle_event(_Event, _StateName, _StateData) ->
    ?ERROR({_Event, _StateName, _StateData}),
    {next_state, _StateName, _StateData}.

handle_info({'DOWN', _MonitorRef, process, _Pid, _Msg}=_Info, _StateName, _StateData) ->
    ?INFO({_Info, _StateName, _StateData}),
    case _Msg of
        % TODO
        #notify_spec{op=vm_resume, reply={error, ?VM_STATE_UNDEFINED}} ->
            gen_fsm:send_event(self(), {?DONE, vm_resume, {error, ?VM_STATE_UNDEFINED}}),
            {next_state, _StateName, _StateData};
        %#notify_spec{op=vm_start, reply={error, nodedown}} ->
        %    gen_fsm:send_event(self(), {?DONE, vm_start, {error, nodedown}}),
        %    {next_state, _StateName, _StateData};
        #notify_spec{op=Op, reply=Reply} ->
            gen_fsm:send_event(self(), {?DONE, Op, Reply}),
            edog_fg:notify(_Msg),
            {next_state, _StateName, _StateData};
        _ ->
            fsm_stop(_Msg, _StateData)
    end;
handle_info(_Info, _StateName, _StateData) ->
    ?ERROR({_Info, _StateName, _StateData}),
    {next_state, _StateName, _StateData}.

code_change(_OldVsn, _StateName, _StateData, _Extra) ->
    {ok, _StateName, _StateData}.

%% -------------------------------------------------------------------
%% Internals
%% -------------------------------------------------------------------
ensure_actor(Op, _VmId) when is_list(_VmId) ->
    Actor = cclib_utils:to_atom(_VmId),
    case whereis(Actor) of
        undefined ->
            supervisor:delete_child(edog_vmsup, Actor),
            ChildSpec = {Actor, {edog_vm, start_link, [Op, _VmId]}, transient, 10000, worker, [edog_vm]},
            supervisor:start_child(edog_vmsup, ChildSpec);
        Pid when is_pid(Pid) ->
            {ok, Pid}
    end.

do_vm_check(#vm_t{vm_id=VmId, pm_id=PmId} = Vm, Node, NewState, OldState, _StateData) ->
    SrcIp = cclib_node:get_ip(Node),
    if
        OldState =:= ?VM_STATE_PROCESSING ->
            fsm_next_state(OldState, _StateData);
        SrcIp =/= PmId ->
            ?ERROR({need_stop, Vm, Node, NewState}),
            edog_libvirt:vm_stop(SrcIp, VmId, Vm#vm_t.vm_name),
            fsm_next_state(OldState, _StateData);
        OldState =:= NewState ->
            fsm_next_state(OldState, _StateData);
        OldState =:= ?VM_STATE_SHUTOFF, NewState =:= ?VM_STATE_UNDEFINED ->
            fsm_next_state(OldState, _StateData);
        OldState =:= ?VM_STATE_SHUTOFF, NewState =:= ?VM_STATE_RUNNING ->
            ?ERROR({need_stop, Vm, Node, NewState}),
            edog_libvirt:vm_stop(SrcIp, VmId, Vm#vm_t.vm_name),
            fsm_next_state(OldState, _StateData);
        OldState =:= ?VM_STATE_RUNNING orelse OldState =:= ?VM_STATE_PAUSED,
        NewState =:= ?VM_STATE_SHUTOFF orelse NewState =:= ?VM_STATE_UNDEFINED ->
            Policy = edog_conf:policy_if_vm_fails(),
            ?WARN({vm_check, Vm, Node, NewState, Policy}),
            case Policy of
                update_db ->
                    edog_table_vm:update_state(VmId, ?VM_STATE_SHUTOFF),
                    fsm_stop(normal, _StateData);
                restart ->
                    % TODO
                    do_vm_restart(Vm, Node),
                    fsm_next_state(OldState, ?VM_STATE_SHUTOFF, _StateData);
                _ ->
                    fsm_stop(normal, _StateData)
                    %fsm_next_state(OldState, _StateData)
            end;
        OldState =:= ?VM_STATE_RUNNING, NewState =:= ?VM_STATE_PAUSED ->
            ?ERROR({need_resume, Vm, Node, NewState}),
            case edog_libvirt:vm_state(PmId, VmId, Vm#vm_t.vm_name) of
                {ok, ?VM_STATE_PAUSED} ->
                    edog_libvirt:vm_resume(SrcIp, VmId, Vm#vm_t.vm_name);
                _ ->
                    ok
            end,
            fsm_next_state(OldState, _StateData);
        true ->
            ?ERROR({Vm, Node, OldState, NewState}),
            fsm_next_state(OldState, _StateData)
    end.

-spec do_vm_destroy(uuid()) -> {ok, any()} | {error, any()}.
do_vm_destroy(VmId) ->
    edog_table_vm:vm_destroy(VmId).

-spec do_vm_start(uuid(), list()) -> {ok, any()} | {error, any()}.
do_vm_start(VmId, Options) ->
    case edog_table_vm:get_start_info(VmId) of
        {ok, {_Vm, _Ns, []}} ->
            {error, no_disks};
        {ok, {Vm, Ns, Disks}} when is_list(Disks) ->
            % TODO
            %?INFO({vm_start, Options, Vm, Disks}),
            DestIp = proplists:get_value(ip, Options),
            RealMemory =
            case proplists:get_value(memory, Options) of
                Memory when is_integer(Memory) ->
                    erlang:min(Memory, Vm#vm_t.vm_mem);
                _ ->
                    Vm#vm_t.vm_mem
            end,
            case RealMemory =:= Vm#vm_t.vm_mem of
                true  -> edog_table_vm:clear_real_memory(VmId);
                false -> edog_table_vm:set_real_memory(VmId, RealMemory)
            end,
            % LOCK DISKS
            lock_disks(Disks),
            case edog_libvirt:vm_start(DestIp, Vm#vm_t{vm_mem=RealMemory}, Ns, Disks) of
                {ok, _} ->
                    ContextOpts = [{vm, Vm}, {dest_ip, DestIp}, {disks, Disks}],
                    {ok, ContextOpts};
                {error, _Reason} ->
                    {error, _Reason}
            end;
        {error, _Reason} ->
            {error, _Reason}
    end.

do_vm_stop(VmId, Opts) ->
    case edog_table_vm:get_stop_info(VmId) of
        {ok, {Vm, _Ns}} ->
            ?INFO({vm_stop, Vm, Opts}),
            edog_libvirt:vm_stop(Vm#vm_t.pm_id, Vm#vm_t.vm_id, Vm#vm_t.vm_name, Opts);
        {error, _Reason} ->
            {error, _Reason}
    end.

-spec do_vm_migrate(uuid(), list()) -> {ok, any()} | {error, any()}.
do_vm_migrate(VmId, DestIp) ->
    case edog_table_vm:get_start_info(VmId) of
        {ok, {#vm_t{pm_id=DestIp1}, _Ns, _Disks}} when DestIp1 =:= DestIp ->
            {error, samenode};
        {ok, {#vm_t{vm_name=VmName, pm_id=SrcIp} = Vm, Ns, Disks}} ->
            lock_disks(Disks),
            case edog_libvirt:vm_migrate(SrcIp, VmId, VmName, DestIp, Ns, Disks) of
                {ok, _} ->
                    ContextOpts = [{vm, Vm}, {src_ip, SrcIp}, {dest_ip, DestIp}, {disks, Disks}],
                    {ok, ContextOpts};
                {error, _Reason} ->
                    {error, _Reason}
            end;
        {error, _Reason} ->
            {error, _Reason}
    end.

%% --------------------------------------------------------------------
%%
%% --------------------------------------------------------------------
do_vm_restart(#vm_t{vm_id=VmId} = Vm, Node) ->
    Opts = [
        {wait_time, 3},
        {memory_policy, edog_conf:vm_restart_memory_policy()},
        {try_node, edog_conf:vm_restart_try_previous()},
        {node, Node}
    ],
    ?WARN({restart, Vm, Node, Opts}),
    Res = edog_select:push({vm_start, VmId, Opts}),
    edog_table_vm:update_state(VmId, ?VM_STATE_SHUTOFF),
    edog_table_vm:set_restart(VmId),
    Res.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_vm_info(Ip, VmId, VmName) ->
    case edog_libvirt:vm_info(Ip, VmId, VmName) of
        {ok, {Status, Port}} when Port > 0 ->
            #vm_info{vmid=VmId, vmname=VmName, ip=Ip, status=Status, port=Port};
        _Res ->
            ?ERROR(_Res),
            #vm_info{vmid=VmId, vmname=VmName, ip=Ip,
                status=?VM_STATE_UNDEFINED, port=-1}
    end.

-ifdef(use_proxy).
check_before_migrate(DestIp) ->
    Node = edog_common:get_node(DestIp),
    case rpc:call(Node, cclib_utils, is_process_exists, ["proxy_server"]) of
        {badrpc, Reason} ->
            {error, Reason};
        false ->
            {error, "proxy server not started on " ++ DestIp};
        true ->
            {ok, true}
    end.
-else.
check_before_migrate(_DestIp) ->
    {ok, true}.
-endif.

%% [{DiskId, Path, Target}]
lock_disks(Disks) ->
    case edog_conf:target_gc() of
        true ->
            Targets = get_targets(Disks),
            lists:foreach(fun lock_target/1, Targets);
        false ->
            ok
    end.

unlock_disks(Disks) ->
    case edog_conf:target_gc() of
        true ->
            Targets = get_targets(Disks),
            lists:foreach(fun edog_target_gc:unlock/1, Targets);
        false ->
            ok
    end.

lock_target(Target) ->
    case edog_target_gc:rdlock(Target) of
        ok ->
            ok;
        {error, _Type} ->
            timer:sleep(500),
            lock_target(Target)
    end.

get_targets(Disks) ->
    [DiskId || {DiskId, _Path, _Target} <- Disks].


%% --------------------------------------------------------------------
%%
%% --------------------------------------------------------------------
tpl() ->
    cclib_dbg:tpl(?MODULE, do_vm_restart),
    cclib_dbg:tpl(?MODULE, vm_action),
    cclib_dbg:tpl(?MODULE, fsm_stop).

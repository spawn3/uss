-module(edog_libvirt).
-behaviour(gen_server).
-export([
        start_link/0,
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3,

        libvirt_start/1,

        vm_start/4,
        vm_stop/3,
        vm_stop/4,
        vm_pause/3,
        vm_resume/3,
        vm_migrate/6,
        vm_setvcpus/3,
        vm_setmem/3,
        vm_info/3,
        vm_vncport/3,
        vm_state/3,

        do/1,

        domain_info/0,
        domain_clear/0,
        domain_state/1,
        domain_state/2,
        domain_vncport/1,
        domain_disklist/1,

        libvirtd_start/0,
        libvirtd_stop/0,

        check_disklist/2,

        get_vmid/1,

        test_writexml/0
    ]).
%% -compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").
-include("edog_common.hrl").

-define(XMLPATH, "/sysy/yfs/ussadmin/edog_runtime/virsh").

%% "error: unable to connect to '/var/run/libvirt/libvirt-sock', libvirtd may need to be started: Connection refused\nerror: failed to connect to the hypervisor\n"
-define(DOMSTATE_RETURN_1, "error.*libvirtd may need to be started.*").

-define(CMD_TIMEO, 600000).

exec(Cmd) ->
    cclib_cmd:exec_debug(Cmd, ?CMD_TIMEO).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------
libvirt_start(IP) ->
    gen_server:cast(?MODULE, {libvirt_start, IP}).

vm_start(IP, #vm_t{} = Vm, Ns, DiskList) ->
    Opts = [{ip, IP}, {ns, Ns}],
    call_wrapper_monitor({vm_start, IP, Vm, DiskList, Opts}).

vm_stop(IP, VmID, VmName) ->
    vm_stop(IP, VmID, VmName, []).

vm_stop(IP, VmID, VmName, Opts) ->
    call_wrapper_monitor({vm_stop, IP, VmID, VmName, Opts}).

vm_pause(IP, VmID, VmName) ->
    call_wrapper_monitor({vm_pause, IP, VmID, VmName}).

vm_resume(IP, VmID, VmName) ->
    call_wrapper_monitor({vm_resume, IP, VmID, VmName}).

vm_migrate(IP, VmID, VmName, DestIp, Ns, DiskList) ->
    Opts = [{src_ip, IP}, {dest_ip, DestIp}, {ns, Ns}],
    call_wrapper_monitor({vm_migrate, IP, VmID, VmName, DiskList, Opts}).

%%
vm_setvcpus(IP, VmID, VmName) ->
    call_wrapper({vm_setvcpus, IP, VmID, VmName}).

vm_setmem(IP, VmID, VmName) ->
    call_wrapper({vm_setmem, IP, VmID, VmName}).

vm_state(IP, VmID, VmName) ->
    call_wrapper({vm_state, IP, VmID, VmName}).

vm_vncport(IP, VmID, VmName) ->
    call_wrapper({vm_vncport, IP, VmID, VmName}).

vm_info(IP, VmID, VmName) ->
    call_wrapper({vm_info, IP, VmID, VmName}).

%%
call_wrapper(Request, Timeout) ->
    try
        gen_server:call(?MODULE, Request, Timeout)
    catch
        exit: {timeout, _} ->
            ?WARN({Request, Timeout, erlang:get_stacktrace()}),
            IP = erlang:element(2, Request),
            libvirt_start(IP),
            call_wrapper(Request, Timeout);
        Class:Exception ->
            ?WARN({Class, Exception, Request, Timeout, erlang:get_stacktrace()}),
            {error, Class}
    end.

call_wrapper(Request) ->
    call_wrapper(Request, ?TO_CALL).

call_wrapper_monitor(Request) ->
    case call_wrapper(Request) of
        {ok, Pid} when is_pid(Pid) ->
            erlang:monitor(process, Pid),
            {ok, Pid};
        Res -> Res
    end.

%% -------------------------------------------------------------------
%% Callback
%% -------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, []}.

terminate(_Reason, _State) ->
    ok.

handle_call({vm_start, IP, Vm, DiskList, Opts} = _Msg, _From, _State) ->
    %?INFO(_Msg),
    Reply = async_handle_call(IP, {vm_start, Vm, DiskList, Opts}, 10*?TO_RPC_NORMAL),
    {reply, {ok, Reply}, _State};
handle_call({vm_stop, IP, VmID, VmName, Opts} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = async_handle_call(IP, {vm_stop, VmID, VmName, Opts}, 10*?TO_RPC_NORMAL),
    {reply, {ok, Reply}, _State};
handle_call({vm_pause, IP, VmID, VmName} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = async_handle_call(IP, {vm_pause, VmID, VmName}, ?TO_RPC_NORMAL),
    {reply, {ok, Reply}, _State};
handle_call({vm_resume, IP, VmID, VmName} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = async_handle_call(IP, {vm_resume, VmID, VmName}, ?TO_RPC_NORMAL),
    {reply, {ok, Reply}, _State};
handle_call({vm_migrate, IP, VmID, VmName, DiskList, Opts} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = async_handle_call(IP, {vm_migrate, VmID, VmName, DiskList, Opts}, 10*?TO_RPC_NORMAL),
    {reply, {ok, Reply}, _State};
%%
handle_call({vm_setvcpus, IP, VmID, VmName, Vcpus} = _Msg, _From, _State) ->
    F = fun() ->
        Reply = sync_handle_call(IP, {vm_setvcpus, VmID, VmName, Vcpus}),
        gen_server:reply(_From, Reply)
    end,
    spawn(F),
    {noreply, _State};
handle_call({vm_setmem, IP, VmID, VmName, Mem} = _Msg, _From, _State) ->
    F = fun() ->
        Reply = sync_handle_call(IP, {vm_setmem, VmID, VmName, Mem}),
        gen_server:reply(_From, Reply)
    end,
    spawn(F),
    {noreply, _State};
handle_call({vm_state, IP, VmID, VmName} = _Msg, _From, _State) ->
    F = fun() ->
        Reply = sync_handle_call(IP, {vm_state, VmID, VmName}),
        gen_server:reply(_From, Reply)
    end,
    spawn(F),
    {noreply, _State};
handle_call({vm_vncport, IP, VmID, VmName} = _Msg, _From, _State) ->
    F = fun() ->
        Reply = sync_handle_call(IP, {vm_vncport, VmID, VmName}),
        gen_server:reply(_From, Reply)
    end,
    spawn(F),
    {noreply, _State};
handle_call({vm_info, IP, VmID, VmName} = _Msg, _From, _State) ->
    F = fun() ->
        Reply = sync_handle_call(IP, {vm_info, VmID, VmName}),
        gen_server:reply(_From, Reply)
    end,
    spawn(F),
    {noreply, _State};

handle_call(_Msg, _From, _State) ->
    ?WARN(_Msg),
    Reply = ok,
    {reply, Reply, _State}.

handle_cast({libvirt_start, IP}, _State) ->
    Node = edog_common:get_node(IP),
    rpc:call(Node, edog_libvirt, libvirtd_start, []),
    {noreply, _State};
handle_cast(_Msg, _State) ->
    {noreply, _State}.

handle_info({'EXIT', _From, _Reason}, _State) ->
    {noreply, _State};
handle_info(_Msg, _State) ->
    {noreply, _State}.

code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.

%% ---------------------------------------------------------------------
%% Remote
%% ---------------------------------------------------------------------
-spec async_handle_call(ip(), tuple(), integer()) -> any().
async_handle_call(IP, {vm_start, #vm_t{} = Vm, DiskList, Opts}, Timeout) ->
    spawn(
        fun() ->
            Node = edog_common:get_node(IP),
            ?PROFILE_BEGIN(),
            Reply = ?RPC_CALL_TO(Node, edog_libvirt, do, [{vm_start, Vm, DiskList, Opts}], Timeout),
            ?PROFILE_END(vm_start),
            Reply1 =
            case Reply of
                {badrpc, _Reason} ->
                    % TODO timeout
                    ?WARN({badrpc, _Reason, Node}),
                    {error, _Reason};
                _Res -> _Res
            end,
            Notify = #notify_spec{op=vm_start, key=Vm#vm_t.vm_id, reply=Reply1},
            %edog_master:notify(Notify),
            exit(Notify)
        end);
async_handle_call(IP, {vm_migrate, VmID, VmName, DiskList, Opts}, Timeout) ->
    spawn(
        fun() ->
            Node = edog_common:get_node(IP),
            ?PROFILE_BEGIN(),
            Reply = ?RPC_CALL_TO(Node, edog_libvirt, do, [{vm_migrate, VmID, VmName, DiskList, Opts}], Timeout),
            ?PROFILE_END(vm_migrate),
            Diff = timer:now_diff(PROFILE_T2__, PROFILE_T1__) div 1000000,
            Reply1 =
            case Reply of
                {badrpc, _Reason} ->
                    {error, _Reason, Diff};
                {ok, Port} ->
                    DestIp = proplists:get_value(dest_ip, Opts),
                    {ok, {DestIp, Port}, Diff};
                _Res -> _Res
            end,
            Notify = #notify_spec{op=vm_migrate, key=VmID, reply=Reply1},
            %edog_master:notify(Notify),
            exit(Notify)
        end);
async_handle_call(IP, {vm_stop, VmID, VmName, Opts}, Timeout) ->
    spawn(
        fun() ->
            Node = edog_common:get_node(IP),
            ?PROFILE_BEGIN(),
            Reply = ?RPC_CALL_TO(Node, edog_libvirt, do, [{vm_stop, VmID, VmName, Opts}], Timeout),
            ?PROFILE_END(vm_stop),
            Reply1 =
            case Reply of
                {badrpc, _Reason} -> {error, _Reason};
                _Res -> _Res
            end,
            Notify = #notify_spec{op=vm_stop, key=VmID, reply=Reply1},
            %edog_master:notify(Notify),
            exit(Notify)
        end);
async_handle_call(IP, {Action, VmID, VmName}, Timeout) ->
    spawn(
        fun() ->
            Node = edog_common:get_node(IP),
            ?PROFILE_BEGIN(),
            Reply = ?RPC_CALL_TO(Node, edog_libvirt, do, [{Action, VmID, VmName}], Timeout),
            ?PROFILE_END(Action),
            Reply1 =
            case Reply of
                {badrpc, _Reason} -> {error, _Reason};
                _Res -> _Res
            end,
            Notify = #notify_spec{op=Action, key=VmID, reply=Reply1},
            %edog_master:notify(Notify),
            exit(Notify)
        end).

-spec sync_handle_call(ip(), tuple()) -> return_t().
sync_handle_call(IP, Args) ->
    sync_handle_call(IP, Args, ?TO_RPC_NORMAL).

sync_handle_call(IP, Args, Timeout) ->
    %% ?INFO({IP, Args, Timeout}),
    try
        Node = edog_common:get_node(IP),
        case rpc:call(Node, edog_libvirt, do, [Args], Timeout) of
            {badrpc, _Reason} -> {error, _Reason};
            {error, _Reason} -> {error, _Reason};
            {ok, Res} -> {ok, Res};
            Res -> {ok, Res}
        end
    catch
        Class:Pattern ->
            ?WARN({Class, Pattern, IP, Args, Timeout, erlang:get_stacktrace()}),
            {error, Pattern}
    end.

%% ---------------------------------------------------------------------
%% Local Method
%% ---------------------------------------------------------------------
%% do/1
-spec do(tuple()) -> return_t().
do({vm_start, #vm_t{vm_id=VmID, vm_name=VmName, vm_cpu=Cpu, vm_mem=Mem, vm_pcis=Pcis}, DiskList, Opts}) ->
    Domain = get_domain(VmID, VmName),
    case domain_state(Domain) of
        ?VM_STATE_UNDEFINED ->
            domain_create(VmID, Domain, Cpu, Mem, Pcis, DiskList, Opts);
        ?VM_STATE_SHUTOFF ->
            domain_undefine(Domain),
            domain_create(VmID, Domain, Cpu, Mem, Pcis, DiskList, Opts);
        ?VM_STATE_PAUSED ->
            {ok, domain_vncport(Domain)};
        ?VM_STATE_RUNNING ->
            {ok, domain_vncport(Domain)};
        vm_state_unknown ->
            {error, vm_state_unknown}
    end;
do({vm_stop, VmID, VmName, Opts}) ->
    Domain = get_domain(VmID, VmName),
    case domain_state(Domain) of
        ?VM_STATE_RUNNING ->
            domain_stop(Domain, Opts),
            domain_undefine(Domain);
        ?VM_STATE_PAUSED ->
            domain_stop(Domain, Opts),
            domain_undefine(Domain);
        ?VM_STATE_SHUTOFF ->
            domain_undefine(Domain);
        ?VM_STATE_UNDEFINED ->
            {ok, true};
        vm_state_unknown ->
            {error, vm_state_unknown}
    end;
do({vm_pause, VmID, VmName}) ->
    Domain = get_domain(VmID, VmName),
    case domain_state(Domain) of
        ?VM_STATE_RUNNING ->
            domain_pause(Domain);
        _Other ->
            {error, _Other}
    end;
do({vm_resume, VmID, VmName}) ->
    Domain = get_domain(VmID, VmName),
    case domain_state(Domain) of
        ?VM_STATE_PAUSED ->
            domain_resume(Domain);
        _Other ->
            {error, _Other}
    end;
do({vm_migrate, VmID, VmName, DiskList, Opts}) ->
    Domain = get_domain(VmID, VmName),
    case domain_state(Domain) of
        ?VM_STATE_RUNNING ->
            domain_migrate(Domain, DiskList, Opts);
        _Other ->
            {error, _Other}
    end;
do({vm_state, VmID, VmName}) ->
    Domain = get_domain(VmID, VmName),
    domain_state(Domain);
do({vm_state, Domain}) ->
    domain_state(Domain);
do({vm_vncport, VmID, VmName}) ->
    Domain = get_domain(VmID, VmName),
    do({vm_vncport, Domain});
do({vm_vncport, Domain}) ->
    case domain_assert(Domain, ?VM_STATE_RUNNING) of
        {ok, _} -> domain_vncport(Domain);
        {error, _Reason} -> -1
    end;
do({vm_info, VmID, VmName}) ->
    Domain = get_domain(VmID, VmName),
    do({vm_info, Domain});
do({vm_info, Domain}) ->
    case domain_state(Domain) of
        ?VM_STATE_RUNNING ->
            {?VM_STATE_RUNNING, domain_vncport(Domain)};
        ?VM_STATE_PAUSED ->
            {?VM_STATE_PAUSED, domain_vncport(Domain)};
        Status ->
            {Status, -1}
    end;
do({vm_setvcpus, VmID, VmName, Vcpus}) ->
    Domain = get_domain(VmID, VmName),
    domain_setvcpus(Domain, Vcpus);
do({vm_setvcpus, Domain, Vcpus}) ->
    domain_setvcpus(Domain, Vcpus);
do({vm_setmem, VmID, VmName, Mem}) ->
    Domain = get_domain(VmID, VmName),
    domain_setmem(Domain, Mem);
do({vm_setmem, Domain, Mem}) ->
    domain_setmem(Domain, Mem).

%% -------------------------------------------------------------------
%% Internal
%% -------------------------------------------------------------------
virsh_cmd(Args) ->
    lists:concat([edog_conf:bin_virsh(), " "|Args]).

rm_domain_log(Domain) ->
    % if this log is too large, ...
    cclib_file:truncate(filename:join([?QEMU_LOG_PATH, Domain ++ ".log"])).

-define(DOMAIN_CREATE_RETRIES, 16).
-define(DOMAIN_CREATE_SLEEP,   10).

domain_create(VmID, Domain, Cpu, Mem, Pcis, DiskList, Opts) ->
    rm_domain_log(Domain),
    domain_create({VmID, Domain, Cpu, Mem, Pcis, DiskList, Opts}, ?DOMAIN_CREATE_RETRIES).

domain_create(Args, 0) ->
    domain_create(Args);
domain_create(Args, Retries) ->
    case timer:tc(edog_yfs_shell, is_yfs_ready, []) of
        {_T, true} ->
            %_DiskIDList = [DiskId || {DiskId, _, _} <- DiskList],
            timer:sleep(1000),
            domain_create(Args);
        {T, false} ->
            Wait = ?DOMAIN_CREATE_SLEEP,
            T1 = T div 1000000,
            ?WARN({yfs_not_ready, T1, Wait, Args, Retries}),
            if
                T1 < Wait -> timer:sleep((Wait-T1) * 1000);
                true -> ok
            end,
            domain_create(Args, Retries-1)
    end.

%% Domain
domain_create({VmID, Domain, Cpu, Mem, Pcis, DiskList, Opts}) ->
    DiskList1 = lists:keysort(3, DiskList),
    case check_disklist(DiskList1, Opts) of
        [] ->
            Pcis1 = [{bridge, Nic} || #nic_t{} = Nic <- Pcis],
            %% write XML file
            Path = filename:join([?XMLPATH, Domain ++ ".xml"]),
            filelib:ensure_dir(Path),
            domain_writexml(Path, VmID, Domain, Cpu, Mem, Pcis1, DiskList1, Opts),
            Cmd = virsh_cmd(["create ", Path]),
            case exec(Cmd) of
                {ok, #cmd_info{}} ->
                    case domain_assert(Domain, ?VM_STATE_RUNNING) of
                        {ok, _} ->
                            % TODO
                            case domain_vncport(Domain) of
                                Port when Port > 0 ->
                                    {ok, Port};
                                _Other ->
                                    {error, _Other}
                            end;
                        {error, _Reason} ->
                            {error, _Reason}
                    end;
                {error, #cmd_info{}=Info} ->
                    {error, Info#cmd_info.data}
            end;
        InvalidPaths ->
            ?ERROR({error, InvalidPaths}),
            {error, InvalidPaths}
    end.

domain_stop(Domain) ->
    domain_stop(Domain, []).

domain_stop(Domain, Opts) ->
    case domain_stop_1(Domain, Opts) of
        {ok, Info} ->
            case proplists:get_bool(shutdown, Opts) of
                true ->
                    wait_for_shutdown(Domain, 30);
                false ->
                    {ok, Info}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec domain_stop(string()) -> return_t().

domain_stop_1(Domain, Opts) ->
    case domain_state(Domain) of
        ?VM_STATE_UNDEFINED ->
            {ok, true};
        ?VM_STATE_SHUTOFF ->
            {ok, true};
        _ ->
            Cmd =
            case proplists:get_bool(shutdown, Opts) of
                true  -> virsh_cmd(["shutdown ", Domain]);
                false -> virsh_cmd(["destroy ", Domain])
            end,
            case exec(Cmd) of
                {ok, #cmd_info{}} ->
                    {ok, true};
                {error, #cmd_info{}=Info} ->
                    {error, Info#cmd_info.data}
            end
    end.


wait_for_shutdown(Domain, N) when N < 0 ->
    case domain_state(Domain) of
        ?VM_STATE_UNDEFINED ->
            {ok, true};
        ?VM_STATE_SHUTOFF ->
            {ok, true};
        State ->
            {error, State}
    end;
wait_for_shutdown(Domain, N) ->
    case domain_state(Domain) of
        ?VM_STATE_UNDEFINED ->
            {ok, true};
        ?VM_STATE_SHUTOFF ->
            {ok, true};
        State ->
            timer:sleep(6000),
            ?WARN({wait_for_shutdown, Domain, N, State}),
            wait_for_shutdown(Domain, N-1)
    end.

-spec domain_undefine(string()) -> return_t().

domain_undefine(Domain) ->
    Cmd = virsh_cmd(["undefine ", Domain]),
    _Result = cclib_utils:run_cmd(Cmd),
    domain_assert(Domain, ?VM_STATE_UNDEFINED).

-spec domain_pause(string()) -> return_t().
domain_pause(Domain) ->
    Cmd = virsh_cmd(["suspend ", Domain]),
    _Result = ?EXECUTE(Cmd),
    domain_assert(Domain, ?VM_STATE_PAUSED).

-spec domain_resume(string()) -> return_t().
domain_resume(Domain) ->
    Cmd = virsh_cmd(["resume ", Domain]),
    _Result = ?EXECUTE(Cmd),
    domain_assert(Domain, ?VM_STATE_RUNNING).

domain_setvcpus(Domain, Vcpus) ->
    Cmd = virsh_cmd(["setvcpus ", Domain, " ", Vcpus]),
    _Result = ?EXECUTE(Cmd).

domain_setmem(Domain, Mem) ->
    Cmd = virsh_cmd(["setmem ", Domain, " ", Mem]),
    _Result = ?EXECUTE(Cmd).

domain_migrate(Domain, DiskList, Opts) ->
    DestIp = proplists:get_value(dest_ip, Opts),
    Node = edog_common:get_node(DestIp),
    ensure_libvirtd_started(Node, Domain),
    case rpc:call(Node, edog_libvirt, check_disklist, [DiskList, Opts]) of
        {badrpc, Reason} ->
            {error, Reason};
        [] ->
            case domain_migrate(Domain, DestIp) of
                {ok, Port} ->
                    {ok, Port};
                {error, _Reason} ->
                    {error, _Reason}
            end;
        Res ->
            {error, Res}
    end.

%% -spec domain_migrate(string(), string()) -> return_t().
domain_migrate(Domain, DestIp) ->
    Cmd = virsh_cmd(["migrate --live ", Domain, " qemu+ssh://", DestIp, "/system"]),
    %_Result = ?EXECUTE(Cmd),
    %case _Result of
    case exec(Cmd) of
        {ok, #cmd_info{}} ->
            case domain_state(Domain) of
                ?VM_STATE_SHUTOFF ->
                    domain_undefine(Domain),
                    check_remote_vm_state(DestIp, Domain);
                ?VM_STATE_UNDEFINED ->
                    check_remote_vm_state(DestIp, Domain);
                _Other ->
                    ?ERROR({error, {local_state, _Other}}),
                    {error, _Other}
            end;
        {error, #cmd_info{}=Info} ->
            ?ERROR(Info),
            {error, Info#cmd_info.data}
    end.

check_remote_vm_state(DestIp, Domain) ->
    case sync_handle_call(DestIp, {vm_state, Domain}) of
        {ok, ?VM_STATE_RUNNING} ->
            Node = edog_common:get_node(DestIp),
            case ?RPC_CALL(Node, edog_libvirt, domain_vncport, [Domain]) of
                {badrpc, _Reason} ->
                    {error, _Reason};
                Port when is_integer(Port) ->
                    {ok, Port}
            end;
        {error, _Reason} ->
            ?ERROR({error, {remote_state, _Reason}}),
            {error, {remote_state, _Reason}}
    end.

-spec domain_assert(string(), vmstate_t()) -> return_t().
domain_assert(Domain, ExpectedState) ->
    case domain_state(Domain) of
        ExpectedState ->
            {ok, true};
        _Other ->
            {error, _Other}
    end.

domain_writexml(File, VmID, Domain, Cpu, Mem, IntList, DiskList, Opts) ->
    {ok, Str} = domain_render(VmID, Domain, Cpu, Mem, IntList, DiskList, Opts),
    file:write_file(File, Str).

domain_render(VmID, Domain, Cpu, Mem, IntList, DiskList, Opts) ->
    Disks = gen_disks(DiskList, Opts),
    Vifs = [gen_vif(X) || X <- IntList],
    edog_domain_view:render([
            {uuid, VmID},
            {name, Domain},
            {vcpu, Cpu},
            {memory, Mem},
            {emulator, edog_conf:bin_kvm()},
            {disks, Disks},
            {vifs, Vifs}]).

gen_vif({bridge, #nic_t{bridge=Bridge, mac=Mac, model=Model}}) ->
    [{bridge, Bridge}, {mac, Mac}, {model, Model}].

gen_disks(DiskList, Opts) ->
    [gen_disk(X, Opts) || X <- DiskList].

gen_disk({DiskId, Path, Target}, Opts) ->
    Bus = get_bus(Target),
    File = gen_file({DiskId, Path, Target}, Opts),
    [{file, File}, {dev, Target}, {bus, Bus}, {cache, cache_mechanism()}].

gen_file({DiskId, Path, _Target}, _Opts) ->
    case edog_conf:storage_module() of
        edog_storage_proxy ->
            io_lib:format("~s/~s", [edog_conf:storage_prefix(), Path]);
        _ ->
            IscsiTarget = DiskId,
            edog_iscsi:lun_path(IscsiTarget, ?ISCSI_LUN)
    end.

get_bus(Target) ->
    case Target of
        "vd" ++ _ -> virtio;
        "hd" ++ _ -> ide;
        "sd" ++ _ -> scsi
    end.

cache_mechanism() ->
    "none".

check_disklist(DiskList, _Opts) ->
    case edog_conf:storage_module() of
        edog_storage_proxy ->
            [];
        _ ->
            %Ns = proplists:get_value(ns, Opts),
            F = fun({DiskId, _Path, _Target}) ->
                    IscsiTarget = DiskId,
                    {edog_iscsi:ensure_lun(IscsiTarget), IscsiTarget}
            end,
            L = cclib_pmap:pmap(F, DiskList),
            [X || {false, X} <- L]
    end.

%% -----------------------------------------------------------
%% PRIVATE
%% -----------------------------------------------------------
-define(DOMAIN_NAME_SEPARATOR, "_").

get_domain(VmID, VmName) ->
    cclib_utils:to_list(VmName) ++ ?DOMAIN_NAME_SEPARATOR ++ cclib_utils:to_list(VmID).

get_vmid(Domain) ->
    L = string:tokens(Domain, ?DOMAIN_NAME_SEPARATOR),
    Uuid = lists:last(L),
    case cclib_uuid:is_uuid(Uuid) of
        true -> Uuid;
        false -> ""
    end.

domain_state(VmID, VmName) ->
    domain_state(get_domain(VmID, VmName)).

domain_state(Domain) ->
    case domain_state2(Domain) of
        vm_state_unknown ->
            ?WARN({vm_state_unknown, Domain}),
            libvirtd_start(),
            timer:sleep(1000),
            domain_state(Domain);
        Res ->
            Res
    end.

ensure_libvirtd_started(Node, Domain) ->
    rpc:call(Node, ?MODULE, domain_state, [Domain]).

%% TODO qemu is down, but domstate is running, why?
% running
% shutoff
% paused
% undefined
% vm_state_unknown
domain_state2(Domain) ->
    Cmd = virsh_cmd(["domstate ", Domain]),
    Result = os:cmd(Cmd),
    S = lists:nth(1, string:tokens(Result, "\n")),
    case edog_common:is_match(S, ?DOMSTATE_RETURN_1) of
        true ->
            % TODO
            % when libvirtd is killed
            ?ERROR({error, Result}),
            vm_state_unknown;
        false ->
            case string:tokens(S, " \t") of
                ["running"] ->
                    ?VM_STATE_RUNNING;
                ["shut", "off"] ->
                    ?VM_STATE_SHUTOFF;
                ["paused"] ->
                    ?VM_STATE_PAUSED;
                _ ->
                    ?VM_STATE_UNDEFINED
            end
    end.

domain_vncport(Domain) ->
    {ok, E} = domain_xml(Domain),
    [GraphicsE] = xmerl_xpath:string("/domain/devices/graphics", E),
    Port = edog_xml:get_attr(GraphicsE#xmlElement.attributes, port),
    Port2 = list_to_integer(Port),
    ?IF_COND((Port2>0), Port2, -1).

domain_disklist(Domain) ->
    File = filename:join([?XMLPATH, Domain ++ ".xml"]),
    ?INFO(File),
    try xmerl_scan:file(File) of
        {Doc, _R} ->
            L = xmerl_xpath:string("/domain/devices/disk/source", Doc),
            F = fun(#xmlElement{attributes=Attrs}) ->
                FileAttr = edog_xml:get_attr(Attrs, file),
                filename:dirname(FileAttr)
            end,
            [F(Elem) || Elem <- L]
    catch
        _Error:_Expr ->
            {error, {_Error, _Expr}}
    end.

domain_xml(Domain) ->
    Cmd = virsh_cmd(["dumpxml ", Domain]),
    Str = os:cmd(Cmd),
    I = 1,
    try xmerl_scan:string(string:substr(Str, I)) of
        {E, _R} ->
            {ok, E}
    catch
        _Error:_Expr ->
            {error, {_Error, _Expr}}
    end.

domain_info() ->
    Cmd = virsh_cmd(["list --all"]),
    L = string:tokens(os:cmd(Cmd), "\n"),
    try lists:nthtail(2, L) of
        L1 ->
            L2 = [ case string:tokens(X, " \t") of
                    [_Id, Name, "running"] ->
                        #domain{name=Name, id=list_to_integer(_Id), status=?VM_STATE_RUNNING};
                    [_Id, Name, "paused"] ->
                        #domain{name=Name, id=list_to_integer(_Id), status=?VM_STATE_PAUSED};
                    [_Id, Name, "shut", "off"] ->
                        #domain{name=Name, id=0, status=?VM_STATE_SHUTOFF};
                    _Other ->
                        #domain{}
                end || X <- L1],
            Domains = [D#domain{uuid=get_vmid(Domain)} ||
                #domain{name=Domain, status=State} = D <- L2,
                State =:= ?VM_STATE_RUNNING orelse State =:= ?VM_STATE_PAUSED],
            attach_proc_info(Domains)
    catch
        _:_ ->
            []
    end.

attach_proc_info(Domains) ->
    Infos = edog_common:qemu_proc_info(),
    F = fun(#domain{uuid=Uuid}=Domain) ->
        case lists:keyfind(Uuid, 5, Infos) of
            false -> Domain;
            #proc_info{pid=Pid,cpu=Cpu,mem=Mem} ->
                Domain#domain{
                    pid=Pid,
                    cpu=Cpu,
                    mem=Mem}
        end
    end,
    [F(X) || X <- Domains].

domain_clear() ->
    L = domain_info(),
    ?INFO({domain_clear, L}),
    F = fun(#domain{} = Domain) ->
        case Domain of
            #domain{name=Name, status=?VM_STATE_RUNNING} ->
                domain_stop(Name),
                domain_undefine(Name);
            #domain{name=Name, status=?VM_STATE_PAUSED} ->
                domain_stop(Name),
                domain_undefine(Name);
            #domain{name=Name, status=?VM_STATE_SHUTOFF} ->
                domain_undefine(Name);
            _ -> ok
        end
    end,
    lists:foreach(F, L).

%% LIBVIRT
libvirtd_start() ->
    libvirtd_stop(),
    Cmd = edog_conf:bin_libvirtd() ++ " -d -p " ++ ?LIBVIRTD_PID,
    _Result = ?EXECUTE(Cmd).

libvirtd_stop() ->
    ?EXECUTE("pkill -9 libvirtd"),
    file:delete(?LIBVIRTD_PID).

%% -------------------------------------------------------------------
%% Test
%% -------------------------------------------------------------------
test_writexml() ->
    Domain = "ubuntu",
    VmID = cclib_uuid:to_string(),
    domain_toxml(VmID, Domain, 4, 524288,
        [{network, "default", "24:42:53:21:52:45"},
         {bridge, #nic_t{bridge="virbr0", mac="24:42:53:21:52:65"}},
         {direct, "eth1", "vepa"}],
         "ns1",
        [{"disk-id", "/share/ubuntu9.04/ubuntu.img", "hda"}]
     ).

domain_toxml(VmID, Domain, Cpu, Mem, IntList, DiskList, Opts) ->
    io_lib:format(
        "<domain type='kvm'>\n"
        "\t<uuid>~s</uuid>\n"
        "\t<name>~s</name>\n"
        "\t<vcpu>~w</vcpu>\n"
        "\t<memory>~w</memory>\n"
        "\t<os>\n"
        "\t\t<type arch='x86_64' machine='pc'>hvm</type>\n"
        "\t\t<boot dev='hd' />\n"
        "\t</os>\n"
        %"\t<clock offset='localtime' />\n"
        "\t<devices>\n"
        "\t\t<emulator>~s</emulator>\n"
        "~s"
        "~s"
        "\t\t<graphics type='vnc' listen='0.0.0.0' port='-1' />\n"
        "\t</devices>\n"
        "\t<features>\n"
        "\t\t<acpi/>\n"
        "\t</features>\n"
        "</domain>", [
            VmID, Domain,
            trunc(Cpu),
            Mem,
            edog_conf:bin_kvm(),
            gen_disks(DiskList, Opts),
            gen_interfaces(IntList)
        ]).

gen_interfaces(IntList) ->
    gen_interfaces(IntList, "").

gen_interfaces([], Acc) ->
    Acc;
gen_interfaces([{bridge, #nic_t{bridge=Bridge, mac=Mac, model=Model}}|T], Acc) ->
    NewAcc = Acc ++ io_lib:format(
        "\t\t<interface type='bridge'>\n"
        "\t\t\t<source bridge='~s' />\n"
        "\t\t\t<mac address='~s' />\n"
        "\t\t\t<model type='~s'/>\n"
        "\t\t\t<driver name='vhost'/>\n"
        "\t\t</interface>\n", [Bridge, Mac, Model]),
    gen_interfaces(T, NewAcc);
gen_interfaces([{direct, Eth, Mode}|T], Acc) ->
    NewAcc = Acc ++ io_lib:format(
        "\t\t<interface type='direct'>\n"
        "\t\t\t<source dev='~s' mode='~s' />\n"
        "\t\t</interface>\n", [Eth, Mode]),
    gen_interfaces(T, NewAcc);
gen_interfaces([{network, Src, Mac}|T], Acc) ->
    NewAcc = Acc ++ io_lib:format(
        "\t\t<interface type='network'>\n"
        "\t\t\t<source network='~s' />\n"
        "\t\t\t<mac address='~s' />\n"
        "\t\t\t<model type='e1000' />\n"
        "\t\t</interface>\n", [Src, Mac]),
    gen_interfaces(T, NewAcc).

% gen_disks(DiskList, Opts) ->
%     gen_disks(DiskList, Opts,  "").
%
% gen_disks([], _Opts, Acc) ->
%     Acc;
% gen_disks([{DiskId, Path, Target}|T], Opts, Acc) ->
%     Bus = get_bus(Target),
%     File = gen_file({DiskId, Path, Target}, Opts),
%     NewAcc = Acc ++ io_lib:format(
%         "\t\t<disk type='file' device='disk'>\n"
%         "\t\t\t<source file='~s' />\n"
%         "\t\t\t<target dev='~s' bus='~w' />\n"
%         "\t\t\t<driver cache='~s' />\n"
%         "\t\t</disk>\n", [File, Target, Bus, cache_mechanism()]),
%     gen_disks(T, Opts, NewAcc).
%
% gen_file({DiskId, Path, _Target}, _Opts) ->
%     case edog_conf:storage_module() of
%         edog_storage_proxy ->
%             io_lib:format("~s/~s", [edog_conf:storage_prefix(), Path]);
%         _ ->
%             %Ns = proplists:get_value(ns, Opts),
%             %Ip = proplists:get_value(ip, Opts),
%             %IscsiTarget = edog_iscsi:target_name(Ns, DiskId),
%             IscsiTarget = DiskId,
%             edog_iscsi:lun_path(IscsiTarget, ?ISCSI_LUN)
%     end.

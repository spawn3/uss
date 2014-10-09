-module(edog_master).
-behaviour(gen_server).
-export([
        start_link/0,
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3,

        %% Erlang cluster
        join/1,
        notify/1,
        notify_nodedown/1,

        cluster_restart/0,
        cluster_stop/0,
        stddisk_load/0,

        db_backup/0,
        db_backup/1,
        db_recover/0,
        db_recover/1,
        db_clear/0,

        master_info/0,
        get_agent_options/0,

        customer_create/1,
        customer_update/1,
        customer_destroy/1,
        customer_lock/1,
        customer_unlock/1,

        disk_create/3,
        disk_create/4,
        disk_destroy/1,
        disk_lock/1,
        disk_unlock/1,

        vm_create/8,
        vm_update/8,
        vm_destroy/1,
        vm_start/1,
        vm_start/2,
        vm_stop/1,
        vm_stop/2,
        vm_pause/1,
        vm_resume/1,
        vm_migrate/1,
        vm_migrate/2,
        vm_coldmigrate/2,
        vm_setvcpus/2,
        vm_setmem/2,

        libvirt_start/0,
        libvirt_stop/0,
        libvirt_show/0,

        %% Internal
        procs/0,
        procs_info/0,

        slave_get_all/0,
        hostnames/0,
        domain_info/1,

        table_dumpxml/0,
        table_dumpxml/1,
        slaves_dumpxml/0
    ]).

-include("edog_common.hrl").

-define(REGNAME,  {global, ?MODULE}).
-define(RPCNAME,  ?REGNAME).

-define(ETS_TABLE,  ?MODULE).

-type libvirt_op()  :: 'start' | 'stop' | 'show'.

%% -----------------------------------------------------------
%% API
%% -----------------------------------------------------------
start_link() ->
    gen_server:start_link(?REGNAME, ?MODULE, [], []).

%% -----------------------------------------------------------
%% DB
%% -----------------------------------------------------------
db_backup() ->
    edog_backup:do_backup_db().

db_backup(File) ->
    edog_backup:do_backup_db(File).

db_recover() ->
    edog_backup:do_recover_db().

db_recover(File) ->
    edog_backup:do_recover_db(File).

db_clear() ->
    edog_mnesia:clear_all_tables().

%% -----------------------------------------------------------
%% AGENT
%% -----------------------------------------------------------
join(Node) when is_atom(Node) ->
    gen_server:cast(?RPCNAME, {join, Node}).

notify(NotifySpec) ->
    gen_server:cast(?RPCNAME, {'NOTIFY', NotifySpec}).

notify_nodedown(Node) ->
    global:send(?MODULE, {nodedown, Node, [{nodedown_reason, ping}]}).

%% Node
master_info() ->
    call_wrapper(master_info).

get_agent_options() ->
    call_wrapper(get_agent_options).

%% CUST
customer_create(CustName) when is_list(CustName) ->
    customer_create({CustName, CustName, undefined, undefined,
        undefined, undefined, undefined, undefined});
customer_create({CustName, Company, Address, Contact, Tel, Cell, Email}) ->
    customer_create({CustName, CustName, Company, Address, Contact, Tel, Cell, Email});
customer_create({CustName, CustHome, Company, Address, Contact, Tel, Cell, Email}) ->
    CustId = cclib_uuid:uuid(),
    Cust = #cust_t{
        cust_id   = CustId,
        cust_name = CustName,
        cust_home = CustHome,
        locked    = false,
        locktime  = 0,
        company   = Company,
        address   = Address,
        contact   = Contact,
        telephone = Tel,
        cellphone = Cell,
        email     = Email
    },
    call_wrapper({customer_create, Cust}).

customer_update({CustId, CustName, Company, Address, Contact, Tel, Cell, Email}) ->
    Cust = #cust_t{
        cust_id   = CustId,
        cust_name = CustName,
        cust_home = CustName,
        locked    = false,
        locktime  = 0,
        company   = Company,
        address   = Address,
        contact   = Contact,
        telephone = Tel,
        cellphone = Cell,
        email     = Email
    },
    call_wrapper({customer_update, Cust}).

customer_destroy(CustId) ->
    call_wrapper({customer_destroy, CustId}).

customer_lock(CustId) ->
    call_wrapper({customer_lock, CustId}).

customer_unlock(CustId) ->
    call_wrapper({customer_unlock, CustId}).

%% --------------------------------------------------------------
%% DISK
%% --------------------------------------------------------------
disk_create(CustId, DiskAlias, Size) ->
    disk_create(CustId, DiskAlias, Size, false).

%% size: GB
-spec disk_create(string(), string(), number(), boolean()) -> any().
disk_create(CustId, DiskAlias, Size, Shared) ->
    Opts = [{boot, false}, {shared, Shared}],
    Disk = #disk_t{cust_id=CustId, disk_alias=DiskAlias, size=Size, canboot=Opts},
    call_wrapper({disk_create, Disk}).

disk_destroy(DiskID) ->
    disk_lock(DiskID).

disk_lock(DiskID) ->
    call_wrapper({disk_lock, DiskID}).

disk_unlock(DiskID) ->
    call_wrapper({disk_unlock, DiskID}).

%% VM
vm_create(CustId, VmName, Cpu, Mem, Pcis, VmImgID, Disks, Opts) ->
    vm_create(CustId, ?INVALID_UUID, VmName, Cpu, Mem, Pcis, VmImgID, Disks, Opts).

vm_create(CustId, PmID, VmName, Cpu, Mem, Pcis, VmImgID, Disks, Opts) ->
    vm_create(CustId, PmID, ?INVALID_UUID, VmName, Cpu, Mem, Pcis, VmImgID, Disks, Opts).

vm_create(CustId, PmID, VmId, VmName, Cpu, Mem, Pcis, VmImgID, Disks, Opts) ->
    Disks1 = [#disk_t{disk_id=DiskID, target=undefined} || DiskID <- Disks ],
    Pcis1  = p_pci_to_nic(Pcis),
    Vm = #vm_t{
        vm_id    = cclib_uuid:uuid(VmId),
        cust_id  = CustId,
        pm_id    = PmID,
        vm_name  = VmName,
        vm_cpu   = Cpu,
        vm_mem   = Mem,
        vm_imgid = VmImgID,
        vm_pcis  = Pcis1 },
    call_wrapper({vm_create, Vm, [{disks, Disks1}|Opts]}).

vm_update(VmId, VmName, Cpu, Mem, Pcis, VmImgID, Disks, Opts) ->
    Disks1 = [ #disk_t{disk_id=DiskID, target=undefined} || DiskID <- Disks ],
    Pcis1  = p_pci_to_nic(Pcis),
    Vm = #vm_t{
        vm_id    = VmId,
        vm_name  = VmName,
        vm_cpu   = Cpu,
        vm_mem   = Mem,
        vm_imgid = VmImgID,
        vm_pcis  = Pcis1 },
    call_wrapper({vm_update, Vm, [{disks, Disks1}|Opts]}).

p_pci_to_nic(Pcis) ->
    [#nic_t{bridge=Bridge, mac=cclib_uuid:to_mac(Mac), model=Model} || {Bridge, Mac, Model} <- Pcis].

vm_destroy(VmId) ->
    call_wrapper({vm_destroy, VmId}).

vm_start(VmId) ->
    call_wrapper({vm_start, VmId}).

vm_start(VmId, PmID) ->
    call_wrapper({vm_start, VmId, PmID}).

vm_stop(VmId) ->
    vm_stop(VmId, []).

vm_stop(VmId, Opts) ->
    call_wrapper({vm_stop, VmId, Opts}).

vm_pause(VmId) ->
    call_wrapper({vm_pause, VmId}).

vm_resume(VmId) ->
    call_wrapper({vm_resume, VmId}).

vm_migrate(VmId) ->
    call_wrapper({vm_migrate, VmId}).

vm_migrate(VmId, DestID) ->
    call_wrapper({vm_migrate, VmId, DestID}).

vm_coldmigrate(VmId, DestID) ->
    call_wrapper({vm_coldmigrate, VmId, DestID}).

vm_setvcpus(VmId, Vcpus) ->
    call_wrapper({vm_setvcpus, VmId, Vcpus}).

vm_setmem(VmId, Mem) ->
    call_wrapper({vm_setmem, VmId, Mem}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
libvirt_start() ->
    call_wrapper({libvirt, start}).
libvirt_stop() ->
    call_wrapper({libvirt, stop}).
libvirt_show() ->
    call_wrapper({libvirt, show}).

stddisk_load() ->
    call_wrapper(stddisk_load).

table_dumpxml() ->
    call_wrapper(table_dumpxml).

table_dumpxml(TabList) ->
    call_wrapper({table_dumpxml, TabList}).

slaves_dumpxml() ->
    call_wrapper(slaves_dumpxml).

%% -----------------------------------------------------------
cluster_restart() ->
    rpc:multicall(init, restart, []).

cluster_stop() ->
    rpc:multicall(init, stop, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_profile(master_info)       -> false;
is_profile(get_agent_options) -> false;
is_profile(slaves_dumpxml)    -> false;
is_profile(table_dumpxml)     -> false;
is_profile(_)                 -> true.

call_wrapper(Request, Timeout) ->
    ?PROFILE_BEGIN(),
    Reply =
    try
        gen_server:call(?RPCNAME, Request, Timeout)
    catch
        Class:Exception ->
            ?ERROR({Class, Exception,
                    Request, Timeout,
                    erlang:get_stacktrace(),
                    {manager_info, manager_info()}}),
        {error, Class}
    end,
    case is_profile(Request) of
        true  -> ?PROFILE_END(Request);
        false -> ok
    end,
    Reply.

call_wrapper(Request) ->
    call_wrapper(Request, ?TO_CALL).

vm_action_wrapper(Request, From, State) ->
    F = fun() ->
        %?PROFILE_BEGIN(),
        Reply =
        try
            edog_vm:vm_action(Request)
        catch
            Class:Exception ->
                ?ERROR({Class, Exception, Request, erlang:get_stacktrace()}),
            {error, Class}
        end,
        %?PROFILE_END(Request),
        gen_server:reply(From, Reply)
    end,
    spawn(F),
    {noreply, State}.

%% -----------------------------------------------------------
%% Callback
%% -----------------------------------------------------------
init(_Args) ->
    case edog_linux:is_ready() of
        true -> ok;
        false ->
            ?WARN({system_check_error, shutdown}),
            ?INIT_STOP()
    end,
    edog_trace:trace_manager(),

    ?PROFILE_BEGIN(),
    process_flag(trap_exit, true),
    try
        %% Global variables
        _ = ets:new(?ETS_TABLE,   [set, named_table, public]),
        _ = ets:new(edog_vm, [set, named_table, public]),

        %mnesia:subscribe(system),
        edog_queue:init(),
        edog_option:option_init(),

        net_kernel:monitor_nodes(true, [nodedown_reason]),

        spawn(fun() -> edog_iscsi:target_discover() end),

        ?INFO({conf, edog_conf:all()}),
        ?INFO({managers, edog_cluster:get_manager_nodes()}),
        ?INFO({agents, edog_cluster:get_agents()}),
        ?INFO({fence_ring, edog_cluster:fence_ring()}),
        ?INFO({?MODULE, inited})
    catch
        Error:Exception ->
            ?ERROR({Error, Exception, erlang:get_stacktrace()}),
            ?INIT_STOP()
    end,
    ?PROFILE_END(manager_init),
    {ok, []}.

terminate(_Reason, _State) ->
    ok.

handle_call(master_info, _From, _State) ->
    Managers = edog_cluster:get_manager_nodes(),
    Reply = [
        {node, node()},
        {self, self()},
        {application, ?APPLICATION},
        {managername, ?MASTERNAME},
        {agentname, ?AGENTNAME},
        {lib_dir, code:lib_dir(?APPLICATION)},
        {data_dir, ?APP_DATA},
        {mnesia_dir, mnesia:system_info(directory)},
        {managers, Managers},
        {agents, edog_cluster:get_agents()},
        {fence_ring, edog_cluster:fence_ring()},
        {nodes, [node()|nodes()]},
        {procs, procs()}
    ],
    {reply, Reply, _State};
handle_call(get_agent_options, _From, _State) ->
    Managers = edog_cluster:get_manager_nodes(),
    Reply = [
        {managers, Managers},
        {fence_ring, edog_cluster:fence_ring()},
        {agent_kill_vm_factor, edog_conf:agent_kill_vm_factor()},
        {agent_loop_interval, 15000}
    ],
    {reply, Reply, _State};
handle_call(table_dumpxml, _From, _State) ->
    Reply = edog_mnesia:table_dumpxml(edog_mnesia:get_all_tables()),
    {reply, Reply, _State};
handle_call({table_dumpxml, TabList}, _From, _State) ->
    Reply = edog_mnesia:table_dumpxml(TabList),
    {reply, Reply, _State};
handle_call(slaves_dumpxml,  _From, _State) ->
    Reply = do_dump_slaves(),
    {reply, Reply, _State};

%% IMAGE
handle_call(stddisk_load, _From, _State) ->
    Reply = edog_image:load(),
    {reply, Reply, _State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CUST
handle_call({customer_create, #cust_t{} = Cust} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = edog_user:customer_create(Cust),
    {reply, Reply, _State};
handle_call({customer_update, #cust_t{} = Cust} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = edog_user:customer_update(Cust),
    {reply, Reply, _State};
handle_call({customer_destroy, CustId} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = edog_user:customer_destroy(CustId),
    {reply, Reply, _State};
handle_call({customer_lock, CustId} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = edog_user:customer_lock(CustId),
    {reply, Reply, _State};
handle_call({customer_unlock, CustId} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = edog_user:customer_unlock(CustId),
    {reply, Reply, _State};

%% DISK
handle_call({disk_create, Disk} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = edog_disk:disk_action({create, Disk}),
    {reply, Reply, _State};
handle_call({disk_lock, DiskID} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = edog_disk:disk_action({lock, DiskID}),
    {reply, Reply, _State};
handle_call({disk_unlock, DiskID} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = edog_disk:disk_action({unlock, DiskID}),
    {reply, Reply, _State};

%% VM
handle_call({vm_create, #vm_t{vm_pcis=NicL} = _Vm, _Disks} = _Msg, _From, _State) ->
    MacL = [Mac || #nic_t{mac=Mac} <- NicL],
    case edog_mnesia:mac_isunique(MacL) of
        true ->
            vm_action_wrapper(_Msg, _From, _State);
        {error, _Reason} ->
            {reply, {error, _Reason}, _State}
    end;
handle_call({vm_update, #vm_t{vm_id=VmId, vm_pcis=NicL} = _Vm, _Disks} = _Msg, _From, _State) ->
    MacL = [Mac || #nic_t{mac=Mac} <- NicL],
    case edog_mnesia:mac_isunique(VmId, MacL) of
        true ->
            vm_action_wrapper(_Msg, _From, _State);
        {error, _Reason} ->
            {reply, {error, _Reason}, _State}
    end;
handle_call({vm_destroy, _VmId} = _Msg, _From, _State) ->
    vm_action_wrapper(_Msg, _From, _State);
handle_call({vm_start, VmId} = _Msg, _From, _State) ->
    Reply = edog_select:push({vm_start, VmId}),
    {reply, Reply, _State};
handle_call({vm_start, _VmId, _PmID} = _Msg, _From, _State) ->
    Msg2 = {vm_start, _VmId, [{ip, _PmID}]},
    vm_action_wrapper(Msg2, _From, _State);
handle_call({vm_stop, _VmId, _Opts} = _Msg, _From, _State) ->
    vm_action_wrapper(_Msg, _From, _State);
handle_call({vm_pause, _VmId} = _Msg, _From, _State) ->
    vm_action_wrapper(_Msg, _From, _State);
handle_call({vm_resume, _VmId} = _Msg, _From, _State) ->
    vm_action_wrapper(_Msg, _From, _State);
handle_call({vm_migrate, VmId} = _Msg, _From, _State) ->
    Reply = edog_select:push({vm_migrate, VmId}),
    {reply, Reply, _State};
handle_call({vm_migrate, _VmId, _DestID} = _Msg, _From, _State) ->
    vm_action_wrapper(_Msg, _From, _State);
handle_call({vm_coldmigrate, _VmId, _DestID} = _Msg, _From, _State) ->
    vm_action_wrapper(_Msg, _From, _State);
handle_call({vm_setvcpus, _VmId, _Vcpus} = _Msg, _From, _State) ->
    vm_action_wrapper(_Msg, _From, _State);
handle_call({vm_setmem, _VmId, _Mem} = _Msg, _From, _State) ->
    vm_action_wrapper(_Msg, _From, _State);

%% YFS
handle_call({libvirt, Action} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = do_libvirt(Action),
    {reply, Reply, _State};

handle_call(_Msg, _From, _State) ->
    ?WARN({unknown, _Msg, _From, _State}),
    {reply, ok, _State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CAST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({join, Node} = _Msg, _State) ->
    _Reply =
    case edog_cluster:is_valid_agent(Node) of
        true ->
            do_join(Node);
        false ->
            ?ERROR({invalid_agent, Node}),
            {error, Node}
    end,
    {noreply, _State};
handle_cast({'NOTIFY', NotifySpec} = _Msg, _State) ->
    ?INFO(_Msg),
    F = fun() -> edog_notify:notify(NotifySpec) end,
    spawn(F),
    {noreply, _State};
handle_cast(_Msg, _State) ->
    ?WARN({unknown, _Msg, _State}),
    {noreply, _State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INFO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_info({report, Node, Time, Data} = _Msg, _State) ->
    F = fun() ->
        case edog_cluster:is_valid_agent(Node) of
            true ->
                do_report(Node, Time, Data);
            false ->
                ?ERROR({invalid_agent, Node})
        end
    end,
    spawn(F),
    {noreply, _State};
% from edog_master_rt
handle_info({vm_check, #vm_t{} = Vm, IP, NewState} = _Msg, _State) ->
    F = fun() -> edog_vm:vm_check(Vm, IP, NewState) end,
    spawn(F),
    {noreply, _State};
handle_info({nodedown, _Node, _InfoList} = _Msg, _State) ->
    ?WARN(_Msg),
    F = fun() ->
        % TODO
        % [{nodedown_reason, net_tick_timeout}]
        % [{nodedown_reason, connection_closed}]
        case edog_cluster:is_valid_node(_Node) of
            true ->
                case net_adm:ping(_Node) of
                    pong ->
                        ok;
                    pang ->
                        case edog_fence:is_fence_passed() of
                            true ->
                                ?INFO({fence_ok, _Node}),
                                do_nodedown(_Node, _InfoList);
                            false ->
                                ?ERROR({fence_failed, _Node, _InfoList}),
                                edog_fence:do_after({_Node, _InfoList})
                        end
                end;
            false ->
                ok
        end
    end,
    spawn(F),
    {noreply, _State};
handle_info({nodeup, _Node, _InfoList} = _Msg, _State) ->
    ?INFO(_Msg),
    {noreply, _State};
handle_info({'EXIT', _From, _Reason} = _Msg, _State) ->
    ?INFO(_Msg),
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
do_nodedown(_Node, _InfoList) ->
    case edog_cluster:node_rule(_Node) of
        agent ->
            do_agent_nodedown(_Node, _InfoList);
        _ ->
            ok
    end.

do_agent_nodedown(_Node, _InfoList) ->
    do_leave(_Node),
    F = fun() ->
        Key = {nodedown, _Node},
        case ets:lookup(?ETS_TABLE, Key) of
            [] ->
                ets:insert(?ETS_TABLE, {Key, _InfoList}),
                edog_node_monitor:do_nodedown(_Node, _InfoList),
                ets:delete(?ETS_TABLE, Key);
            [_] ->
                ok
        end
    end,
    spawn(F).

do_dump_slaves() ->
    Slaves = slave_get_all(),
    F = fun(R, AccIn) ->
        [{{slaves, R}, Time, Data}] = ets:lookup(?ETS_TABLE, {slaves, R}),
        io_lib:format("~s<node name='~s' time='~w'>\n~s</node>\n", [
                AccIn, R, Time, edog_xml:rtdata_to_xml(Data)])
    end,
    Str = lists:foldl(F, "", Slaves),
    Xml = io_lib:format("<xml time='0'>\n~s</xml>", [Str]),
    %% file:write_file("/home/gj/nodes.xml", Xml),
    lists:flatten(Xml).

do_join(Node) ->
    ets:insert(?ETS_TABLE, {{slaves, Node}, 0, []}),
    edog_mnesia:node_update(Node).

do_leave(Node) ->
    edog_mnesia:node_delete(Node),
    case ets:lookup(?ETS_TABLE, {slaves, Node}) of
        [_] ->
            ets:delete(?ETS_TABLE, {slaves, Node}),
            ok;
        [] ->
            {error, no_entry}
    end.

-spec do_report(atom(), integer(), list()) -> any().
do_report(Node, Time, Data) ->
    ets:insert(?ETS_TABLE, {{slaves, Node}, Time, Data}),
    edog_mnesia:node_update(Node),

    case edog_common:keyfind(libvirtd, 1, Data, true) of
        true -> ok;
        false ->
            % TODO
            %?ERROR({libvirtd_not_started, Node, Time}),
            ok
    end,
    case edog_common:keyfind(proxy, 1, Data, true) of
        true -> ok;
        false ->
            %?ERROR({proxyserver_not_started, Node, Time}),
            ok
    end,

    Domains = edog_common:keyfind(domain, 1, Data, []),
    F1 =
    fun(#domain{uuid=VmId}=Domain) ->
        ets:insert(?ETS_TABLE, {{domain, VmId}, Domain}),
        %
        case ets:lookup(?ETS_TABLE, {vms, VmId}) of
            [] ->
                ets:insert(?ETS_TABLE, {{vms, VmId}, [Node]});
            [{{vms, VmId}, Nodes}] ->
                case lists:member(Node, Nodes) of
                    false ->
                        ets:insert(?ETS_TABLE, {{vms, VmId}, [Node|Nodes]});
                    true ->
                        ok
                end
        end
    end,
    lists:foreach(F1, Domains),

    F2 = fun(Domain) -> edog_vm:vm_check(Node, Domain) end,
    lists:foreach(F2, Domains),

    ok.

-spec do_libvirt(libvirt_op()) -> any().
do_libvirt(start) ->
    Servers = slave_get_all(),
    F = fun() ->
        lists:foreach(
            fun(Node) ->
                io:format("Server ~p~n", [Node]),
                io:format("---------------------------------------------------~n"),
                rpc:call(Node, edog_libvirt, libvirtd_start, [])
            end, Servers)
    end,
    spawn(F);
do_libvirt(stop) ->
    Servers = slave_get_all(),
    F = fun() ->
        lists:foreach(
            fun(Node) ->
                io:format("Server ~p~n", [Node]),
                io:format("---------------------------------------------------~n"),
                rpc:call(Node, edog_libvirt, libvirtd_stop, [])
            end, Servers)
    end,
    spawn(F);
do_libvirt(show) ->
    Servers = slave_get_all(),
    F = fun() ->
        lists:foreach(
            fun(Node) ->
                io:format("Server ~p~n", [Node]),
                io:format("---------------------------------------------------~n"),
                Cmd = "ps aux|grep libvirtd|grep -v grep",
                L = string:tokens(edog_common:cmd(Node, Cmd), "\n"),
                io:format("~p~n", [L])
            end, Servers)
    end,
    spawn(F).

%% -----------------------------------------------------------------------
slave_get_all() ->
    L = ets:match(?ETS_TABLE, {{slaves, '$1'}, '_', '_'}),
    lists:flatten(L).

hostnames() ->
    DataList = ets:match(?ETS_TABLE, {{slaves, '$1'}, '$2', '$3'}),
    F = fun([Node, Time, Data]) ->
        Host = edog_common:keyfind(hostname, 1, Data, []),
        {Node, Time, Host}
    end,
    [F(N) || N <- DataList].

domain_info(VmId) ->
    case ets:lookup(?ETS_TABLE, {domain, VmId}) of
        [{{domain, VmId}, Domain}] ->
            Domain;
        [] ->
            #domain{}
    end.

procs() ->
    L = [edog_master, edog_master_rt, edog_libvirt, edog_load, edog_select, edog_vmsup, edog_storage],
    [{X, cclib_utils:name_to_pid(X)} || X <- L].

procs_info() ->
    L = [edog_master, edog_master_rt, edog_libvirt, edog_load, edog_select, edog_vmsup, edog_storage],
    F = fun(X) ->
            case cclib_utils:name_to_pid(X) of
                Pid when is_pid(Pid) ->
                    {Pid, process_info(Pid)};
                undefined ->
                    undefined
            end
    end,
    [{X, F(X)} || X <- L].

manager_info() ->
    case cclib_utils:name_to_pid(edog_master) of
        Pid when is_pid(Pid) ->
            process_info(Pid);
        Other ->
            Other
    end.

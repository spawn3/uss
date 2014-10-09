-module(edog_xml).
-export([
        to_xml/1,
        rtdata_to_xml/1
    ]).
-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").
-include("edog_common.hrl").

check_undefined(A) ->
    case A of
        undefined -> "";
        _         -> A
    end.

to_xml({vm_disk, Disks}) ->
    vmdisk_to_xml(Disks);
to_xml({vm_pci, Pcis}) ->
    vmpci_to_xml(Pcis);
to_xml({disk, Disks}) ->
    disk_to_xml(Disks);
to_xml(#cust_t{cust_id=CustID, cust_name=CustName, locked=Locked, locktime=LockTime,
        company=Company, address=Address, contact=Contact,
        telephone=Tel, cellphone=Cell, email=Email
    } = _R) ->
    io_lib:format(
        "<customer id='~s' name='~s' company='~s' address='~s' contact='~s' telephone='~s' cellphone='~s' email='~s'>\n"
        "<status locked='~w' locktime='~w' />\n"
        "</customer>\n", [CustID,
            CustName,
            check_undefined(Company),
            check_undefined(Address),
            check_undefined(Contact),
            check_undefined(Tel),
            check_undefined(Cell),
            check_undefined(Email),
            Locked, LockTime]);
to_xml(#pm_t{pm_id=PmID, pm_ip=Ip, pm_cpu=Cpu, pm_mem=Mem, pm_bridges=Bri, status=Status} = _R) ->
    Status1 = case Status of
        running -> true;
        _       -> false
    end,
    % TODO pmid === vms
    {ok, Vms} = edog_mnesia:select_vm(Ip),
    {AllocMem, AllocCpu} = edog_table_vm:vm_resource_alloced(Ip),
    io_lib:format(
        "<node pmid='~s' ip='~p' alloc_mem='~B' alloc_cpu='~B' running='~w'>\n"
        "~s"
        "~s"
        "~s"
        "</node>\n", [
            PmID,
            length(Vms),
            AllocMem,
            AllocCpu,
            Status1,
            to_xml({cpu, Cpu}),
            to_xml({mem, Mem}),
            to_xml({bri, Bri})
        ]);
to_xml(#vm_t{vm_id=VmID, vm_name=Name, cust_id=CustID, pm_id=PmID,
        vm_cpu=Cpu, vm_mem=Mem, vm_pcis=Pcis, vm_imgid=BootDisk, port=Port,
        status=Status} = _R) ->
    IoDriver = edog_table_vm:get_iodriver(_R),
    RealMemoey =
    case Status of
        ?VM_STATE_RUNNING -> edog_table_vm:get_real_memory(_R);
        _                 -> Mem
    end,
    Domain =
    case Status of
        ?VM_STATE_RUNNING -> edog_master:domain_info(VmID);
        _                 -> #domain{}
    end,
    io_lib:format(
        "<node vmid='~s' name='~s' custid='~s' pmid='~s' port='~w' status='~w'
        iodriver='~s' cpu_util='~w' mem_util='~w' mem_real='~w' >\n"
        "<cpu number='~w' />\n"
        "<memory size='~w' />\n"
        "<boot disk='~s' />\n"
        "~s"
        "</node>\n", [VmID, Name, CustID, PmID, Port, Status, IoDriver,
            Domain#domain.cpu, Domain#domain.mem,
            RealMemoey,
            trunc(Cpu),
            Mem,
            BootDisk,
            to_xml({vm_pci, Pcis})
            ]);
to_xml(#disk_t{disk_id=DiskID, disk_alias=Alias, path=Path, size=Size, canboot=Boot,
        cust_id=CustID, locktime=LockTime, status=Status, vm_id=VmID, target=Target}) ->
    Xattr = edog_disk:canboot_to_xattr(Boot),
    Canboot = proplists:get_bool(boot, Xattr),
    Shared  = proplists:get_bool(shared, Xattr),
    io_lib:format(
        "<disk id='~s' alias='~s' custid='~s' canboot='~w' path='~s' size='~w' status='~w' locktime='~w' vmid='~s' target='~s' shared='~w' />\n",
        [DiskID, Alias, CustID, Canboot, Path, Size, Status, LockTime,
            check_undefined(VmID),
            check_undefined(Target),
            Shared
            ]);
to_xml(#disk_vm_t{id={DiskId, VmId}, target=Target}) ->
    io_lib:format("<disk_vm disk_id='~s' vm_id='~s' target='~s' />\n", [DiskId, VmId, Target]);
to_xml(#stddisk_t{disk_id=DiskID, path=Path, size=Size, os_type=Type, os_version=Ver} = _R) ->
    io_lib:format("<disk id='~s' path='~s' size='~w' os_type='~s' os_version='~s' />\n", [DiskID, Path, Size, Type, Ver]);
to_xml(#yfs_t{server={Name, IP, Num}, status=Status, pidlist=PidL, info=_Info} = _R) ->
    io_lib:format("<yfs name='~w' ip='~s' num='~B' status='~w'>\n"
        "~s"
        "</yfs>", [Name, IP, Num, Status, pidlist_to_xml(PidL)]);
to_xml(#option_t{key=Key, value=Value}) when is_list(Value) ->
    io_lib:format("<item key='~w' value='~s' />\n", [Key, Value]);
to_xml(#option_t{key=Key, value=Value}) ->
    io_lib:format("<item key='~w' value='~w' />\n", [Key, Value]);
to_xml({cpu, Cpu}) ->
    CoreList = [X||#cpu_info{count=X} <- Cpu],
    N = lists:sum(CoreList),
    io_lib:format("<cpu number='~w' />\n", [N]);
to_xml({mem, Mem}) ->
    Total    = proplists:get_value(mem_total, Mem, 0),
    Free     = proplists:get_value(mem_free, Mem, 0),
    Cached   = proplists:get_value(mem_cached, Mem, 0),
    Buffered = proplists:get_value(mem_buffered, Mem, 0),
    io_lib:format("<memory total='~w' used='~w' />\n", [
            ?BYTE_TO_KB(Total),
            ?BYTE_TO_KB(Total-Free-Cached-Buffered)]);
to_xml({bri, Bri}) ->
    Xml = bri_to_xml(Bri, ""),
    io_lib:format("<bridges>\n~s</bridges>\n", [Xml]);
to_xml({_, E2, E3}) ->
    io_lib:format("<record key='~s' value='~s'/>\n", [E2, E3]).

pidlist_to_xml(PidL) ->
    Xml = pidlist_to_xml(PidL, ""),
    io_lib:format("<pids>\n~s</pids>\n", [Xml]).

pidlist_to_xml([], Acc) -> Acc;
pidlist_to_xml([{Pid, Cmd}|T], Acc) ->
    NewAcc = io_lib:format("~s<pid id='~B' cmd='~s' />\n", [Acc, Pid, Cmd]),
    pidlist_to_xml(T, NewAcc).

disk_to_xml(Disks) ->
    Xml = disk_to_xml(Disks, ""),
    io_lib:format("<disks>\n~s</disks>\n", [Xml]).

disk_to_xml([], Acc) ->
    Acc;
disk_to_xml([#disk_t{disk_id=DiskID, disk_alias=Alias, path=Path, size=Size, canboot=Boot, vm_id=VmID, target=Target, status=Status, locktime=LockTime}|T], Acc) ->
    Xattr = edog_disk:canboot_to_xattr(Boot),
    Canboot = proplists:get_bool(boot, Xattr),
    Shared  = proplists:get_bool(shared, Xattr),
    NewAcc = io_lib:format("~s<disk id='~s' alias='~s' path='~s' size='~w' canboot='~w' status='~w' locktime='~w' vmid='~s' target='~s' shared='~w' />\n", [
            Acc, DiskID, Alias, Path, Size, Canboot, Status, LockTime,
            check_undefined(VmID),
            check_undefined(Target),
            Shared
        ]),
    disk_to_xml(T, NewAcc).

vmdisk_to_xml(Disks) ->
    Xml = vmdisk_to_xml(Disks, ""),
    io_lib:format("<vmdisks>\n~s</vmdisks>\n", [Xml]).

vmdisk_to_xml([], Acc) ->
    Acc;
vmdisk_to_xml([{Path, Target}|T], Acc) ->
    NewAcc = io_lib:format("~s<vmdisk path='~s' target='~s' />\n", [Acc, Path, Target]),
    vmdisk_to_xml(T, NewAcc).

vmpci_to_xml(Pcis) ->
    Xml = vmpci_to_xml(Pcis, ""),
    io_lib:format("<vmpcis>\n~s</vmpcis>\n", [Xml]).

vmpci_to_xml([], Acc) ->
    Acc;
vmpci_to_xml([#nic_t{bridge=Br, mac=Mac, model=Model}|T], Acc) ->
    NewAcc = io_lib:format("~s<vmpci bridge='~s' mac='~s' model='~s' />\n", [Acc, Br, Mac, Model]),
    vmpci_to_xml(T, NewAcc).

pm_to_xml(R) ->
    pm_to_xml(R, "").

pm_to_xml([], Acc) ->
    io_lib:format(
        "<physical_node>\n"
        "~s"
        "</physical_node>", [Acc]);
pm_to_xml([{Server, Info}|T], Acc) ->
    NewAcc = io_lib:format(
                "~s"
                "<node ip='~s'>\n"
                "~s"
                "</node>\n", [Acc, Server, info_to_xml(Info)]),
    pm_to_xml(T, NewAcc).

info_to_xml(Info) ->
    {cpu, Cpu} = lists:keyfind(cpu, 1, Info),
    {mem, Mem} = lists:keyfind(mem, 1, Info),
    {bridge, Bri} = lists:keyfind(bridge, 1, Info),
    io_lib:format(
        "~s"
        "~s"
        "~s", [
            to_xml({cpu, Cpu}),
            to_xml({mem, Mem}),
            to_xml({bri, Bri})
        ]).

bri_to_xml([], Acc) ->
    Acc;
bri_to_xml([{Name, Mac, Ip, Mask}|T], Acc) ->
    NewAcc = io_lib:format(
        "~s"
        "<bridge name='~s' mac='~s' ip='~s' mask='~s' />\n",
        [Acc, Name, Mac, Ip, Mask]),
    bri_to_xml(T, NewAcc).

pci_to_xml([], Acc) ->
    Acc;
pci_to_xml([H|T], Acc) ->
    NewAcc = io_lib:format(
        "~s"
        "~s", [Acc, pci_to_xml(H)]),
    pci_to_xml(T, NewAcc).

pci_to_xml({{Pci, Driver, Eth, Mac}, Virts}) when is_list(Virts) ->
    case Virts of
        [] ->
            io_lib:format("<pci num='~s' driver='~s' eth='~s' mac='~s' />\n",
                [Pci, Driver, Eth, Mac]);
        _ ->
            io_lib:format(
                "<pci num='~s' driver='~s' eth='~s' mac='~s'>\n"
                "~s"
                "</pci>\n",
                [Pci, Driver, Eth, Mac, virt_to_xml(Virts, "")])
    end.

virt_to_xml([], Acc) ->
    Acc;
virt_to_xml([{Pci, Driver, Eth, Mac}|T], Acc) ->
    NewAcc = io_lib:format(
                "~s"
                "<virt num='~s' driver='~s' eth='~s' mac='~s' />\n",
                [Acc, Pci, Driver, Eth, Mac]),
    virt_to_xml(T, NewAcc).

rtdata_to_xml(Data) when is_list(Data) ->
    %% CPU
    Avg1  = edog_common:keyfind(avg1, 1, Data, 0),
    Avg5  = edog_common:keyfind(avg5, 1, Data, 0),
    Avg15 = edog_common:keyfind(avg15, 1, Data, 0),

    %% Memory
    Mem       = edog_common:keyfind(mem, 1, Data, []),
    Total     = edog_common:keyfind(system_total_memory, 1, Mem, 0),
    Free      = edog_common:keyfind(free_memory, 1, Mem, 0),
    Cached    = edog_common:keyfind(cached_memory, 1, Mem, 0),
    Buffered  = edog_common:keyfind(buffered_memory, 1, Mem, 0),
    TotalSwap = edog_common:keyfind(total_swap, 1, Mem, 0),
    FreeSwap  = edog_common:keyfind(free_swap, 1, Mem, 0),

    {Netflow, TotalRx, TotalTx} = edog_common:keyfind(netflow, 1, Data, {[], 0, 0}),
    Domains = edog_common:keyfind(domain, 1, Data, []),

    io_lib:format(
        "<avg1 value='~w'/>\n"
        "<avg5 value='~w'/>\n"
        "<avg15 value='~w'/>\n"
        "<total value='~w'/>\n"
        "<free value='~w'/>\n"
        "<cached value='~w'/>\n"
        "<buffered value='~w'/>\n"
        "<totalswap value='~w'/>\n"
        "<freeswap value='~w'/>\n"
        "~s"
        "~s", [
            Avg1, Avg5, Avg15,
            Total, Free, Cached, Buffered, TotalSwap, FreeSwap,
            netflow_to_xml(TotalRx, TotalTx, Netflow, ""),
            domain_to_xml(Domains, "")
        ]).

netflow_to_xml(TotalRx, TotalTx, [], Acc) ->
    io_lib:format("<netflow total_rx='~w' total_tx='~w'>\n~s</netflow>\n", [
            TotalRx, TotalTx, Acc]);
netflow_to_xml(TotalRx, TotalTx, [#netflow{nic=Nic, rx_bytes=Rx, tx_bytes=Tx}|T], Acc) ->
    NewAcc = io_lib:format(
        "~s<if name='~s' rx_bytes='~w' tx_bytes='~w' />\n",
        [Acc, Nic, Rx, Tx]),
    netflow_to_xml(TotalRx, TotalTx, T, NewAcc).

domain_to_xml([], Acc) ->
    io_lib:format("<domains>\n~s</domains>\n", [Acc]);
domain_to_xml([#domain{pid=Pid,cpu=Cpu,mem=Mem,uuid=Uuid}|T], Acc) ->
    NewAcc = io_lib:format(
        "~s"
        "<domain uuid='~s' pid='~B' cpu='~w' memory='~w' />\n", [Acc, Uuid, Pid, Cpu, Mem]),
    domain_to_xml(T, NewAcc).

%%--------------------------------------------------------------
%% XML
%%--------------------------------------------------------------
xml_isvalid(Doc) ->
    try xmerl_scan:string(Doc) of
        {#xmlElement{}, _Rest} -> true;
        _Other -> false
    catch
        _:_ -> false
    end.

get_attr(Attrs, Name) ->
    case lists:keyfind(Name, 2, Attrs) of
        #xmlAttribute{value=Val} ->
            Val;
        false ->
            false
    end.

-module(edog_mnesia).
-export([
        start/0,
        node_delete/1,
        del_schema/1
    ]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("edog_common.hrl").

-define(WAIT_FOR_TABLES_TIMEOUT, 20000).

start() ->
    try
        NodeList = edog_cluster:get_manager_nodes(),
        ?INFO({masters, NodeList}),
        create_schema(NodeList),
        rpc:multicall(NodeList, mnesia, start, []),
        wait_for_tables(),

        ok
    catch
        Class:Reason ->
            ?ERROR({Class, Reason, erlang:get_stacktrace()}),
            ?INIT_STOP()
    end.

create_schema(NodeList) ->
    Dir = mnesia:system_info(directory),
    ?INFO({dir, Dir}),

    rpc:multicall(NodeList, filelib, ensure_dir, [Dir]),
    case cclib_mnesia:is_schema_inited(Dir) of
        false ->
            ?INFO({create_schema, NodeList, Dir}),
            edog_cluster:wait_managers(),
            rpc:multicall(NodeList, mnesia, stop, []),
            mnesia:create_schema(NodeList);
        true ->
            ok
    end,

    create_tables(NodeList).

create_tables(NodeList) ->
    rpc:multicall(NodeList, mnesia, start, []),
    ok = mnesia:wait_for_tables([schema], 3000),

    F = fun(Node) -> ?INFO({Node, rpc:call(Node, mnesia, system_info, [all])}) end,
    lists:foreach(F, NodeList),

    ?INFO({schema, mnesia:table_info(schema, all)}),
    Tables = mnesia:system_info(tables),

    ?CREATE_RAM_TABLE(Tables, NodeList, pm_t,     ordered_set),
    ?CREATE_RAM_TABLE(Tables, NodeList, option_t, ordered_set),

    ?CREATE_TABLE(Tables, NodeList, cust_t,    ordered_set),
    ?CREATE_TABLE(Tables, NodeList, stddisk_t, ordered_set),
    ?CREATE_TABLE(Tables, NodeList, disk_t,    ordered_set),
    ?CREATE_TABLE(Tables, NodeList, vm_t,      ordered_set),
    ?CREATE_TABLE(Tables, NodeList, disk_vm_t, ordered_set),

    ?INFO({schema, mnesia:table_info(schema, all)}),
    ok.

wait_for_tables() ->
    ?PROFILE_BEGIN(),
    Result = wait_for_tables(get_all_tables()),
    ?PROFILE_END(wait_for_tables),
    ?INFO({wait_for_tables, Result}).

wait_for_tables(Tables) ->
    case mnesia:wait_for_tables(Tables, 10000) of
        {timeout, RemainingTabs} ->
            mnesia_panic(RemainingTabs),
            {timeout, RemainingTabs};
        ok ->
            ok
    end.

mnesia_panic(RemainingTabs) ->
    ?WARN({force_load_table, RemainingTabs}),
    F = fun(Tab) -> mnesia:force_load_table(Tab) end,
    lists:foreach(F, RemainingTabs).

get_all_tables() ->
    [pm_t, option_t, cust_t, stddisk_t, vm_t, disk_t, disk_vm_t].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear_all_tables() ->
    lists:foreach(
        fun(Tab) -> mnesia:clear_table(Tab) end,
        get_all_tables() -- [option_t, pm_t]).

del_schema(Node) when is_atom(Node) ->
    mnesia:dump_to_textfile("./edog.data"),
    if
        Node =:= node() ->
            {error, cannot_del_local_copy};
        true ->
            do_del_schema(Node)
    end.

do_del_schema(Node) when is_atom(Node) ->
    TableList = get_all_tables() ++ [usedip_t, freeip_t],
    lists:foreach(
        fun(Table) -> mnesia:del_table_copy(Table, Node) end,
        TableList),

    rpc:call(Node, mnesia, stop, []),
    mnesia:del_table_copy(schema, Node).

table_dumpxml() ->
    lists:foreach(fun table_dumpxml/1, get_all_tables()).

table_dumpxml([], Acc) ->
    Xml = io_lib:format("<xml>\n~s</xml>", [Acc]),
    lists:flatten(Xml);
table_dumpxml([H|T], Acc) ->
    %?INFO({"table_dumpxml", H}),
    Tab = table_dumpxml(H),
    NewAcc = io_lib:format("~s~s", [Acc, Tab]),
    table_dumpxml(T, NewAcc).

table_dumpxml(TabList) when is_list(TabList) ->
    table_dumpxml(TabList, "");
table_dumpxml(Tab) when is_atom(Tab) ->
    F = fun(R, Acc) -> io_lib:format("~s~s", [Acc, edog_xml:to_xml(R)]) end,
    G = fun() -> mnesia:foldl(F, "", Tab) end,
    Xml = case mnesia:transaction(G) of
        {atomic, Result} ->
            Result;
        {aborted, _Reason} ->
            ?INFO([_Reason]),
            ""
    end,
    io_lib:format("<table name='~w'>\n~s</table>\n", [Tab, Xml]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_info(Node) ->
    case rpc:call(Node, cclib_os, info, [[cpu,mem,bridge]]) of
        {badrpc, Reason} ->
            ?ERROR({badrpc, Reason}),
            [{cpu, []}, {mem, []}, {bridge, []}];
        Res ->
            Res
    end.

node_update(Node) when is_atom(Node) ->
    Info = get_info(Node),
    Cpu = proplists:get_value(cpu, Info),
    Mem = proplists:get_value(mem, Info),
    Bri = proplists:get_value(bridge, Info),
    IP = cclib_node:get_ip(Node),
    F = fun() ->
        {Member, Row} =
        case mnesia:wread({pm_t, IP}) of
            [] ->
                {false, #pm_t{pm_id=IP, pm_ip=IP, pm_cpu=Cpu, pm_mem=Mem, pm_bridges=Bri, status=running}};
            [Pm] ->
                {true, Pm#pm_t{pm_cpu=Cpu, pm_mem=Mem, pm_bridges=Bri, status=running}}
        end,
        mnesia:write(Row),
        Member
    end,
    case mnesia:transaction(F) of
        {atomic, false} ->
            do_nodeup(Node);
        {atomic, true} ->
            ok;
        {aborted, _Reason} ->
            {error, _Reason}
    end.

node_delete(Node) when is_atom(Node) ->
    Ip = cclib_node:get_ip(Node),
    node_delete(Ip);
node_delete(Ip) ->
    cclib_mnesia:delete({pm_t, Ip}).

%% ----------------------------------------------------------------
%% @deprecated Please use the module {@link edog_vm} instead.
%% ----------------------------------------------------------------
-spec vm_action(tuple()) -> return_ok_t().
vm_action({vm_destroy, VmID}) ->
    %% TODO
    F = fun() ->
        case mnesia:read({vm_t, VmID}) of
            [] ->
                mnesia:abort(no_such_vm);
            [#vm_t{cust_id=_CustID, status=Status} = _Vm] ->
                if
                    Status =:= ?VM_STATE_CREATING orelse Status =:= ?VM_STATE_SHUTOFF ->
                        % update disk_vm_t
                        Q = qlc:q([X ||
                                #disk_vm_t{id={_, VmId2}} = X <- mnesia:table(disk_vm_t), VmId2 =:= VmID]),
                        Disks = qlc:e(Q),
                        lists:foreach(
                            fun(#disk_vm_t{id=Id}) -> mnesia:delete({disk_vm_t, Id}) end,
                            Disks),
                        %
                        edog_disk:delete_status([DiskId ||
                                #disk_vm_t{id={DiskId, _}} <- Disks], ?DISK_STATE_CREATING),
                        %
                        mnesia:delete({vm_t, VmID});
                     true ->
                        mnesia:abort(Status)
                end
        end
    end,
    case mnesia:transaction(F) of
        {atomic, _Val} ->
            edog_notify:notify(#notify_spec{op=vm_destroy, key=VmID, reply={ok, true}}),
            ?REPLY(ok);
        {aborted, _Reason} ->
            ?REPLY_ERROR(vm_destroy, VmID, _Reason)
    end;
vm_action({vm_coldmigrate, _VmID, _Dest}) ->
    ok.

%% ----------------------------------------------------------------
%%
%% ----------------------------------------------------------------
do_nodeup(Node) when is_atom(Node) ->
    PmID = cclib_node:get_ip(Node),
    edog_notify:notify(#notify_spec{op=pm_up, key=PmID, reply={ok, good}}).

get_allmac() ->
    F = fun() ->
        Q = qlc:q([Vm#vm_t.vm_pcis || Vm <- mnesia:table(vm_t)]),
        qlc:e(Q)
    end,
    case mnesia:transaction(F) of
        {atomic, NicL} ->
            {ok, [Nic#nic_t.mac || Nic <- lists:flatten(NicL)]};
        {aborted, _Reason} ->
            {error, _Reason}
    end.

get_othermac(VmID) ->
    F = fun() ->
        Q = qlc:q([Vm#vm_t.vm_pcis || Vm <- mnesia:table(vm_t), Vm#vm_t.vm_id =/= VmID]),
        qlc:e(Q)
    end,
    case mnesia:transaction(F) of
        {atomic, NicL} ->
            {ok, [Nic#nic_t.mac || Nic <- lists:flatten(NicL)]};
        {aborted, _Reason} ->
            {error, _Reason}
    end.

mac_dumpxml() ->
    case get_allmac() of
        {ok, MacL} ->
            mac_to_list(MacL, "");
        {error, _Reason} ->
            {error, _Reason}
    end.

mac_to_list([], Acc) ->
    Xml = io_lib:format("<xml>\n~s</xml>", [Acc]),
    lists:flatten(Xml);
mac_to_list([Mac|T], Acc) ->
    NewAcc = io_lib:format("~s<nic mac='~s' />\n", [Acc, Mac]),
    mac_to_list(T, NewAcc).

mac_isunique(MacL) ->
    case lists_unique(MacL) of
        true ->
            case get_allmac() of
                {ok, AllMac} ->
                    case lists_intersection(MacL, AllMac) of
                        [] -> true;
                        Res -> {error, {already_used, Res}}
                    end;
                {error, _Reason} ->
                    {error, _Reason}
            end;
        false ->
            {error, {duplicated, MacL}}
    end.

mac_isunique(VmID, MacL) ->
    case lists_unique(MacL) of
        true ->
            case get_othermac(VmID) of
                {ok, AllMac} ->
                    case lists_intersection(MacL, AllMac) of
                        [] -> true;
                        Res -> {error, {already_used, Res}}
                    end;
                {error, _Reason} ->
                    {error, _Reason}
            end;
        false ->
            {error, {duplicated, MacL}}
    end.

get_nodes()->
    F = fun() -> mnesia:all_keys(pm_t) end,
    {atomic,L1} = mnesia:transaction(F),
    lists:usort([X || X <- L1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lists_intersection(L1, L2) ->
    Set1 = sets:from_list(L1),
    Set2 = sets:from_list(L2),
    sets:to_list(sets:intersection(Set1, Set2)).

lists_unique(L) ->
    L1 = lists:usort(L),
    if
        length(L1) =:= length(L) -> true;
        true                     -> false
    end.

%% -----------------------------------------------------------
%% PRIVATE
%% -----------------------------------------------------------
select_vm(PmID) ->
    cclib_mnesia:q(qlc:q([ X || X <- mnesia:table(vm_t), X#vm_t.pm_id =:= PmID])).

running_vm_on_pm(Node) when is_atom(Node) ->
    running_vm_on_pm(cclib_node:get_ip(Node));
running_vm_on_pm(PmId) ->
    {ok, Vms} = select_vm(PmId),
    [Vm || #vm_t{status=Status} = Vm <- Vms,
        Status =:= ?VM_STATE_RUNNING orelse Status =:= ?VM_STATE_PAUSED].

select_pm(VmID) ->
    cclib_mnesia:q(qlc:q([ Y#pm_t.pm_ip ||
                X <- mnesia:table(vm_t),
                Y <- mnesia:table(pm_t),
                X#vm_t.vm_id =:= VmID,
                X#vm_t.pm_id =:= Y#pm_t.pm_id])).

select_cust_disk(CustId) ->
    cclib_mnesia:q(qlc:q([ X || X <- mnesia:table(disk_t), X#vm_t.cust_id =:= CustId ])).


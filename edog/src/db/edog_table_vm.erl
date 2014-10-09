-module(edog_table_vm).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("edog_common.hrl").

-define(TABLE, vm_t).

all() ->
    F = fun() ->
        Q = qlc:q([Vm || Vm <- mnesia:table(vm_t)]),
        qlc:e(Q)
    end,
    case mnesia:transaction(F) of
        {atomic, Vms} ->
            Vms;
        {aborted, _Reason} ->
            []
    end.

lookup(VmId) ->
    cclib_mnesia:lookup(?TABLE, VmId).

select(Status) ->
    case cclib_mnesia:q(qlc:q([X || X <- mnesia:table(vm_t), X#vm_t.status=:=Status])) of
        {ok, L} -> L ;
        _       -> []
    end.

%% --------------------------------------------------------------------------
%% EXTRA
%% --------------------------------------------------------------------------
extra_update(Extra, Key, Value) ->
    case proplists:is_defined(Key, Extra) of
        true ->
            [{Key, Value}|proplists:delete(Key, Extra)];
        false ->
            [{Key, Value}|Extra]
    end.

set_extra(VmId, Key, Value) ->
    F = fun() ->
        case mnesia:wread({vm_t, VmId}) of
            [#vm_t{extra=Extra} = Vm] ->
                Extra2 = extra_update(Extra, Key, Value),
                mnesia:write(Vm#vm_t{extra=Extra2});
            _ ->
                ok
        end
    end,
    cclib_mnesia:transaction(F).

clear_extra(VmId, Key) ->
    F = fun() ->
        case mnesia:wread({vm_t, VmId}) of
            [#vm_t{extra=Extra} = Vm] ->
                case proplists:is_defined(Key, Extra) of
                    true ->
                        Extra2 = extra_del(Extra, Key),
                        mnesia:write(Vm#vm_t{extra=Extra2});
                    false ->
                        ok
                end;
            _ ->
                ok
        end
    end,
    cclib_mnesia:transaction(F).

extra_del(Extra, Key) ->
    proplists:delete(Key, Extra).

%% --------------------------------------------------------------------------
%% REAL MEMORY
%% --------------------------------------------------------------------------
-define(REAL_MEMORY, real_memory).
-define(RESTART,     restart).
-define(IODRIVER,    iodriver).

%%
set_real_memory(VmId, Memory) ->
    set_extra(VmId, ?REAL_MEMORY, Memory).

set_restart(VmId) ->
    set_extra(VmId, ?RESTART, true).

set_iodriver(VmId, IoDriver) ->
    set_extra(VmId, ?IODRIVER, IoDriver).

%%
get_real_memory(#vm_t{vm_mem=VmMem, extra=Extra}) ->
    case proplists:get_value(?REAL_MEMORY, Extra) of
        undefined -> VmMem;
        Value     -> Value
    end.

get_restart(#vm_t{extra=Extra}) ->
    case proplists:get_value(?RESTART, Extra) of
        undefined -> false;
        Value     -> Value
    end;
get_restart(_) ->
    false.

get_iodriver(#vm_t{extra=Extra}) ->
    case proplists:get_value(?IODRIVER, Extra) of
        undefined -> virtio;
        Value     -> Value
    end;
get_iodriver(_) ->
    virtio.

%%
clear_real_memory(VmId) ->
    clear_extra(VmId, ?REAL_MEMORY).

clear_restart(VmId) ->
    clear_extra(VmId, ?RESTART).


%% --------------------------------------------------------------------------
%%
%% --------------------------------------------------------------------------
vm_resource_alloced(Node) when is_atom(Node) ->
    vm_resource_alloced(cclib_node:get_ip(Node));
vm_resource_alloced(PmId) ->
    L = edog_mnesia:running_vm_on_pm(PmId),
    F = fun(#vm_t{vm_cpu=Cpu0} = Vm, {Memory, Cpu}) ->
        Memory0 = get_real_memory(Vm),
        %?INFO(Vm),
        {Memory + Memory0, Cpu + Cpu0}
    end,
    lists:foldl(F, {0, 0}, L).

get_start_info(VmId) ->
    F = fun() ->
        case mnesia:read({vm_t, VmId}) of
            [#vm_t{cust_id=CustID} = Vm] ->
                [#cust_t{cust_home=Ns}] = mnesia:read({cust_t, CustID}),
                Q = qlc:q([{DiskId1, Path, Target} ||
                        #disk_vm_t{id={DiskId1, VmId2}, target=Target} <- mnesia:table(disk_vm_t),
                        #disk_t{disk_id=DiskId2, path=Path, status=Status} <- mnesia:table(disk_t),
                        VmId2 =:= VmId,
                        DiskId1 =:= DiskId2,
                        Status =:= ?DISK_STATE_CREATED]),
                Disks = qlc:e(Q),
                {Vm, Ns, Disks};
            [] ->
                mnesia:abort(no_such_vm)
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Value} ->
            {ok, Value};
        {abort, Reason} ->
            {error, Reason}
    end.

get_stop_info(VmId) ->
    F = fun() ->
        case mnesia:read({vm_t, VmId}) of
            [#vm_t{cust_id=CustID} = Vm] ->
                [#cust_t{cust_home=Ns}] = mnesia:read({cust_t, CustID}),
                {Vm, Ns};
            [] ->
                mnesia:abort(no_such_vm)
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Value} ->
            {ok, Value};
        {abort, Reason} ->
            {error, Reason}
    end.

update_state(VmId, NewState) ->
    F = fun() ->
        case mnesia:read({vm_t, VmId}) of
            [] ->
                mnesia:abort({no_such_vm, VmId});
            [#vm_t{} = Vm] ->
                ?INFO({update_state, VmId, Vm#vm_t.status, " ==> ", NewState}),
                case NewState of
                    ?VM_STATE_SHUTOFF ->
                        mnesia:write(Vm#vm_t{status=NewState, pm_id=?INVALID_UUID, port=-1});
                    _ ->
                        mnesia:write(Vm#vm_t{status=NewState})
                end
        end
    end,
    mnesia:transaction(F).

update_pmid(VmId, PmId, Port) ->
    F = fun() ->
        case mnesia:read({vm_t, VmId}) of
            [] ->
                mnesia:abort({no_such_vm, VmId});
            [#vm_t{} = Vm] ->
                mnesia:write(Vm#vm_t{pm_id=PmId, port=Port})
        end
    end,
    mnesia:transaction(F).

vm_destroy(VmId) ->
    F = fun() ->
        case mnesia:read({vm_t, VmId}) of
            [#vm_t{} = _Vm] ->
                % update disk_vm_t
                Q = qlc:q([X || #disk_vm_t{id={_DiskId, VmId2}} = X <-
                        mnesia:table(disk_vm_t),
                        VmId2 =:= VmId
                    ]),
                Disks = qlc:e(Q),
                lists:foreach(
                    fun(#disk_vm_t{id=Id}) -> mnesia:delete({disk_vm_t, Id}) end,
                    Disks),
                %
                edog_disk:delete_status([DiskId || #disk_vm_t{id={DiskId, _}} <- Disks],
                    ?DISK_STATE_CREATING),
                %
                mnesia:delete({vm_t, VmId})
        end
    end,
    case mnesia:transaction(F) of
        {atomic, _Val} ->
            {ok, true};
        {aborted, _Reason} ->
            {error, _Reason}
    end.

vm_isready(VmId) ->
    F = fun() ->
        Q = qlc:q([ Disk ||
                Disk <- mnesia:table(disk_t),
                Disk#disk_t.vm_id =:= VmId,
                Disk#disk_t.status =/= ?DISK_STATE_CREATED
            ]),
        qlc:e(Q)
    end,
    case mnesia:transaction(F) of
        {atomic, []} ->
            true;
        _ ->
            false
    end.

device_prefix(ide)    -> "hd";
device_prefix(sata)   -> "sd";
device_prefix(virtio) -> "vd";
device_prefix(_)      -> "vd".

device_first(ide)     -> "hda";
device_first(sata)    -> "sda";
device_first(virtio)  -> "vda";
device_first(_)       -> "vda".

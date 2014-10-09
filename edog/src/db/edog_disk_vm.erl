-module(edog_disk_vm).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("edog_common.hrl").

select_by_diskid(DiskId) ->
    cclib_mnesia:q(qlc:q([X ||
                #disk_vm_t{id={DiskId2, _}} = X <- mnesia:table(disk_vm_t),
                DiskId2 =:= DiskId])).

select_by_vmid(VmId) ->
    cclib_mnesia:q(qlc:q([ X ||
                #disk_vm_t{id={_DiskId, VmId2}} = X <- mnesia:table(disk_vm_t),
                VmId2 =:= VmId ])).

vms_by_diskid(DiskId) ->
    case select_by_diskid(DiskId) of
        {ok, L} ->
            [VmId || #disk_vm_t{id={_DiskId, VmId}} <- L];
        {error, _Reason} ->
            []
    end.

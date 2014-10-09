-module(edog_upgrade_shared_disk).
-compile(export_all).

-include("edog_common.hrl").

upgrade() ->
    ?INFO({?MODULE, upgrade}),
    F = fun
        (#disk_t{disk_id=_DiskId, vm_id=VmId, target=Target}, Acc)
        when VmId =:= undefined orelse Target =:= undefined ->
            Acc;
        (#disk_t{disk_id=DiskId, vm_id=VmId, target=Target}, Acc) ->
            mnesia:write(#disk_vm_t{id={DiskId, VmId}, target=Target}),
            Acc + 1
    end,
    G = fun() -> mnesia:foldl(F, 0, disk_t) end,
    mnesia:transaction(G).

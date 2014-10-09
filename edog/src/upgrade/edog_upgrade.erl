-module(edog_upgrade).
-compile(export_all).

start() ->
    edog_upgrade_shared_disk:upgrade(),
    edog_upgrade_vm_extra:upgrade(),
    ok.



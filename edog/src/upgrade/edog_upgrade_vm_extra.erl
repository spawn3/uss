-module(edog_upgrade_vm_extra).
-compile(export_all).

-include("edog_common.hrl").

upgrade() ->
    ?INFO({?MODULE, upgrade}),
    F = fun
        (OldRecord) when is_record(OldRecord, vm_t) ->
            OldRecord;
        (OldRecord) ->
            erlang:append_element(OldRecord, [])
    end,
    mnesia:transform_table(vm_t, F, record_info(fields, vm_t)).

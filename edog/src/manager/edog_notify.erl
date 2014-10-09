-module(edog_notify).
-compile(export_all).

-include("edog_common.hrl").

%% NOTIFY
-spec notify(#notify_spec{}) -> any().

notify(#notify_spec{op=pm_up, key=_PmID, reply=_Reply} = NotifySpec) ->
    edog_fg:notify(NotifySpec);
notify(#notify_spec{op=pm_down, key=_PmID, reply=_Reply} = NotifySpec) ->
    edog_fg:notify(NotifySpec);
%%
notify(#notify_spec{op=yfs_cp, key=DiskId, reply=Reply}) ->
    case edog_disk_vm:vms_by_diskid(DiskId) of
        [] ->
            {error, no_such_vm};
        [VmId|_] ->
            case Reply of
                {ok, _} ->
                    ?INFO({disk_ready, DiskId, Reply}),
                    edog_disk:update_status(DiskId, ?DISK_STATE_CREATED),
                    notify(#notify_spec{op=vm_create, key=VmId, reply=Reply});
                {error, Reason} ->
                    edog_mnesia:vm_action({vm_destroy, VmId}),
                    ?REPLY_ERROR(vm_create, VmId, Reason)
            end
    end;
notify(#notify_spec{op=disk_create, key=DiskId, reply=Reply}) ->
    case Reply of
        {ok, _} ->
            ?INFO({disk_ready, DiskId, Reply}),
            edog_disk:update_status(DiskId, ?DISK_STATE_CREATED);
        {error, _Reason} ->
            cclib_mnesia:delete(disk_t, DiskId),
            ?REPLY_ERROR(disk_create, DiskId, _Reason)
    end;
notify(#notify_spec{op=vm_create, key=_VmID, reply=_Reply} = NotifySpec) ->
    edog_vm:notify(NotifySpec),
    edog_fg:notify(NotifySpec);
notify(#notify_spec{op=vm_destroy, key=_VmID, reply=_Reply} = NotifySpec) ->
    edog_fg:notify(NotifySpec);
notify(#notify_spec{op=vm_update, key=_VmID, reply=_Reply} = NotifySpec) ->
    edog_fg:notify(NotifySpec);
%%
notify(#notify_spec{op=vm_start, key=_VmID, reply=_Reply} = NotifySpec) ->
    %edog_vm:notify(NotifySpec),
    edog_fg:notify(NotifySpec);
notify(#notify_spec{op=vm_stop, key=_VmID, reply=_Reply} = NotifySpec) ->
    %edog_vm:notify(NotifySpec),
    edog_fg:notify(NotifySpec);
notify(#notify_spec{op=vm_pause, key=_VmID, reply=_Reply} = NotifySpec) ->
    %edog_vm:notify(NotifySpec),
    edog_fg:notify(NotifySpec);
notify(#notify_spec{op=vm_resume, key=_VmID, reply=_Reply} = NotifySpec) ->
    %edog_vm:notify(NotifySpec),
    edog_fg:notify(NotifySpec);
notify(#notify_spec{op=vm_migrate, key=_VmID, reply=_Reply} = NotifySpec) ->
    %edog_vm:notify(NotifySpec),
    edog_fg:notify(NotifySpec).


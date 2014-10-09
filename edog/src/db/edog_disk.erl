-module(edog_disk).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("edog_common.hrl").

lookup(DiskId) ->
    cclib_mnesia:lookup(disk_t, DiskId).

is_free(#disk_t{disk_id=DiskId}) ->
    case edog_disk_vm:select_by_diskid(DiskId) of
        {ok, L} when length(L) > 0 -> false;
        _                          -> true
    end.

select_free_disk() ->
    case cclib_mnesia:q(qlc:q([ X || X <- mnesia:table(disk_t), is_free(X)])) of
        {ok, L} -> L;
        _       -> []
    end.

disk_action({create, #disk_t{cust_id=CustId, disk_alias=Alias, size=Size} = Disk}) ->
    F = fun() ->
        case mnesia:read({cust_t, CustId}) of
            [] ->
                mnesia:abort(no_such_user);
            [#cust_t{cust_home=Ns}] ->
                Format = edog_option:option_get(disk_format),
                {GlobalDiskId, DiskId} = edog_storage_util:diskid(Ns, Alias),
                case lookup(GlobalDiskId) of
                    [] ->
                        Path = edog_storage_util:disk_to_path(Ns, DiskId),
                        case edog_storage:disk_create(Ns, DiskId, Format, Size) of
                            {ok, _} ->
                                Disk1 = Disk#disk_t{
                                    disk_id=GlobalDiskId,
                                    path=Path,
                                    createtime=cclib_utils:now_to_integer(),
                                    status=?DISK_STATE_CREATED
                                },
                                mnesia:write(Disk1),
                                Disk1;
                            {error, _Reason} ->
                                ?WARN(_Reason),
                                mnesia:abort(_Reason)
                        end;
                    _ ->
                        mnesia:abort(already_exists)
                end
        end
    end,
    case cclib_mnesia:transaction(F, true) of
        {ok, #disk_t{} = Disk2} ->
            {ok, Disk2#disk_t.disk_id};
        {error, Reason} ->
            {error, Reason}
    end;
disk_action({destroy, DiskId}) ->
    F = fun() ->
        case mnesia:read({disk_t, DiskId}) of
            [] ->
                mnesia:abort(no_such_disk);
            [#disk_t{} = Disk] ->
                case is_free(Disk) of
                    true ->
                        case edog_storage:rm(Disk#disk_t.path) of
                            {ok, _} ->
                                mnesia:delete({disk_t, DiskId});
                            {error, Reason} ->
                                mnesia:abort(Reason)
                        end;
                    false ->
                        mnesia:abort(disk_used)
                end
        end
    end,
    cclib_mnesia:transaction(F, true);
disk_action({lock, DiskId}) ->
    F = fun() ->
        case mnesia:read({disk_t, DiskId}) of
            [] ->
                mnesia:abort(no_suck_disk);
            [#disk_t{} = Disk] ->
                case is_free(Disk) of
                    true ->
                        Now = cclib_utils:now_to_integer(),
                        mnesia:write(Disk#disk_t{status=?DISK_STATE_LOCKED, locktime=Now});
                    false ->
                        mnesia:abort(disk_used)
                end
        end
    end,
    cclib_mnesia:transaction(F);
disk_action({unlock, DiskId}) ->
    F = fun() ->
        case mnesia:read({disk_t, DiskId}) of
            [] ->
                mnesia:abort(no_suck_disk);
            [Disk] ->
                mnesia:write(Disk#disk_t{status=?DISK_STATE_CREATED, locktime=0})
        end
    end,
    cclib_mnesia:transaction(F).


%% TODO
canboot_to_xattr(Value) when is_boolean(Value) ->
    [{boot, Value}];
canboot_to_xattr(Value) when is_list(Value) ->
    Value.

add_owner(Disks, VmId, Opts) ->
    F = fun() ->
        case proplists:get_bool(reset, Opts) of
            true ->
                Q = qlc:q([{DiskId, VmId2} ||
                        #disk_vm_t{id={DiskId, VmId2}} <- mnesia:table(disk_vm_t),
                        VmId2 =:= VmId]),
                [mnesia:delete({disk_vm_t, Id}) || Id <- qlc:e(Q)];
            false ->
                ok
        end,
        lists:foreach(
            fun(#disk_t{disk_id=DiskId, target=Target}) ->
                case mnesia:read({disk_t, DiskId}) of
                    [#disk_t{}] ->
                        mnesia:write(#disk_vm_t{id={DiskId, VmId}, target=Target});
                    _ ->
                        mnesia:abort(no_such_disk)
                end
            end, Disks)
    end,
    mnesia:transaction(F).

get_disks(_VmId) ->
    [].

% [DiskId]
delete_status(Disks, Status) ->
    F = fun() ->
        lists:foreach(
            fun(DiskId) ->
                case mnesia:read({disk_t, DiskId}) of
                    [#disk_t{status=Status}] ->
                        mnesia:delete({disk_t, DiskId});
                    _ ->
                        ok
                end
            end, Disks)
    end,
    mnesia:transaction(F).

change_target(From, To) ->
    F = fun(#disk_t{target=Target} = X, _Acc) ->
        case Target of
            From -> mnesia:write(X#disk_t{target=To});
            _    -> ok
        end
    end,
    mnesia:transaction(fun() -> mnesia:foldl(F, [], disk_t) end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
disk_check() ->
    Now = cclib_utils:now_to_integer(),
    F = fun() ->
        Q = qlc:q([ Disk || Disk <- mnesia:table(disk_t),
                Disk#disk_t.status =:= ?DISK_STATE_CREATING,
                Disk#disk_t.createtime > 0,
                Now - Disk#disk_t.createtime > 1800
            ]),
        qlc:e(Q)
    end,
    case mnesia:transaction(F) of
        {atomic, []} -> ok;
        {atomic, L} when is_list(L) ->
            ?ERROR({disk_not_ready, L}),
            lists:foreach(fun(#disk_t{vm_id=VmID}) ->
                        edog_mnesia:vm_action({vm_destroy, VmID}) end, L);
        {aborted, _Reason} ->
            {aborted, _Reason}
    end.

update_status(DiskId, Status) ->
    F = fun() -> mnesia:read({disk_t, DiskId}) end,
    case mnesia:transaction(F) of
        {atomic, [#disk_t{} = Disk]} ->
            mnesia:transaction(fun() -> mnesia:write(Disk#disk_t{status=Status}) end);
        {atomic, []} ->
            {error, no_such_disk};
        {aborted, Reason} ->
            {error, Reason}
    end.


all_targets() ->
    Free = [DiskId || #disk_t{disk_id=DiskId} <- select_free_disk()],
    F = fun() ->
        Q = qlc:q([DiskId1 ||
                #disk_t{disk_id=DiskId1} <- mnesia:table(disk_t),
                #disk_vm_t{id={DiskId2, VmId1}} <- mnesia:table(disk_vm_t),
                DiskId1 =:= DiskId2,
                #vm_t{vm_id=VmId2, status=?VM_STATE_SHUTOFF} <- mnesia:table(vm_t),
                VmId1 =:= VmId2
            ]),
        qlc:e(Q)
    end,
    case mnesia:transaction(F) of
        {atomic, L}  -> Free ++ L;
        {aborted, _} -> Free
    end.


disk_on_node(DiskId) ->
    F = fun() ->
        Q = qlc:q([PmId ||
                #disk_t{disk_id=DiskId1} <- mnesia:table(disk_t),
                DiskId1 =:= DiskId,
                #disk_vm_t{id={DiskId2, VmId1}} <- mnesia:table(disk_vm_t),
                DiskId1 =:= DiskId2,
                #vm_t{vm_id=VmId2, status=Status, pm_id=PmId} <- mnesia:table(vm_t),
                VmId1 =:= VmId2,
                Status =/= ?VM_STATE_SHUTOFF
            ]),
        qlc:e(Q)
    end,
    case mnesia:transaction(F) of
        {atomic, L}  -> L;
        {aborted, _} -> []
    end.

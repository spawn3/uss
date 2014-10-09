-module(edog_storage_util).
-compile(export_all).

-include("edog_common.hrl").

diskid(Ns, DiskId) ->
    case edog_conf:storage_module() of
        edog_storage_proxy ->
            Uuid = cclib_uuid:uuid(DiskId),
            {Uuid, Uuid};
        _ ->
            {edog_iscsi:target_name(Ns, DiskId), cclib_utils:to_list(DiskId)}
    end.

disk_to_path(Ns, DiskId, _OsType, _OsVer) ->
    ClusterName = edog_conf:cluster_name(),
    Ns2 = disk_root(ClusterName, Ns),
    case edog_conf:storage_module() of
        edog_storage_proxy ->
            filename:join(["/", Ns2, DiskId, DiskId ++ ".img"]);
        _ ->
            edog_iscsi:lun_dir(Ns2, DiskId, ?ISCSI_LUN)
    end.

disk_to_path(Ns, DiskId) ->
    disk_to_path(Ns, DiskId, "", "").

disk_root([], UserName) ->
    cclib_utils:to_list(UserName);
disk_root(ClusterName, UserName) when is_list(ClusterName) ->
    cclib_utils:to_list(ClusterName) ++ "_" ++ cclib_utils:to_list(UserName).

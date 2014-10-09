%%%
%%% only 'a-z', '0-9', '.', '-', ':' is allowed
%%%

-module(edog_storage_iscsi).
-export([ns_create/1, yfs_mkdisk/1, yfs_copyfile/1]).
-export([rm/1]).

-include("edog_common.hrl").

ns_create(Path) ->
    edog_yfs_vol:make_ns(Path).

yfs_mkdisk([Ns, DiskId, Format, Size]) ->
    ?INFO([Ns, DiskId, Format, Size]),
    Target = target_name(Ns, DiskId),
    edog_iscsi:target_new_with_default_account(Target),
    edog_iscsi:lun_new(Target, ?ISCSI_LUN, Size).

yfs_copyfile([Src, Ns, DiskId]) ->
    ?INFO([Src, Ns, DiskId]),
    Src2  = path_add_prefix(Src),
    Path2 = edog_iscsi:lun_name(Ns, DiskId, ?ISCSI_LUN),
    Target = target_name(Ns, DiskId),
    edog_iscsi:target_new_with_default_account(Target),
    edog_yfs_shell:set_xattr(edog_iscsi:target_dir(Ns, DiskId), "iscsi.is_target", "true"),
    case edog_yfs_shell:copyfile([Src2, Path2]) of
        {ok, #cmd_info{}} ->
            LunDir = edog_iscsi:lun_dir(Ns, DiskId, ?ISCSI_LUN),
            edog_yfs_shell:set_xattr(LunDir, "prealloc", "true"),
            edog_yfs_shell:set_xattr(LunDir, "writeback", "false"),
            {ok, true};
        {error, #cmd_info{code=?E_FILE_EXISTS}=Info} ->
            ?WARN(Info),
            edog_iscsi:lun_del(Target, ?ISCSI_LUN),
            %edog_iscsi:target_del(Target),
            timer:sleep(1000),
            yfs_copyfile([Src, Ns, DiskId]);
        {error, #cmd_info{data=Data}=Info} ->
            ?WARN(Info),
            {error, iolist_to_binary(Data)}
    end.

rm(Path) ->
    ?INFO({rm, Path}),
    {Target, LunId} = path_to_target(Path),
    case edog_iscsi:lun_del(Target, LunId) of
        {ok, #cmd_info{}} ->
            {ok, true};
        {error, #cmd_info{}=Info} ->
            {error, Info}
    end.

%% -----------------------------------------------------------------------
%%
%% -----------------------------------------------------------------------

target_name(Ns, DiskId) ->
    edog_iscsi:target_name(Ns, DiskId).

path_add_prefix(Path) ->
    edog_conf:storage_prefix() ++ "/" ++ Path.

path_to_target(Path) ->
    case string:tokens(Path, "/") of
        [H1, H2, LunId|_] ->
            {target_name(H1, H2), list_to_integer(LunId)};
        [H1, H2] ->
            {target_name(H1, H2), 0}
    end.

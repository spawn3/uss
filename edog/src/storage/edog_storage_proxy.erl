-module(edog_storage_proxy).
-export([ns_create/1, yfs_mkdisk/1, yfs_copyfile/1]).
-export([rm/1]).
-export([mkoss/0]).
-export([ensure_proxy/0, start_proxy/0]).

-include("edog_common.hrl").
-include("edog_const.hrl").

mkoss() ->
    edog_yfs_vol:mkvol(edog_conf:storage_prefix()).

ns_create(Path) ->
    Path2 = path_add_prefix(Path),
    edog_yfs_shell:mkdir(Path2).

yfs_mkdisk([Ns, DiskId, Format, Size]) ->
    Path = edog_storage_util:disk_to_path(Ns, DiskId),
    Path2 = path_add_prefix(Path),
    edog_yfs_shell:create_img([Path2, Format, Size]).

yfs_copyfile([Src, Ns, DiskId]) ->
    Path = edog_storage_util:disk_to_path(Ns, DiskId),
    Src2  = path_add_prefix(Src),
    Path2 = path_add_prefix(Path),
    case edog_yfs_shell:copyfile([Src2, Path2]) of
        {ok, Info} ->
            edog_yfs_shell:set_xattr(Path2, "prealloc", "true"),
            edog_yfs_shell:set_xattr(Path2, "writeback", "false"),
            {ok, Info};
        {error, Info} ->
            {error, Info}
    end.

rm(Path) ->
    Path2 = path_add_prefix(Path),
    edog_yfs_shell:rm(Path2).

ensure_proxy() ->
    case cclib_utils:is_process_exists("proxy_server") of
        true ->
            true;
        false ->
            ?ERROR("proxy server not started"),
            %start_proxy()
            false
    end.

start_proxy() ->
    Cmd = io_lib:format("~s/sbin/proxy_server", [?YFS_APP]),
    ?EXECUTE(Cmd).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
path_add_prefix(Path) ->
    edog_conf:storage_prefix() ++ "/" ++ Path.

-module(edog_storage_local).
-export([ns_create/1, yfs_mkdisk/1, yfs_copyfile/1]).
-compile(export_all).

-include("edog_common.hrl").

-define(LOCAL_STORAGE_PREFIX, "/sysy/local/").

ns_create(Path) ->
    filelib:ensure_dir(lists:concat([?LOCAL_STORAGE_PREFIX, Path, "/"])).

yfs_mkdisk([Home, DiskId, Format, Size]) ->
    Path = get_image_path(Home, DiskId),
    filelib:ensure_dir(Path),
    Cmd = io_lib:format("~s create -f ~s ~s ~wG", [
            edog_conf:bin_kvm_img(),
            Format, Path, Size]),
    ?EXECUTE(Cmd).

yfs_copyfile([Src, Home, DiskId]) ->
    SrcPath = get_src_path(Src),
    Path = get_image_path(Home, DiskId),
    filelib:ensure_dir(Path),
    Cmd = io_lib:format("cp ~s ~s", [SrcPath, Path]),
    ?EXECUTE(Cmd).

%% PRIVATE
get_image_path(Home, DiskId) ->
    filename:join([?LOCAL_STORAGE_PREFIX, Home, DiskId, DiskId ++ ".img"]).

get_src_path(Src) ->
    filename:join([?LOCAL_STORAGE_PREFIX, Src]).

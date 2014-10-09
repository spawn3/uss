-module(edog_image).
-compile(export_all).

-include("edog_common.hrl").

-define(TABLE, stddisk_t).

default_conf() ->
    filename:join([cclib:conf_dir(), "stddisk.conf"]).

load() ->
    File = default_conf(),
    load(File).

load(File) ->
    L = cclib_mnesia:i(?TABLE),
    PathId = [{Path, Id} || #stddisk_t{disk_id=Id, path=Path} <- L],
    ?INFO(PathId),
    FGetId = fun(Path) -> proplists:get_value(Path, PathId, cclib_uuid:uuid()) end,

    {ok, Disks} = file:consult(File),
    Disks1 = [#stddisk_t{disk_id=FGetId(Path), path=Path, size=Size, os_type=Type, os_version=Ver} ||
        {Path, Size, Type, Ver} <- Disks],
    ?INFO(Disks1),

    mnesia:clear_table(?TABLE),
    F = fun() -> lists:foreach(fun(X) -> mnesia:write(X) end, Disks1) end,
    mnesia:transaction(F).

save() ->
    File = default_conf(),
    save(File).

save(File) ->
    L = cclib_mnesia:i(stddisk),
    Disks = [stddisk_to_tuple(X) || X <- L],
    cclib_file:unconsult(File, Disks).

stddisk_to_tuple(#stddisk_t{path=Path, size=Size, os_type=Type, os_version=Ver}) ->
    {Path, Size, Type, Ver}.

insert(#stddisk_t{}) ->
    ok.

delete(_Id) ->
    ok.

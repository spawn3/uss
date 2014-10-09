-module(edog_storage_mock).
-export([ns_create/1, yfs_mkdisk/1, yfs_copyfile/1]).
-export([rm/1]).

-include("edog_common.hrl").

ns_create(Path) ->
    ?INFO([Path]),
    {ok, true}.

yfs_mkdisk([Path, Format, Size]) ->
    ?INFO([Path, Format, Size]),
    {ok, true}.

yfs_copyfile([Src, Path]) ->
    ?INFO([Src, Path]),
    {ok, true}.

rm(Path) ->
    ?INFO([Path]),
    {ok, true}.

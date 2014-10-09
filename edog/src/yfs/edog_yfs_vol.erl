-module(edog_yfs_vol).
-export([
        mkvol/1,
        make_ns/1
    ]).

-include("edog_common.hrl").

mkvol(Vol) ->
    Cmd = io_lib:format("~s/bin/ylvm --create ~s", [?YFS_APP, Vol]),
    _Result = ?EXECUTE(Cmd),
    case _Result of
        [] -> {ok, true};
        _ -> {error, _Result}
    end.

make_ns(Ns) ->
    Cmd = io_lib:format("~s/bin/ylvm --create ~s", [?YFS_APP, Ns]),
    cclib_cmd:exec(Cmd).

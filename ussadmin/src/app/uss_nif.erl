-module(uss_nif).
-compile(export_all).

-include("uss_common.hrl").

start() ->
    %Path = "/sysy/yfs/ussadmin/uss_app",
    Path = code:lib_dir(ussadmin),
    case erlang:load_nif(filename:join([Path, "ebin/uss_nif"]), 0) of
        ok -> ok;
        {error, Reason} ->
            ?ERROR_REPORT(Reason),
            {error, Reason}
    end.

hello() ->
    nif_error().

trylock(_Path) ->
    nif_error().

nif_error() ->
    exit({nif_not_loaded,module,?MODULE,line,?LINE}).

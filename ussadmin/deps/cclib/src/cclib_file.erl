-module(cclib_file).
-compile(export_all).

-include("cclib.hrl").

file_exist(Which) ->
    case filelib:is_regular(Which) of
        true ->
            ?INFO_REPORT(Which),
            true;
        false ->
            ?ERROR_REPORT(Which),
            false
    end.

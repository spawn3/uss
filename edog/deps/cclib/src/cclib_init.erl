-module(cclib_init).
-compile(export_all).

-include("cclib.hrl").

restart() ->
    ?INFO(restart),
    init:restart().

stop() ->
    ?INFO(stop),
    init:stop().

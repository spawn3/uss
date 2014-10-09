-ifndef(__CCLIB_INIT_HRL__).
-define(__CCLIB_INIT_HRL__, true).

-define(INIT_RESTART(),
    begin
            ?FATAL(restart),
            init:restart()
    end).

-define(INIT_STOP(),
    begin
            ?FATAL(stop),
            init:stop()
    end).

-endif.

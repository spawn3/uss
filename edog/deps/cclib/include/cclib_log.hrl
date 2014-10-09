-ifndef(__CCLIB_LOG_HRL__).
-define(__CCLIB_LOG_HRL__, true).

-define(DEBUG(X), log4erl:debug("[~p:~p] ~p", [?MODULE, ?LINE, X])).
-define(INFO(X),  log4erl:info ("[~p:~p] ~p", [?MODULE, ?LINE, X])).
-define(WARN(X),  log4erl:warn ("[~p:~p] ~p", [?MODULE, ?LINE, X])).
-define(ERROR(X), log4erl:error("[~p:~p] ~p", [?MODULE, ?LINE, X])).
-define(FATAL(X), log4erl:fatal("[~p:~p] ~p", [?MODULE, ?LINE, X])).

-endif.

-ifndef(__EDOG_TYPES_HRL__).
-define(__EDOG_TYPES_HRL__, true).

-type ip()   :: string().
-type return_t() :: {ok, _} | {error, _}.
-type return_ok_t() :: ok | {error, _}.
-type filename() :: string().
-type nodename() :: {atom(), node()}.
-type domainstate_t() :: 'shutoff' | 'running' | 'paused'.

-endif.

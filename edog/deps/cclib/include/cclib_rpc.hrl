-ifndef(__CCLIB_RPC_HRL__).
-define(__CCLIB_RPC_HRL__, true).

%% RPC
-define(TO_RPC_SHORT,  10000).
-define(TO_RPC_NORMAL, 60000).
-define(TO_RPC_LONG,   1800000).
-define(TO_CALL,       30000).

-define(RPC_CALL(Node, M, F, A),
    begin
        ?INFO({Node, M, F, A}),
        rpc:call(Node, M, F, A)
    end).

-define(RPC_CALL_TO(Node, M, F, A, TO),
    begin
        ?INFO({Node, M, F, A, TO}),
        rpc:call(Node, M, F, A, TO)
    end).

-endif.

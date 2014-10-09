-module(edog_reconf).
-compile(export_all).

-include("edog_common.hrl").

%% OFFLINE
backup(Node) ->
    rpc_call(Node, ?MODULE, do_backup, []).

backup(Node, File) ->
    rpc_call(Node, ?MODULE, do_backup, [File]).

recover(Node) when is_atom(Node) ->
    rpc_call(Node, ?MODULE, do_recover, []).

recover(Node, Txt) when is_atom(Node) ->
    rpc_call(Node, ?MODULE, do_recover, [Txt]).

%% --------------------------------------------------------------
%% DO
%% --------------------------------------------------------------
show() ->
    ok = cclib_mnesia:start(),
    cclib_mnesia:print_copies(),
    cclib_mnesia:stop().

do_backup() ->
    ok = cclib_mnesia:start(),
    edog_backup:do_backup_db(),
    cclib_mnesia:stop().

do_backup(File) ->
    ok = cclib_mnesia:start(),
    edog_backup:do_backup_db(File),
    cclib_mnesia:stop().

do_recover() ->
    ok = cclib_mnesia:start(),
    edog_backup:do_recover_db(),
    cclib_mnesia:stop().

do_recover(File) ->
    ok = cclib_mnesia:start(),
    edog_backup:do_recover_db(File),
    cclib_mnesia:stop().

add_node(Node) ->
    io:format("~p~n", [[{local, node()}, {remote, Node}]]),
    do_backup(),
    %
    cclib_mnesia:add_node(Node),
    ok.

del_node(Node) ->
    do_backup(),
    %
    cclib_mnesia:del_node(Node),
    ok.

rpc_call(Node, M, F, A) ->
    case rpc:call(Node, M, F, A) of
        {badrpc, Reason} ->
            ?INFO({"!!! ", [{local, node()}, {remote, Node}, {badrpc, Reason}]}),
            {badrpc, Reason};
        Other ->
            ?INFO({Other}),
            Other
    end.

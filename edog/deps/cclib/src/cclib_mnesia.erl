-module(cclib_mnesia).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("cclib.hrl").

-define(SCHEMA, "schema.DAT").

i([]) ->
    ok;
i([H|T]) ->
    ?INFO(H),
    ?INFO(i(H)),
    i(T);
i(Tab) ->
    case q(qlc:q([X || X <- mnesia:table(Tab)])) of
        {ok, V} ->
            V;
        {error, Reason} ->
            {error, Reason}
    end.

q(Q) ->
    F = fun() -> qlc:e(Q) end,
    case mnesia:transaction(F) of
        {atomic, Val} ->
            {ok, Val};
        {aborted, Reason} ->
            {error, Reason}
    end.

transaction(F) ->
    transaction(F, true).

-spec transaction(any(), boolean()) -> any().
transaction(F, IsReturnValue) ->
    case mnesia:transaction(F) of
        {atomic, _Value} ->
            case IsReturnValue of
                true -> {ok, _Value};
                false -> ok
            end;
        {aborted, Reason} ->
            {error, Reason}
    end.

select(Pattern) ->
    F = fun() -> mnesia:match_object(Pattern) end,
    case mnesia:transaction(F) of
        {atomic, L} when is_list(L) -> {ok, L};
        {aborted, Reason} -> {error, Reason}
    end.

delete({Tab, Key}) ->
    F = fun() -> mnesia:delete({Tab, Key}) end,
    case mnesia:transaction(F) of
        ok ->
            ok;
        _Other ->
            _Other
    end.

delete(Tab, Key) ->
    delete({Tab, Key}).

exists(Table, Key) ->
    F = fun() -> mnesia:read(Table, Key) end,
    case mnesia:transaction(F) of
        {aborted, _Reason} -> false;
        {atomic, []} -> false;
        {atomic, _} -> true
    end.

%%%%%%%%%%%% SCHEMA %%%%%%%%%%%
set_dbdir(NodeList, DbDir) ->
    rpc:multicall(NodeList, application, stop, [mnesia]),
    rpc:multicall(NodeList, application, load, [mnesia]),
    rpc:multicall(NodeList, application, set_env, [mnesia, dir, DbDir]),

    ok.

is_schema_inited(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            case file:list_dir(Dir) of
                {ok, L} ->
                    lists:member(?SCHEMA, L);
                _ ->
                    false
            end;
        false ->
            false
    end.

start() ->
    start(10000).

start(Timeout) ->
    ok = mnesia:start(),
    case mnesia:wait_for_tables(mnesia:system_info(tables), Timeout) of
        ok ->
            ok;
        {timeout, BadTableList} ->
            {timeout, BadTableList};
        {error, Reason} ->
            {error, Reason}
    end.

stop() ->
    mnesia:stop().

add_node(Node) ->
    try
        pong = net_adm:ping(Node),
        rpc_call(Node, mnesia, start, []),

        ok = start(),

        % schema
        mnesia:change_config(extra_db_nodes, [Node]),
        mnesia:change_table_copy_type(schema, Node, disc_copies),

        % data tables
        Tables = mnesia:system_info(tables) -- [schema],
        lists:foreach(
            fun(Tab) ->
                add_table_copy(Tab, Node)
            end, Tables),
        print_copies(),

        %
        rpc_call(Node, mnesia, stop, []),
        stop()
    catch
        Class:Exception ->
            ?INFO({"!!! ", Class, Exception, erlang:get_stacktrace()})
    end.

add_table_copy(Tab, Node) ->
    Storage = mnesia:table_info(Tab, storage_type),
    case mnesia:add_table_copy(Tab, Node, Storage) of
        {atomic, ok} ->
            ok;
        %{aborted, {not_active, schema, Node}} ->
        %{aborted, {already_exists, Tab, Node}} ->
        {aborted, Reason} ->
            ?INFO({"!!! ", {aborted, Reason}}),
            ok
    end.

del_node(Node) ->
    rpc_call(Node, mnesia, stop, []),
    ok = start(),

    case mnesia:del_table_copy(schema, Node) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            ?INFO({"!!! ", {aborted, Reason}}),
            ok
    end,
    rpc_call(Node, mnesia, delete_schema, [[Node]]),

    %
    print_copies(),
    stop(),
    ok.

get_copies(Tab) ->
    Storage = mnesia:table_info(Tab, storage_type),
    {Tab, Storage, mnesia:table_info(Tab, Storage)}.

print_copies() ->
    lists:foreach(
        fun(Tab) -> io:format("~p~n", [get_copies(Tab)]) end,
        mnesia:system_info(tables)).

db_nodes() ->
    mnesia:system_info(db_nodes) ++ mnesia:system_info(extra_db_nodes).

add_table_copy(Tab, Node, Type) ->
    case mnesia:table_info(Tab, Type) of
        [] ->
            ok;
        L when is_list(L) ->
            mnesia:add_table_copy(Tab, Node, Type)
    end.

rpc_call(Node, M, F, A) ->
    case rpc:call(Node, M, F, A) of
        {badrpc, Reason} ->
            ?INFO({"!!! ", {badrpc, Reason}}),
            {badrpc, Reason};
        {error, Reason} ->
            ?INFO({"!!! ", {error, Reason}}),
            {error, Reason};
        _ ->
            ok
    end.

lookup(Tab, Key) ->
    F = fun() -> mnesia:read({Tab, Key}) end,
    case mnesia:transaction(F) of
        {atomic, L} ->
            L;
        {aborted, _Reason} ->
            []
    end.

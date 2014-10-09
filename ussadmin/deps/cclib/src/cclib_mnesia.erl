-module(cclib_mnesia).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("cclib_debug.hrl").

-define(SCHEMA, "schema.DAT").

i([]) ->
    ok;
i([H|T]) ->
    ?INFO_REPORT(H),
    ?INFO_REPORT(i(H)),
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

delete_table(Table) ->
    % TODO
    {ok, NodeList} = clib_utils:get_managers(),
    F = fun() ->
        lists:foreach(
            fun(X) -> mnesia:del_table_copy(Table, X) end,
            NodeList)
    end,
    transaction(F).

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

add_node(Node) ->
    rpc:call(Node, mnesia, start, []),
    % schema
    mnesia:change_config(extra_db_nodes, [Node]),
    mnesia:change_table_copy_type(schema, Node, disc_copies),
    % data tables
    Tables = mnesia:system_info(tables) -- [schema],
    [add_table_copy(Tab, Node) || Tab <- Tables].

add_table_copy(Tab, Node) ->
    % mnesia:system_info(Tab, storage_type),
    add_table_copy(Tab, Node, disc_copies),
    add_table_copy(Tab, Node, ram_copies),
    add_table_copy(Tab, Node, disc_only_copies).

add_table_copy(Tab, Node, Type) ->
    case mnesia:table_info(Tab, Type) of
        [] ->
            ok;
        L when is_list(L) ->
            mnesia:add_table_copy(Tab, Node, Type)
    end.

del_node(Node) ->
    rpc:call(Node, mnesia, stop, []),
    mnesia:del_table_copy(schema, Node),
    rpc:call(Node, mnesia, delete_schema, [[Node]]),
    ok.

db_nodes() ->
    mnesia:system_info(db_nodes) ++ mnesia:system_info(extra_db_nodes).

-ifndef(__CCLIB_DB_HRL__).
-define(__CCLIB_DB_HRL__, true).

-include("cclib.hrl").

-define(CREATE_TABLE(Tables, NodeList, Table, Type),
    case lists:member(Table, Tables) of
        false ->
            ?INFO({create_table, Table, Type, NodeList}),
            case mnesia:create_table(Table, [
                    {attributes, record_info(fields, Table)},
                    {type, Type},
                    {disc_copies, NodeList}]) of
                {atomic, ok} ->
                    ok;
                {aborted, {already_exists, Table}} ->
                    ok
            end;
        true ->
            ok
    end).

-define(CREATE_RAM_TABLE(Tables, NodeList, Table, Type),
    case lists:member(Table, Tables) of
        false ->
            ?INFO({create_table, Table, Type, NodeList}),
            case mnesia:create_table(Table, [
                    {attributes, record_info(fields, Table)},
                    {type, Type},
                    {ram_copies, NodeList}]) of
                {atomic, ok} ->
                    ok;
                {aborted, {already_exists, Table}} ->
                    ok
            end;
        true ->
            ok
    end).

-endif.

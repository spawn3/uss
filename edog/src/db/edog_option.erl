-module(edog_option).
-compile(export_all).

-include("edog_common.hrl").

option_from_env(Key) ->
    case application:get_env(?APPLICATION, Key) of
        {ok, Value} ->
            option_set(Key, Value)
    end.

option_init() ->
    ?INFO(option_init),
    %% option_t
    option_set(edog_start_flag, max_memory),
    set_load_policy(#load_policy{}),

    option_from_env(ws_ip),
    option_from_env(ws_port),
    option_from_env(disk_format),
    ok.

option_set(Key, Value) ->
    Row = #option_t{key=Key, value=Value},
    F = fun() -> mnesia:write(Row) end,
    case mnesia:transaction(F) of
        {atomic, _} -> ok;
        {aborted, _Reason} ->
            {error, _Reason}
    end.

option_get(Key) ->
    F = fun() -> mnesia:read({option_t, Key}) end,
    case mnesia:transaction(F) of
        {atomic, [#option_t{value=Value}]} ->
            Value;
        {atomic, []} ->
            option_init(),
            option_get2(Key)
    end.

option_get2(Key) ->
    F = fun() -> mnesia:read({option_t, Key}) end,
    case mnesia:transaction(F) of
        {atomic, [#option_t{value=Value}]} ->
            Value
    end.

option_get(Key, Default) ->
    F = fun() -> mnesia:read({option_t, Key}) end,
    case mnesia:transaction(F) of
        {atomic, [#option_t{value=Value}]} -> Value;
        _ -> Default
    end.

option_del(Key) ->
    cclib_mnesia:delete(option_t, Key).

set_load_policy(#load_policy{on=On, timeout=Timeout, flag=Flag, min=Min, diff=Diff}) ->
    option_set(edog_load_on, On),
    option_set(edog_load_timeout, Timeout),
    option_set(edog_load_flag, Flag),
    option_set(edog_load_min,  Min),
    option_set(edog_load_diff, Diff),
    ok.

get_load_policy() ->
    #load_policy{
        on      = option_get(edog_load_on, false),
        timeout = option_get(edog_load_timeout),
        flag    = option_get(edog_load_flag),
        min     = option_get(edog_load_min),
        diff    = option_get(edog_load_diff)
    }.

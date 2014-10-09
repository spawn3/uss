-module(uss_yfs_conf).
-compile(export_all).

-include("uss_common.hrl").

get_value(File, Key) ->
    case file:read_file(File) of
        {ok, Bin} ->
            L = string:tokens(binary:bin_to_list(Bin), "\n"),
            find_key(Key, L);
        {error, Reason} ->
            {error, Reason}
    end.

find_key(_Key, []) ->
    {error, noentry};
find_key(Key, [H|T]) ->
    case string:tokens(H, " \t;") of
        [Key, Value|_] ->
            {ok, Value};
        _ ->
            find_key(Key, T)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_conf(Src) ->
    case file:consult(Src) of
        {ok, L} ->
            {ok, parse(L, [])};
        {error, Reason} ->
            {error, Reason}
    end.

parse([], Acc) ->
    lists:flatten(lists:reverse(Acc));
parse([{Kind, L}|T], Acc) when is_atom(Kind) ->
    Result = [parse_item(Kind, X) || X <- L],
    parse(T, [Result|Acc]).

parse_item(Kind, {Key, Value}) ->
    #conf_item{
        catagory = Kind,
        key      = Key,
        value    = value_to_string(Value)
    }.

value_to_string([H|_T] = Value) when is_tuple(H) ->
    L = [string:join(tuple_to_list(X), " ") || X <- Value],
    string:join(L, "; ");
value_to_string(Value) ->
    Value.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_yfs_conf_file(Src, Dst) ->
    case file:consult(Src) of
        {ok, L} ->
            S = gen(L, ""),
            filelib:ensure_dir(Dst),
            file:write_file(Dst, S);
        {error, Reason} ->
            {error, Reason}
    end.

gen([], Acc) ->
    lists:flatten(Acc);
gen([{global, L}|T], Acc) ->
    NewAcc = io_lib:format("~s#global~n~s~n~n", [Acc, lists:foldl(fun gen_items/2, "", L)]),
    gen(T, NewAcc);
gen([{Kind, L}|T], Acc) when is_atom(Kind) ->
    NewAcc = io_lib:format("~s~w {~n~s}~n~n~n", [Acc, Kind, lists:foldl(fun gen_items/2, "", L)]),
    gen(T, NewAcc).

gen_items(X, Acc) ->
    io_lib:format("~s~s", [Acc, gen_item(X)]).

gen_item({Key, Value}) when is_integer(Value) ->
    io_lib:format("\t~-40w ~w;~n", [Key, Value]);
gen_item({Key, Value}) when is_atom(Value) ->
    io_lib:format("\t~-40w ~w;~n", [Key, Value]);
gen_item({Key, Value}) when is_list(Value) ->
    gen_list({Key, Value}).

gen_list({Key, Value}) when is_tuple(hd(Value)) ->
    io_lib:format(
        "\t~w {~n"
        "~s"
        "\t}~n", [Key, lists:foldl(fun gen_tuples/2, "", Value)]);
gen_list({Key, Value}) ->
    io_lib:format("\t~-40w ~s;~n", [Key, Value]).

gen_tuples(X, Acc) ->
    io_lib:format("~s~s", [Acc, gen_tuple(X)]).

gen_tuple(X) ->
    io_lib:format("\t\t~s;~n", [string:join(tuple_to_list(X), " ")]).

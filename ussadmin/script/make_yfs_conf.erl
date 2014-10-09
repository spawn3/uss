#!/usr/bin/env escript
%% -*- erlang -*-

-define(ROOT,  "/sysy/yfs").

-define(PRINT(X), io:format("[~p] ~p~n", [?LINE, X])).

main(Args) ->
    Templ   = lists:nth(1, Args),
    YfsConf = lists:nth(2, Args),
    YfsBack = YfsConf ++ ".2",

    copy_file(YfsConf, YfsBack),
    make_yfs_conf(Templ, YfsConf).

copy_file(Src, Dst) ->
    ?PRINT({copy_file, Src, Dst}),
    case file:read_file(Src) of
        {ok, B} ->
            write_file(Dst, B);
        {error, Reason} ->
            ?PRINT({error, Reason}),
            {error, Reason}
    end.

write_file(File, Text) ->
    ?PRINT({write_file, File}),
    case file:write_file(File, Text) of
        ok ->
            ok;
        {error, Reason} ->
            ?PRINT({error, Reason}),
            {error, Reason}
    end.

make_yfs_conf(Src, Dst) ->
    ?PRINT({make_yfs_conf, Src, Dst}),
    case file:consult(Src) of
        {ok, L} ->
            S = gen(L, ""),
            filelib:ensure_dir(Dst),
            write_file(Dst, S);
        {error, Reason} ->
            {error, Reason}
    end.

gen([], Acc) ->
    lists:flatten(Acc);
gen([{global, L}|T], Acc) ->
    F = fun(X, Y) -> gen_items(X, Y) end,
    NewAcc = io_lib:format("~s#global~n~s~n~n", [Acc, lists:foldl(F, "", L)]),
    gen(T, NewAcc);
gen([{Kind, L}|T], Acc) when is_atom(Kind) ->
    F = fun(X, Y) -> gen_items(X, Y) end,
    NewAcc = io_lib:format("~s~w {~n~s}~n~n~n", [Acc, Kind, lists:foldl(F, "", L)]),
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
    F = fun(X, Y) -> gen_tuples(X, Y) end,
    io_lib:format(
        "\t~w {~n"
        "~s"
        "\t}~n", [Key, lists:foldl(F, "", Value)]);
gen_list({Key, Value}) ->
    io_lib:format("\t~-40w ~s;~n", [Key, Value]).

gen_tuples(X, Acc) ->
    io_lib:format("~s~s", [Acc, gen_tuple(X)]).

gen_tuple(X) ->
    io_lib:format("\t\t~s;~n", [string:join(tuple_to_list(X), " ")]).

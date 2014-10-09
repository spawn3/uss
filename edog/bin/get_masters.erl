#!/usr/bin/env escript
%% -*- erlang -*-

-define(APPLICATION_ROOT,  "/sysy/yfs/ussadmin").

main(_Args) ->
    File = filename:join([?APPLICATION_ROOT, "edog_runtime/conf/agent.config"]),
    try
        Masters = get(File, kernel, edog_masters),
        F = fun(X) -> io:format("~s ", [X]) end,
        lists:foreach(F, Masters),
        io:format("~n")
    catch
        _:_ ->
            ""
    end.

get(File, Application, Item) ->
    {ok, [L]} = file:consult(File),
    AppConfig = proplists:get_value(Application, L),
    Masters = proplists:get_value(Item, AppConfig),
    Masters.

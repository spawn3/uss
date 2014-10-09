#!/usr/bin/env escript
%% -*- erlang -*-

-define(NAME, cclib_xref).

-define(PRINT(X), io:format("~p~n", [lists:flatten(X)])).

deps_conf() ->
    {ok, Cwd} = file:get_cwd(),
    Script = filename:join([Cwd, ?FILE]),
    File = filename:join([filename:dirname(Script), "../deps/deps.conf"]),
    ?PRINT(File),
    File.

main(_Args) ->
    DepsConf = deps_conf(),
    PathPrefix = filename:dirname(DepsConf),
    {ok, Deps} = file:consult(DepsConf),
    lists:foreach(
        fun(Dep) -> handle_dep_line(Dep, PathPrefix) end,
        Deps).

handle_dep_line({App, Deps}, PathPrefix) ->
    ?PRINT("================================================================================================"),
    ?PRINT(io_lib:format("== ~p: deps ~p path ~p", [App, Deps, PathPrefix])),
    ?PRINT("================================================================================================"),
    xref:start(?NAME),
    xref:set_default(?NAME, [{warnings,true}]),
    DepsDir = [filename:join([PathPrefix, X, "ebin"])||X <- Deps],
    AppDir = filename:join([PathPrefix, App]),

    Opts = [],

    xref:set_library_path(?NAME, DepsDir ++ code:get_path()),
    xref:add_application(?NAME, AppDir, []),
    F = fun(Analysis) ->
        Res = xref:analyze(?NAME, Analysis, Opts),
        io:format("===============> ~p~n", [Analysis]),
        io:format("~p~n", [Res])
    end,
    lists:foreach(F, [undefined_function_calls, deprecated_function_calls]),
    xref:stop(?NAME).

#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable

main(_Args) ->
    Cwd = script_dir(),
    TempDir = filename:join([Cwd, "../priv/templates"]),
    OutDir = filename:join([Cwd, "../ebin"]),
    {ok, Files} = file:list_dir(TempDir),

    Opts = [{out_dir, OutDir}, {compiler_options, [debug_info]}],
    lists:foreach(
        fun(File) -> compile_file(filename:join([TempDir, File]), Opts) end,
        Files).

script_dir() ->
    {ok, Dir} = file:get_cwd(),
    filename:dirname(filename:join([Dir, ?FILE])).

compile_file(File, Opts) ->
    Basename = filename:basename(File, ".dtl"),
    Mod = list_to_atom(Basename ++ "_view"),
    io:format("=> compile ~p to ~p~n", [File, Mod]),
    erlydtl:compile(File, Mod, Opts).

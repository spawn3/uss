-module(cclib_file).
-compile(export_all).

-include("cclib.hrl").

file_exist(File) ->
    case filelib:is_regular(File) of
        true ->
            true;
        false ->
            false
    end.

unconsult(File, L) ->
    {ok, S} = file:open(File, [write]),
    lists:foreach(fun(X) -> io:format(S, "~p.~n", [X]) end, L),
    file:close(S).

pwrite(File, Off, Buf) ->
    {ok, S} = file:open(File, [read,write,raw,binary]),
    ok = file:pwrite(S, Off, Buf),
    file:close(S).

-spec pread(string(), integer(), integer()) -> {ok, binary()} | {error, eof}.

pread(File, Off, Count) ->
    {ok, S} = file:open(File, [read,binary,raw]),
    case file:pread(S, Off, Count) of
        {ok, Buf} ->
            file:close(S),
            {ok, Buf};
        _Other ->
            file:close(S),
            {error, eof}
    end.

truncate(File) ->
    case filelib:is_regular(File) of
        true ->
            case file:open(File, [write]) of
                {ok, Fd} ->
                    file:truncate(Fd),
                    file:close(Fd);
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            ok
    end.

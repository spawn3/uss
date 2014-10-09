-module(cclib_cmd).
-export([run/1, run/2,
        exec/1, exec/2,
        exec_debug/1, exec_debug/2,
        test/0]).

-include("cclib.hrl").

-define(EXIT_1, 1).

-define(CMD_TIMEO, 10000).

run(Cmd) ->
	run(Cmd, ?CMD_TIMEO).

% milliseconds
run(Cmd, Timeout) ->
    run(Cmd, Timeout, [wrapper]).

run(Cmd, Timeout, Options) ->
    F = fun() ->
        process_flag(trap_exit, true),
        Cmd2 =
        case proplists:get_bool(wrapper, Options) of
            true ->
                lists:concat(["bash ", filename:dirname(code:which(?MODULE)), "/../priv/wrapper.sh ", Cmd]);
            false ->
                Cmd
        end,
        Port = erlang:open_port({spawn, lists:flatten(Cmd2)},
            [exit_status, stderr_to_stdout]),
        Res = loop(Port,[], Timeout, Options),
        exit({self(), Res})
    end,
    {Pid, Mref} = spawn_monitor(F),
    monitor_loop(Pid, Mref, Options).

%%
exec(Cmd) ->
    exec(Cmd, ?CMD_TIMEO).

exec(Cmd, Timeout) ->
    Info = #cmd_info{timeout=Timeout},
    case cclib_cmd:run(Cmd, Timeout) of
        {0, Data} ->
            {ok, Info#cmd_info{data=Data}};
        {ExitCode, {timeout, Data}} ->
            {error, Info#cmd_info{code=ExitCode, data=Data, is_timeout=true}};
        {ExitCode, Data} ->
            {error, Info#cmd_info{code=ExitCode, data=Data}}
    end.

exec_debug(Cmd) ->
    exec_debug(Cmd, ?CMD_TIMEO).

exec_debug(Cmd, Timeout) ->
    ?INFO({cmd, iolist_to_binary(Cmd), Timeout}),
    exec(Cmd, Timeout).

loop(Port, Data, Timeout, Options) ->
	receive
		{Port, {data, NewData}} ->
            loop(Port, [NewData|Data], Timeout, Options);
		{Port, {exit_status, S}} ->
            case proplists:get_bool(wrapper, Options) of
                true  -> {S, tl(lists:reverse(Data))};
                false -> {S, lists:reverse(Data)}
            end;
        {'EXIT', Port, Reason} ->
            {?EXIT_1, Reason}
	after Timeout ->
        case proplists:get_bool(wrapper, Options) of
            true  ->
                case lists:reverse(Data) of
                    [] -> ok;
                    [H|_] ->
                        case string:tokens(H, ":") of
                            ["mypid", ExternalPid|_] ->
                                os:cmd(["kill -9 ", ExternalPid]);
                            _ ->
                                ok
                        end
                end,
                {?EXIT_1, {timeout, tl(lists:reverse(Data))}};
            false ->
                {?EXIT_1, {timeout, lists:reverse(Data)}}
        end
	end.

monitor_loop(Pid, Mref, Options) ->
    receive
        {'DOWN', Mref, _, _, {Pid, {ExitStatus, Result}}} ->
            %?INFO(Result),
            {ExitStatus, map(Result)};
        _Msg ->
            monitor_loop(Pid, Mref, Options)
    end.

map({timeout, Result}) ->
    {timeout, line_by_line(Result)};
map(Result) ->
    line_by_line(Result).

line_by_line([]) -> [];
line_by_line(L) ->
    string:tokens(string:join(L, "\n"), "\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
	shouldReturnCommandResult(),
	shouldThrowAfterTimeout(),
	shouldThrowIfCmdFailed(),
	{ok, "Tests PASSED"}.

shouldReturnCommandResult() ->
    {0, ["Hello"]} = run("echo Hello").

shouldThrowAfterTimeout()->
    {?EXIT_1, {timeout, _}} = (catch run("sleep 10", 20)).

shouldThrowIfCmdFailed()->
    {_S1, _} = (catch run("wrongcommand")),
    {_S2, _} = (catch run("ls nonexistingfile")).

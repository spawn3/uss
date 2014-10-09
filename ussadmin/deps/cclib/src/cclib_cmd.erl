-module(cclib_cmd).
-export([run/1, run/2, test/0]).

-include("cclib.hrl").

-define(EXIT_CODE_1, 1).

run(Cmd) ->
	run(Cmd, 5000).

% milliseconds
run(Cmd, Timeout) ->
    process_flag(trap_exit, true),
    Port = erlang:open_port({spawn, Cmd}, [exit_status, stderr_to_stdout]),
    loop(Port,[], Timeout).

loop(Port, Data, Timeout) ->
	receive
		{Port, {data, NewData}} ->
            loop(Port, [NewData|Data], Timeout);
		{Port, {exit_status, S}} ->
            {S, lists:reverse(Data)};
        {'EXIT', _Port, Reason} ->
            {?EXIT_CODE_1, Reason}
	after Timeout ->
        {?EXIT_CODE_1, timeout}
	end.

test() ->
	shouldReturnCommandResult(),
	shouldThrowAfterTimeout(),
	shouldThrowIfCmdFailed(),
	{ok, "Tests PASSED"}.

shouldReturnCommandResult() ->
    {0, ["Hello\n"]} = run("echo Hello").

shouldThrowAfterTimeout()->
	timeout = (catch run("sleep 10", 20)).

shouldThrowIfCmdFailed()->
    {_S1, _} = (catch run("wrongcommand")),
    {_S2, _} = (catch run("ls nonexistingfile")).

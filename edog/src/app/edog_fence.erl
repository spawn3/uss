-module(edog_fence).
-compile(export_all).

-include("edog_common.hrl").

% FENCE
fence_ring() ->
    edog_cluster:fence_ring().

fence_check(L) ->
    Main = self(),
    lists:foreach(
        fun(Ip) ->
            F = fun() -> Main ! {self(), Ip, cclib_node:ping(Ip)} end,
            spawn(F)
        end, L),
    wait_here(length(L), []).

wait_here(0, Acc) -> Acc;
wait_here(N, Acc) ->
    receive
        {From, Ip, Res} when is_pid(From) ->
            wait_here(N-1, [{Ip, Res}|Acc]);
        _ ->
            wait_here(N, Acc)
    end.

fence_check() ->
    fence_check(fence_ring()).

is_fence_passed2(Hosts) ->
    L = fence_check(Hosts),
    Passed = [Ip || {Ip, true} <- L],
    if
        length(Passed) > length(L) div 2 ->
            true;
        true ->
            ?WARN({fence_test, Hosts, L}),
            false
    end.

-define(FENCE_TEST_TIMEO, 30000).
-define(FENCE_TEST_FAIL,  "fence_test fail").

is_fence_passed(Hosts) ->
    ?INFO({nodes, [node()|nodes()]}),
    Cmd = io_lib:format("~s/script/fence_test.py ~s", [?APP_SRC, string:join(Hosts, " ")]),
    case cclib_cmd:exec(Cmd, ?FENCE_TEST_TIMEO) of
        {ok, #cmd_info{}} ->
            true;
        {error, Info} ->
            ?WARN({fence_test, lists:flatten(Cmd), Info}),
            false
    end.

is_fence_passed() ->
    ?PROFILE_BEGIN(),
    Res = is_fence_passed(fence_ring()),
    ?PROFILE_END(fence_check),
    Res.

do_after(Info) ->
    ?ERROR({fence_test, Info}),
    cclib_app:write_exit_info(?APPLICATION, ?FENCE_TEST_FAIL),
    edog_app:stop_specific(),
    ?INIT_STOP().

is_fence_failed(App) ->
    Cmd = io_lib:format("~s/script/is_fence_failed.sh ~s",
        [?APP_SRC, cclib_app:exit_info_file(App)]),
    case cclib_cmd:exec(Cmd, ?FENCE_TEST_TIMEO) of
        {0, _} -> true;
        _ -> false
    end.

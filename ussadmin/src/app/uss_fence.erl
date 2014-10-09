-module(uss_fence).
-compile(export_all).

-include("uss_common.hrl").

% FENCE
fence_ring() ->
    {ok, Masters} = clib_utils:get_managers(),
    Ips = [cclib_utils:to_list(cclib_utils:get_ip(X)) || X <- Masters],
    Agents = [cclib_utils:to_list(X) || X <- uss_mnesia:all_nodes_in_cluster()],
    lists:usort(Ips ++ Agents).

fence_test(L) ->
    Main = self(),
    lists:foreach(
        fun(Ip) ->
            F = fun() -> Main ! {self(), Ip, cclib_utils:ping(Ip)} end,
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

fence_test() ->
    fence_test(fence_ring()).

is_fence_passed(Hosts) ->
    L = fence_test(Hosts),
    ?TTY_WARN_REPORT({fence_test, Hosts, L}),
    Passed = [Ip || {Ip, true} <- L],
    if
        length(Passed) > length(L) div 2 ->
            true;
        true ->
            false
    end.

is_fence_passed() ->
    is_fence_passed(fence_ring()).

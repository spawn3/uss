-module(cclib_node).
-compile(export_all).

-include("cclib.hrl").

get_ip(Node) ->
    L = string:tokens(atom_to_list(Node), "@"),
    lists:nth(2, L).

% PING
node_ping(Node, N) ->
    node_ping(Node, N, 4).

node_ping(_Node, N, _Step) when N =< 0 ->
    false;
node_ping(Node, N, Step)  when is_atom(Node) ->
    ?INFO({ping, Node, N, Step}),
    case timer:tc(net_adm, ping, [Node]) of
        {_T, pong} ->
            true;
        {T, pang} ->
            T1 = T div 1000000,
            if
                T1 >= Step ->
                    node_ping(Node, N-T1, Step);
                true ->
                    timer:sleep((Step-T1) * 1000),
                    node_ping(Node, N-Step, Step)
            end
    end.

wait_nodes(Nodes, Timeout) ->
    F = fun(Node) ->
        case node_ping(Node, Timeout, 1) of
            true ->
                true;
            false ->
                throw({timeout, {Node, Timeout}})
        end
    end,
    lists:all(F, Nodes).

ping(Node) when is_atom(Node) ->
    ping(get_ip(Node));
ping(Ip) when is_list(Ip) ->
    Result = os:cmd(["ping -c 5 -i 0.2 -w 5 ", Ip]),
    try
        L = string:tokens(Result, "\n"),
        case [X || X <- L, cclib_utils:check_cmd_result(X, ["packets transmitted"])] of
            [S] ->
                L2 = string:tokens(S, ", "),
                Send = list_to_integer(lists:nth(1, L2)),
                Recv = list_to_integer(lists:nth(4, L2)),
                if
                    Send =:= 5, Recv > 0 -> true;
                    true                 -> false
                end;
            _ ->
                ?WARN({ping, Ip, Result}),
                false
        end
    catch
        Error:Exception ->
            ?ERROR({Error, Exception, Result, erlang:get_stacktrace()}),
            false
    end.

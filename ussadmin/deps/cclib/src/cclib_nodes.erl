-module(cclib_nodes).
-compile(export_all).

-include("cclib.hrl").

node_ping(Node, N) ->
    node_ping(Node, N, 6).

node_ping(_Node, N, _Step) when N =< 0 ->
    false;
node_ping(Node, N, Step) when is_atom(Node) ->
    case net_adm:ping(Node) of
        pong ->
            true;
        pang ->
            ?INFO_REPORT({ping, N}),
            timer:sleep(Step * 1000),
            node_ping(Node, N-Step, Step)
    end.

-define(WAIT_MASTERS_TIMEOUT, 30).

wait_nodes(Nodes) ->
    wait_nodes(Nodes, ?WAIT_MASTERS_TIMEOUT).

wait_nodes(Nodes, Timeout) ->
    F = fun(Node) ->
        case node_ping(Node, Timeout, 1) of
            true ->
                true;
            false ->
                throw({timeout, {Nodes, Timeout}})
        end
    end,
    lists:all(F, Nodes).

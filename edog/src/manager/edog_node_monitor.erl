-module(edog_node_monitor).
-compile(export_all).

-include("edog_common.hrl").

% async
do_nodedown(Node, InfoList) when is_atom(Node) ->
    ?PROFILE_BEGIN(),
    Res = do_nodedown2(Node, InfoList),
    ?PROFILE_END(handle_nodedown),
    Res.

do_nodedown2(Node, InfoList) when is_atom(Node) ->
    PmID = cclib_node:get_ip(Node),
    ?INFO({vms, edog_mnesia:select_vm(PmID)}),
    case cclib_node:ping(Node) of
        true ->
            % TODO WHEN poweroff
            ?WARN({nodedown, Node, "can ping, please ensure agent started on it"}),
            edog_notify:notify(#notify_spec{op=pm_down, key=PmID, reply={ok, agent_not_started}}),

            case is_poweroff(PmID) of
                true ->
                    ?WARN({poweroff, PmID}),
                    do_nodedown_atonce(Node);
                false ->
                    ok
            end;
        false ->
            NodedownReason = proplists:get_value(nodedown_reason, InfoList),
            edog_notify:notify(#notify_spec{op=pm_down, key=PmID, reply={ok, NodedownReason}}),

            Timeout = edog_conf:manager_update_db_factor() * net_kernel:get_net_ticktime(),
            Timeout2 =
            case NodedownReason of
                ping -> Timeout - net_kernel:get_net_ticktime();
                _    -> Timeout
            end,
            ?WARN({nodedown, Node, InfoList, Timeout2}),

            % TODO
            % case 1: network down
            %         connected_closed
            %         net_tick_timeout
            % case 2: physical machine down
            case cclib_node:node_ping(Node, Timeout2) of
                true ->
                    ok;
                false ->
                    do_nodedown_atonce(Node)
            end
    end.

is_poweroff(Ip) ->
    is_poweroff(Ip, 30).

is_poweroff(_Ip, Count) when Count =< 0 ->
    false;
is_poweroff(Ip, Count) ->
    case cclib_node:ping(Ip) of
        true ->
            %?INFO({poweroff, Ip, Count}),
            timer:sleep(1000),
            is_poweroff(Ip, Count-1);
        false ->
            true
    end.

do_nodedown_atonce(Node) when is_atom(Node) ->
    PmID = cclib_node:get_ip(Node),
    case edog_mnesia:select_vm(PmID) of
        {ok, Vms} ->
            % TODO
            ?ERROR({nodedown, PmID, Vms}),
            lists:foreach(fun(Vm) -> edog_vm:vm_check(Vm, Node, ?VM_STATE_SHUTOFF) end, Vms);
        {error, _Reason} ->
            {error, _Reason}
    end.

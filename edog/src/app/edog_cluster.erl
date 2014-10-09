-module(edog_cluster).
-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").
-include("edog_common.hrl").

-record(r_cluster, {
    pms,
    yfs
}).

get_manager_nodes() ->
    case application:get_env(kernel, edog_masters) of
        {ok, Nodes} ->
            Nodes
    end.

get_managers() ->
    Managers = get_manager_nodes(),
    [cclib_node:get_ip(X) || X <- Managers].

get_agents() ->
    Cfg = filename:join([?APP_DATA, "conf/cluster.xml"]),
    try
        {E, _R} = xmerl_scan:file(Cfg),
        get_agents(E)
    catch
        Error:Exception ->
            ?ERROR({Error, Exception, Cfg, erlang:get_stacktrace()}),
            []
    end.

get_agents(#xmlElement{} = E) ->
    Agents = xmerl_xpath:string("/cluster/slave",  E),
    F = fun(X) ->
        {ok, Ip} = get_attr(X, ip),
        Ip
    end,
    lists:usort([F(X) || X <- Agents]).

-spec node_rule(atom()) -> manager | agent | other.
node_rule(Node) when is_atom(Node) ->
    try edog_common:get_pair(Node) of
        {?MASTERNAME, _Ip} ->
            manager;
        {?AGENTNAME, _Ip} ->
            agent;
        _ ->
            other
    catch
        Error:Exception ->
            ?ERROR({Error, Exception, erlang:get_stacktrace()}),
            other
    end.

is_manager_alive() ->
    case global:whereis_name(edog_master) of
        Pid when is_pid(Pid) ->
            true;
        _ ->
            false
    end.

is_valid_agent(Node) when is_atom(Node) ->
    try edog_common:get_pair(Node) of
        {?AGENTNAME, Ip} ->
            is_valid_agent(Ip)
    catch
        Error:Exception ->
            ?ERROR({Error, Exception, erlang:get_stacktrace()}),
            false
    end;
is_valid_agent(Ip) when is_list(Ip) ->
    L = get_agents(),
    lists:member(Ip, L).

is_valid_node(Node) when is_atom(Node) ->
    try node_rule(Node) of
        manager ->
            is_valid_node(cclib_node:get_ip(Node));
        agent ->
            is_valid_node(cclib_node:get_ip(Node));
        _ ->
            false
    catch
        Error:Exception ->
            ?ERROR({Error, Exception, erlang:get_stacktrace()}),
            false
    end;
is_valid_node(Ip) when is_list(Ip) ->
    L = fence_ring(),
    lists:member(Ip, L).

% FENCE
fence_ring() ->
    L = get_agents() ++ get_managers(),
    lists:usort(L).

-define(WAIT_MASTERS_TIMEOUT, 30).

wait_managers() ->
    Managers = get_manager_nodes(),
    cclib_node:wait_nodes(Managers, ?WAIT_MASTERS_TIMEOUT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
    Cfg = filename:join([code:lib_dir(edog), "../edog_runtime/conf/cluster.xml"]),
    try xmerl_scan:file(Cfg) of
        {E, _R} ->
            Pms = get_servers(E, true),
            Yfs = get_yfs_servers(E, false),
            Yfs1 = lists:map(
                fun(#xmlElement{name=Name, attributes=Attrs}) ->
                        IP = edog_xml:get_attr(Attrs, ip),
                        case Name of
                            mds -> {Name, IP, []};
                            smds -> {Name, IP, []};
                            cds ->
                                Num = edog_xml:get_attr(Attrs, num),
                                case string:tokens(Num, ":") of
                                    [Start, End|_] ->
                                        {Name, IP, lists:seq(
                                                list_to_integer(Start),
                                                list_to_integer(End))};
                                    [Start] ->
                                        {Name, IP, [list_to_integer(Start)]}
                                end;
                            proxy -> {Name, IP, []};
                            ynfs -> {Name, IP, []}
                        end
                end, Yfs),
            {ok, #r_cluster{pms=Pms, yfs=Yfs1}}
    catch
        _Error:_Expr ->
            {error, {_Error, _Expr}}
    end.

get_servers(#xmlElement{name=cluster} = E, IpOnly) ->
    Servers = xmerl_xpath:string("/cluster/edog/*",  E),
    case IpOnly of
        true ->
            lists:usort([edog_xml:get_attr(X#xmlElement.attributes, ip) || X <- Servers]);
        false ->
            Servers
    end.

get_yfs_servers(#xmlElement{name=cluster} = E, IpOnly) ->
    Servers = xmerl_xpath:string("/cluster/yfs/*",  E),
    case IpOnly of
        true ->
            lists:usort([edog_xml:get_attr(X#xmlElement.attributes, ip) || X <- Servers]);
        false ->
            Servers
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_attr(#xmlElement{} = E, Attr) ->
    Attrs = E#xmlElement.attributes,
    case lists:keyfind(Attr, 2, Attrs) of
        #xmlAttribute{value=Val} ->
            {ok, Val};
        false ->
            false
    end.

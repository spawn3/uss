%% ------------------------------------------------------------
%% @author Dongguanjun <dongguanjun@meidisen.com>
%% @copyright 2010 Meidisen Co.,Ltd
%% @doc 网络虚拟化层
%% @end
%% ------------------------------------------------------------
-module(cclib_network).
-export([
        get_subnet/1,
        atoi/1,
        get_ifs/1,
        if_info/2,
        ethtool/1,
        info/0,
        info/1,
        info_bridge/0,
        netflow/0
    ]).

-include("cclib_network.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
info_bridge() ->
    L = get_ifs(bridge),
    [bridge_info(X) || X <- L].

bridge_info(Bridge) ->
    [S1, S2|_] = string:tokens(os:cmd("ifconfig " ++ Bridge), "\n"),
    L1 = string:tokens(S1, " \t"),
    L2 = string:tokens(S2, " \t:"),
    {Bridge, lists:nth(5, L1), lists:nth(3, L2), lists:nth(7, L2)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
info() ->
    info([{type, ethernet}]).

info(Options) ->
    Type = proplists:get_value(type, Options, ethernet),
    L = get_ifs(Type),
    [Y || Y <- [if_info({Type, X}, Options) || X <- L], is_record(Y, if_info)].

get_ifs(ovs) ->
    string:tokens(os:cmd("ovs-vsctl list-br"), "\n");
get_ifs(ethernet) ->
    F = fun(Line) ->
        [H|_] = string:tokens(Line, " \t"),
        H
    end,
    L = string:tokens(os:cmd("ifconfig|grep HWaddr"), "\n"),
    [F(Line) || Line <- L];
get_ifs(bridge) ->
    [_|L] = string:tokens(os:cmd("brctl show|grep br"), "\n"),
    [hd(string:tokens(Row, " \t")) || Row <- L, length(string:tokens(Row, " \t")) >= 3].

if_info({ovs, Bridge}, _Options) ->
    {Bridge, "55:00:11:22:33:44", "192.168.1.20", "255.255.255.0"};
if_info({ethernet, Eth}, Options) ->
    if_info({bridge, Eth}, Options);
if_info({bridge, Bridge}, Options) ->
    % HWaddr
    case string:tokens(os:cmd("ifconfig " ++ Bridge ++ " |grep addr:.*Bcast:.*Mask:"), "\n") of
        [] ->
            false;
        [Line2] ->
            [_, "addr:" ++ Addr, "Bcast:" ++ Bcast, "Mask:" ++ Mask] = string:tokens(Line2, " \t"),

            RefNetwork = proplists:get_value(network, Options),
            RefNetmask = proplists:get_value(netmask, Options),

            case is_same_subnet({RefNetwork, RefNetmask}, {Addr, Mask}) of
                true ->
                    [Line1] = string:tokens(os:cmd("ifconfig " ++ Bridge ++ " |grep HWaddr"), "\n"),
                    [Bridge, _, _, _, Mac] = string:tokens(Line1, " \t"),
                    case string:tokens(os:cmd("ifconfig " ++ Bridge ++ " |grep \"RX bytes\""), "\n") of
                        [LineX] ->
                            L = string:tokens(LineX, " :\t"),
                            #if_info{
                                ifname=Bridge,
                                mac=Mac,
                                ip=Addr,
                                bcast=Bcast,
                                mask=Mask,
                                rx_bytes=list_to_integer(lists:nth(3, L)),
                                tx_bytes=list_to_integer(lists:nth(8, L)),
                                speed=ethtool(Bridge)
                            };
                        _ ->
                            false
                    end;
                false ->
                    false
            end
    end.

% Need root privileges
-ifdef(use_limit).
ethtool(Dev) -> undefined.
-else.
ethtool(Dev) ->
    L = string:tokens(os:cmd("ethtool " ++ Dev ++ " |grep Speed"), " :\t\n"),
    case L of
        ["Speed", Speed] -> Speed;
        _ -> "NA"
    end.
-endif.

netflow() ->
    Cmd = "cat /proc/net/dev",
    L = string:tokens(os:cmd(Cmd), "\n"),
    L1 = lists:nthtail(2, L),
    Netflow = [netflow(X) || X <- L1],
    F = fun(#netflow{nic=Nic, tx_bytes=Tx, rx_bytes=Rx}, {TotalRx, TotalTx}) ->
        case Nic of
            "eth" ++ _ -> {TotalRx + Rx, TotalTx + Tx};
            _ -> {TotalRx, TotalTx}
        end
    end,
    {TotalRx, TotalTx} = lists:foldl(F, {0, 0}, Netflow),
    {Netflow, TotalRx, TotalTx}.

netflow(Line) ->
    L = string:tokens(Line, ": \t"),
    case length(L) of
        17 ->
            #netflow{
                nic=lists:nth(1, L),
                tx_bytes=list_to_integer(lists:nth(2, L)),
                rx_bytes=list_to_integer(lists:nth(10, L))}
    end.
is_same_subnet({RefNetwork, RefNetmask}, {_Network, _Netmask}) when RefNetwork =:= undefined; RefNetmask =:= undefined ->
    true;
is_same_subnet({RefNetwork, RefNetmask}, {Network, Netmask}) ->
    get_subnet({RefNetwork, RefNetmask}) =:= get_subnet({Network, Netmask}).

get_subnet({Ip, Netmask}) ->
    atoi(Ip) band atoi(Netmask).

atoi(Ip) ->
    {ok, Addr} = inet_parse:address(Ip),
    address_to_integer(Addr).

address_to_integer({D1,D2,D3,D4}) ->
    (D1 bsl 24) + (D2 bsl 16) + (D3 bsl 8) + D4.

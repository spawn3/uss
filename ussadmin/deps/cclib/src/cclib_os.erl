-module(cclib_os).
-export([
        get_load/0,
        which/1,
        netflow/0,
        info/0
    ]).
-compile(export_all).

-include("cclib.hrl").

avg_to_float(Avg) ->
    list_to_float(string:strip(Avg)).

%% API
uptime() ->
    F = fun(Line) ->
        case string:tokens(Line, ",") of
            [Time, User, Avg, Avg5, Avg15] ->
                [Curr, TimeH] = string:tokens(Time, "up"),
                TimeT = " ",
                [UserCount, _] = string:tokens(User, " "),
                [_, Avg1] = string:tokens(Avg, ":");
            [Time, Time2, User, Avg, Avg5, Avg15] ->
                [Curr, TimeH] = string:tokens(Time, "up"),
                TimeT = string:strip(Time2),
                [UserCount, _] = string:tokens(User, " "),
                [_, Avg1] = string:tokens(Avg, ":")
        end,
        #uptime_info{
            currtime=string:strip(Curr),
            time=string:strip(TimeH ++ " " ++ TimeT),
            user=list_to_integer(UserCount),
            avg1=avg_to_float(Avg1),
            avg5=avg_to_float(Avg5),
            avg15=avg_to_float(Avg15)
        }
    end,
    [Line] = string:tokens(os:cmd("uptime"), "\n"),
    F(Line).

lists_count(L) ->
    lists_count(L, dict:new()).

lists_count([], Dict) ->
    L = dict:to_list(Dict),
    [Info#cpu_info{count=Count} || {Info,Count} <- L];
lists_count([H|T], Dict) ->
    case dict:find(H, Dict) of
        error ->
            lists_count(T, dict:store(H, 1, Dict));
        {ok, Value} ->
            lists_count(T, dict:store(H, Value+1, Dict))
    end.

%% --------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------
-spec get_load() -> #load_info{}.
get_load() ->
    Mem = memsup:get_system_memory_data(),
    {free_memory, Free} = lists:keyfind(free_memory, 1, Mem),
    {cached_memory, Cached} = lists:keyfind(cached_memory, 1, Mem),
    %% XXX
    #load_info{
        node=node(),
        time=cclib_utils:now_to_integer(),
        avg1=cpu_sup:avg1(),
        avg5=cpu_sup:avg5(),
        avg15=cpu_sup:avg15(),
        free= ?to_kb(Free+Cached)}.

info() ->
    info(all).

info(all) ->
    info([cpu, mem, disk, network, bridge]);
info(L) when is_list(L) ->
    [{X, info(X)} || X <- L];
info(util) ->
    L = cpu_sup:util([per_cpu]),
    F = fun(R) ->
            #cpu_util{
                id=element(1,R),
                busy=element(2,R),
                idle=element(3,R)
            }
    end,
    [F(R) || R <- L];
info(cpu) ->
    L = string:tokens(os:cmd("cat /proc/cpuinfo"), "\n"),
    F = fun(X) ->
            IL0 = string:tokens(X, ":"),
            list_to_tuple([string:strip(string:strip(Y), both, $\t) || Y <- IL0])
    end,
    L1 = [F(X) || X <- L],
    Total = length(L1),
    IdxL = [N || N <- lists:seq(1, Total),
        tuple_size(lists:nth(N, L1)) =:= 2,
        element(1, lists:nth(N, L1)) =:= "processor"],
    IdxL1 = lists:reverse([Total+1|lists:reverse(IdxL)]),
    %io:format("IDX ~p~n", [IdxL1]),

    F2 = fun(N) ->
            Len = lists:nth(N+1, IdxL1) - lists:nth(N, IdxL1),
            Sublist = lists:sublist(L1, lists:nth(N, IdxL1), Len),
            #cpu_info{
                physical_id=proplists:get_value("physical id", Sublist),
                model_name=string_compress(
                    proplists:get_value("model name", Sublist), " ")
            }
    end,
    L2 = [F2(X) || X <- lists:seq(1, length(IdxL1)-1)],
    lists_count(L2);
info(cpu_util) ->
    L = string:tokens(os:cmd("top -b -n 1|grep Cpu"), " ,\n"),
    F = fun(X) ->
            [V, K] = string:tokens(X, "%"),
            {K, V}
    end,
    [F(X) || X <- lists:nthtail(2, L)];
info(mem) ->
    L = memsup:get_system_memory_data(),
    Fields = [
        {system_total_memory, mem_total},
        {free_memory, mem_free},
        {cached_memory, mem_cached},
        {buffered_memory, mem_buffers},
        {total_swap, swap_total},
        {free_swap, swap_free}],
    F = fun(X, Y) ->
            {X, V} = lists:keyfind(X, 1, L),
            {Y, V}
    end,
    [F(X,Y) || {X,Y} <- Fields];
info(disk) ->
    disksup:get_disk_data();
info(network) ->
    cclib_network:info();
info(bridge) ->
    cclib_network:info_bridge();
info(pci) ->
    L = string:tokens(os:cmd("lspci|grep -i eth"), "\n"),
    L1 = lists:map(
        fun(X) ->
            [H|_] = string:tokens(X, " "),
            PciDir = string:strip(os:cmd("find /sys/devices/ -name '*" ++ H ++ "'"), right, $\n),
            case get_nic1(PciDir) of
                [] -> virtual;
                {Physfn, Virtfn} ->
                    {Physfn, Virtfn}
            end
        end, L),
    lists:filter(
        fun(X) ->
            case X of
                virtual -> false;
                _ -> true
            end
        end, L1).

string_compress(Str, Sep) ->
    string:join(string:tokens(Str, Sep), Sep).

%% --------------------------------------------------------------------
%%
check_prerequired() ->
    L = [
        "/sysy/yfs/app/bin/ylvm",
        "/sysy/yfs/app/bin/ymkdir",
        "/sysy/yfs/app/bin/ycp",
        "/sysy/yfs/app/sbin/c60d",
        "/sysy/yfs/app/sbin/yfs_mds",
        "/sysy/yfs/app/sbin/yfs_cds",
        "/sysy/yfs/app/sbin/proxy_server"
    ],
    case [X || X <- L, not cclib_utils:file_exist(X)] of
        [] -> {ok, yfs_ready};
        ErrL -> {error, {invalid_path, ErrL}}
    end.

which(Cmd) ->
    case os:cmd("which " ++ Cmd) of
        [] -> {error, Cmd};
        Other -> {ok, Other}
    end.

get_disk_data() ->
    L = string:tokens(os:cmd("df -l"), "\n"),
    L1 = [line_to_list(X) || X <- L],
    p_scan_disk_data(lists:nthtail(1,L1)).

-define(DF_NR, 6).

line_to_list(Line) -> string:tokens(Line, " \t").

list_to_disk_info(L) when length(L) =:= ?DF_NR ->
    #disk_info{
        device     = lists:nth(1, L),
        capacity   = p_list_to_integer(lists:nth(2, L)),
        used       = p_list_to_integer(lists:nth(3, L)),
        avail      = p_list_to_integer(lists:nth(4, L)),
        util       = p_list_to_integer(string:strip(lists:nth(5, L), both, $%)) / 100,
        mountpoint = lists:nth(6, L)
    }.

p_scan_disk_data(L) -> p_scan_disk_data(L, []).

p_scan_disk_data([], Acc) -> Acc;
p_scan_disk_data(L, Acc) ->
    N = p_feed_disk_data(L),
    L1 = lists:append(lists:sublist(L, N)),
    DiskInfo = list_to_disk_info(L1),
    p_scan_disk_data(lists:nthtail(N,L), [DiskInfo|Acc]).

p_feed_disk_data(L) -> p_feed_disk_data(L, {0, 0}).

p_feed_disk_data([], {_Len, N}) -> N;
p_feed_disk_data(_L, {?DF_NR, N}) -> N;
p_feed_disk_data([H|T], {Len, N}) ->
    p_feed_disk_data(T, {Len+length(H), N+1}).

p_list_to_integer(Str) ->
    list_to_integer(string:strip(Str)).

get_disk_data2() ->
    L = disksup:get_disk_data(),
    [#disk_info{mountpoint=Id, capacity=Kbyte, used=Used} || {Id, Kbyte, Used} <- L].

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

%% --------------------------------------------------------------------
%% useless
%% --------------------------------------------------------------------
is_link() ->
    L = string:tokens(os:cmd("mii-tool"), "\n"),
    lists:map(fun(X) -> io:format("~p~n", [X]) end, L).

is_match(S, RE) ->
    case re:run(S, RE) of
        {match, _} -> true;
        nomatch -> false
    end.

get_driver(S) ->
    case is_match(S, "82574L") of
        true -> e1000e;
        false ->
            case is_match(S, "82575") of
                true -> igb;
                false ->
                    case is_match(S, "82576") of
                        true -> igb;
                        false -> igbvf
                    end
            end
    end.

get_nic(PciDir) ->
    PhysfnDir = PciDir ++ "/physfn",
    {Type, Dir1} = case filelib:is_dir(PhysfnDir) of
        false -> {physical, PciDir};
        true  -> {virtual, PhysfnDir}
    end,
    NetDir = Dir1 ++ "/net",
    case filelib:is_dir(NetDir) of
        true ->
            io:format("~p~n", [NetDir]),
            {Type, get_mac(NetDir)};
        false ->
            io:format("~p~n", [Dir1]),
            {Type, []}
    end.

%% --------------------------------------------------------------------
get_nic1(PciDir) ->
    case filelib:is_dir(PciDir ++ "/physfn") of
        true ->  %% virtual
            [];
        false -> %% physical
            Physfn = get_nic3(PciDir),
            Virtfn =
            case filelib:wildcard("virtfn*", PciDir) of
                [] -> [];
                Virts when is_list(Virts) ->
                    get_nic2(PciDir, Virts)
            end,
            {Physfn, Virtfn}
    end.

get_nic2(PciDir, Virts) ->
    [ begin
            D = io_lib:format("~s/~s", [PciDir, X]),
            {ok, Link} = file:read_link(D),
            get_nic3(PciDir ++ "/" ++ Link)
        end || X <- Virts].

get_nic3(PciDir) ->
    NetDir = PciDir ++ "/net",
    case filelib:is_dir(NetDir) of
        true ->
            Pci = filename:basename(PciDir),
            {Eth, Mac} = get_mac(NetDir),
            {Pci, get_driver1(PciDir), Eth, Mac}
    end.

get_driver1(PciDir) ->
    {ok, S} = file:read_link(PciDir ++ "/driver"),
    filename:basename(S).

get_mac(NetDir) ->
    {ok, [Nic]} = file:list_dir(NetDir),
    AddressF = io_lib:format("~s/~s/address", [NetDir, Nic]),
    {ok, B} = file:read_file(AddressF),
    {Nic, string:strip(binary_to_list(B), right, $\n)}.

-module(edog_yfs_common).
-export([start/1, stop/1]).
-export([ps_info/1, info/2, cds_info/1]).

-include("edog_common.hrl").

start({Proc, IP, N}) ->
    Node = edog_common:get_node(IP),
    case rpc:call(Node, ?MODULE, start, [{Proc, N}]) of
        {badrpc, _Reason} -> {error, _Reason};
        Res -> Res
    end;
start({_Proc, _N}) ->
    Proc = cclib_utils:to_atom(_Proc),
    N = list_to_integer(_N),
    case ps_info(Proc, N) of
        PidL when length(PidL) =:= 2 ->
            PidL;
        _ ->
            Cmd = case  Proc of
                mds   -> io_lib:format("~s/yfs_mds -n ~B", [?YFS_SBIN, N]);
                smds  -> io_lib:format("~s/yfs_mds -n ~B", [?YFS_SBIN, N]);
                cds   -> io_lib:format("~s/yfs_cds -n ~B", [?YFS_SBIN, N]);
                proxy -> io_lib:format("~s/proxy_server", [?YFS_SBIN])
            end,
            io:format("~s/yfs_mds -n ~B", [?YFS_SBIN, N]),
            edog_common:cmd(Cmd),
            ps_info(Proc, N)
    end.

stop({Proc, IP, N}) ->
    Node = edog_common:get_node(IP),
    case rpc:call(Node, ?MODULE, stop, [{Proc, N}]) of
        {badrpc, _Reason} -> {error, _Reason};
        Res -> Res
    end;
stop({_Proc, _N}) ->
    Proc = cclib_utils:to_atom(_Proc),
    N = list_to_integer(_N),
    case ps_info(Proc, N) of
        [] -> [];
        PidL ->
            {Pids, _} = lists:unzip(PidL),
            lists:foreach(fun(Pid) -> os:cmd("kill -9 " ++ integer_to_list(Pid)) end, Pids),
            ps_info(Proc, N)
    end.

%% ---------------------------------------------------------------------
%% YFS interaction
%% Directory Structure:
%% /oss .. public
%%      .. user1/disks/
%%      .. user2/disks/
%% ---------------------------------------------------------------------
ps(Proc, 0) ->
    case Proc of
        mds   -> "ps --no-header -o pid,cmd -C yfs_mds";
        smds  -> "ps --no-header -o pid,cmd -C yfs_mds";
        cds   -> "ps --no-header -o pid,cmd -C yfs_cds";
        proxy -> "ps --no-header -o pid,cmd -C proxy_server"
    end;
ps(Proc, N) when N > 0 ->
    Str =
    case Proc of
        mds   -> io_lib:format("ps --no-header -o pid,cmd -C yfs_mds|grep 'n ~B'", [N]);
        smds  -> io_lib:format("ps --no-header -o pid,cmd -C yfs_mds|grep 'n ~B'", [N]);
        cds   -> io_lib:format("ps --no-header -o pid,cmd -C yfs_cds|grep 'n ~B'", [N]);
        proxy -> "ps --no-header -o pid,cmd -C proxy_server"
    end,
    lists:flatten(Str).

ps_info(Proc) -> ps_info(Proc, 0).
ps_info(Proc, N) ->
    Ps = string:tokens(os:cmd(ps(Proc, N)), "\n"),
    lists:map(
        fun(Line) ->
            {Pid, Cmd} = string:to_integer(string:strip(Line)),
            {Pid, string:strip(Cmd)}
        end, Ps).

%% used-total-send-recv-time
-define(YFS_PROC, "/dev/shm/sysy/proc/").

info(Proc, N) ->
    case ps_info(Proc, N) of
        [] -> {[], []};
        PidL when is_list(PidL) ->
            case Proc of
                cds -> {PidL, cds_info(N)};
                _   -> {PidL, []}
            end
    end.

cds_info(N) when N > 0 ->
    Path = filename:join([?YFS_PROC,  "cds_" ++ integer_to_list(N),  "status"]),
    case file:read_file(Path) of
        {ok, Bin} ->
            KeyL = [used_memory, total_memory, tx, rx, timestamp],
            ValL = [list_to_integer(X) || X <- string:tokens(binary_to_list(Bin), " \t")],
            lists:zip(KeyL, ValL);
        _ ->
            []
    end.

-module(uss_yfs_info).
-compile(export_all).

%% ---------------------------------------------------------------------
%% used-total-send-recv-time
%% ---------------------------------------------------------------------
-define(YFS_PROC, "/dev/shm/sysy/proc/").

info(Type, N) ->
    case ps_info(Type, N) of
        [] -> {[], []};
        PidL when is_list(PidL) ->
            case Type of
                cds -> {PidL, cds_info(N)};
                _   -> {PidL, []}
            end
    end.

cds_info(N) when N > 0 ->
    Path = filename:join([?YFS_PROC,  "cds_" ++ integer_to_list(N), "status"]),
    case file:read_file(Path) of
        {ok, Bin} ->
            KeyL = [used_memory, total_memory, tx, rx, timestamp],
            ValL = [list_to_integer(X) || X <- string:tokens(binary_to_list(Bin), " \t")],
            lists:zip(KeyL, ValL);
        _ ->
            []
    end.

%% ---------------------------------------------------------------------
%% @doc YFS interaction
%% Directory Structure:
%% /oss .. public
%%      .. user1/disks/
%%      .. user2/disks/
%% @end
%% ---------------------------------------------------------------------
ps(Type, 0) ->
    case Type of
        c60   -> "ps --no-header -o pid,cmd -C c60d";
        mds   -> "ps --no-header -o pid,cmd -C yfs_mds";
        cds   -> "ps --no-header -o pid,cmd -C yfs_cds";
        nfs   -> "ps --no-header -o pid,cmd -C ynfs_server";
        proxy -> "ps --no-header -o pid,cmd -C proxy_server"
    end;
ps(Type, N) when N > 0 ->
    Str =
    case Type of
        c60   -> io_lib:format("ps --no-header -o pid,cmd -C c60d|grep 'n ~B'", [N-1]);
        mds   -> io_lib:format("ps --no-header -o pid,cmd -C yfs_mds|grep 'n ~B'", [N]);
        cds   -> io_lib:format("ps --no-header -o pid,cmd -C yfs_cds|grep 'n ~B'", [N]);
        nfs   -> "ps --no-header -o pid,cmd -C ynfs_server";
        proxy -> "ps --no-header -o pid,cmd -C proxy_server"
    end,
    lists:flatten(Str).

ps_info(Type) ->
    ps_info(Type, 0).
ps_info(Type, N) ->
    Ps = string:tokens(os:cmd(ps(Type, N)), "\n"),
    lists:map(fun(Line) ->
                {Pid, Cmd} = string:to_integer(string:strip(Line)),
                {Pid, string:strip(Cmd)}
        end, Ps).

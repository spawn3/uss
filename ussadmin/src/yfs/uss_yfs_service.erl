-module(uss_yfs_service).
-compile(export_all).

-include("uss_common.hrl").

info() ->
    L = [service_scan_1(Srv) || Srv <- [c60, mds, nfs, proxy]],
    Lcds = service_scan_1(cds),
    Lrjnld = case Lcds of
        [] -> [];
        _ -> service_scan_1(rjnld)
    end,
    L1 = lists:flatten(L) ++ Lcds ++ Lrjnld,
    [get_info(X) || X <- L1].

services() ->
    [c60, mds, rjnld, cds, nfs, proxy].

is_service(Srv) ->
    lists:member(Srv, services()).

% file access server
is_fas(#uss_yfs_t{sid={_Ip, Type, _N}}) -> is_fas(Type);
is_fas(proxy) -> true;
is_fas(nfs) -> true;
is_fas(_) -> false.

service_scan_1(Srv) ->
    Ip = cclib_utils:get_ip(node()),
    Srvs =
    case service_scan(Srv) of
        {ok, Dirs} -> Dirs;
        {error, _} -> []
    end,
    [#uss_yfs_t{sid={Ip, Srv, X}} || X <- Srvs].

service_enabled(rjnld) ->
    filelib:is_regular(filename:join([?YFS_PREFIX, "app/sbin/rjnld"]));
service_enabled(_) ->
    true.

service_scan(rjnld) ->
    case service_enabled(rjnld) of
        true -> {ok, [0]};
        false -> {error, {not_enabled, rjnld}}
    end;
service_scan(proxy) -> {ok, [0]};
service_scan(nfs) -> {ok, [0]};
service_scan(c60) -> {ok, [0]};
service_scan(mds) -> {ok, [1]};
service_scan(Srv) ->
    Prefix = service_prefix(Srv),
    case file:list_dir(Prefix) of
        {ok, Dirs} ->
            {ok, [list_to_integer(X) || X <- Dirs,
                    is_service_dir(Prefix, X)]};
        {error, _Reason} ->
            {error, _Reason}
    end.

is_service_dir(Prefix, Dir) ->
    try list_to_integer(Dir) of
        N when is_integer(N) ->
            case filelib:is_dir(filename:join([Prefix, Dir])) of
                true ->
                    not
                    filelib:is_file(filename:join([Prefix,Dir, ?SKIP_ME]));
                false -> false
            end
    catch
        _:_ -> false
    end.

service_prefix(Srv) ->
    case Srv of
        c60   -> filename:join([?YFS_PREFIX, "c60"]);
        mds   -> filename:join([?YFS_PREFIX, "mds"]);
        cds   -> filename:join([?YFS_PREFIX, "cds"]);
        nfs   -> filename:join([?YFS_PREFIX, "nfs"]);
        proxy -> filename:join([?YFS_PREFIX, "proxy"]);
        rjnld -> filename:join([?YFS_PREFIX, "rjnld"])
    end.

get_info(#uss_yfs_t{sid={_Ip, Type, N}} = Yfs) ->
    case uss_yfs_status:info({Type, N}) of
        {ok, {Status, Pid}} ->
            Yfs#uss_yfs_t{pid=Pid, status=Status};
        {error, _} ->
            Yfs#uss_yfs_t{pid=-1, status=shutoff}
    end.

start_cmd_string({Type, N}) ->
    case Type of
        c60   -> io_lib:format("~s/app/sbin/c60d", [?YFS_PREFIX]);
        mds   -> io_lib:format("~s/app/sbin/yfs_mds -n ~B", [?YFS_PREFIX, N]);
        cds   -> io_lib:format("~s/app/sbin/yfs_cds -n ~B", [?YFS_PREFIX, N]);
        nfs   -> io_lib:format("~s/app/sbin/ynfs_server",   [?YFS_PREFIX]);
        proxy -> io_lib:format("~s/app/sbin/proxy_server",  [?YFS_PREFIX]);
        rjnld -> io_lib:format("~s/app/sbin/rjnld",  [?YFS_PREFIX])
    end.

stop_cmd_string(Pid, Force) ->
    case Force of
        true  -> "kill -9 " ++ integer_to_list(Pid);
        false -> "kill -USR2 " ++ integer_to_list(Pid)
    end.

log_file({Type, N}) ->
    case Type of
        c60   -> io_lib:format("~s/c60_~B.log", [?YFS_LOG_PREFIX, N]);
        mds   -> io_lib:format("~s/mds_~B.log", [?YFS_LOG_PREFIX, N]);
        cds   -> io_lib:format("~s/cds_~B.log", [?YFS_LOG_PREFIX, N]);
        nfs   -> io_lib:format("~s/nfs.log",    [?YFS_LOG_PREFIX]);
        proxy -> io_lib:format("~s/proxy.log",  [?YFS_LOG_PREFIX]);
        rjnld -> io_lib:format("~s/rjnld.log",  [?YFS_LOG_PREFIX])
    end.

pid_file({Type, N}) ->
    case Type of
        c60 -> {ok, filename:join([?YFS_PREFIX, "c60", integer_to_list(N), "status/status.pid"])};
        mds -> {ok, filename:join([?YFS_PREFIX, "mds", integer_to_list(N), "status/status.pid"])};
        cds -> {ok, filename:join([?YFS_PREFIX, "cds", integer_to_list(N), "status/status.pid"])};
        nfs -> {ok, filename:join([?YFS_PREFIX, "nfs", "status/status.pid"])};
        proxy -> {ok, filename:join([?YFS_PREFIX, "proxy", "status/status.pid"])};
        rjnld -> {ok, filename:join([?YFS_PREFIX, "rjnld", "status/status.pid"])};
        _ -> {error, {Type, N}}
    end.

status_file({Type, N}) ->
    case Type of
        c60 -> {ok, filename:join([?YFS_PREFIX, "c60", integer_to_list(N), "status/status"])};
        mds -> {ok, filename:join([?YFS_PREFIX, "mds", integer_to_list(N), "status/status"])};
        cds -> {ok, filename:join([?YFS_PREFIX, "cds", integer_to_list(N), "status/status"])};
        nfs -> {ok, filename:join([?YFS_PREFIX, "nfs", "status/status"])};
        proxy -> {ok, filename:join([?YFS_PREFIX, "proxy", "status/status"])};
        rjnld -> {ok, filename:join([?YFS_PREFIX, "rjnld", "status/status"])};
        _ -> {error, {Type, N}}
    end.

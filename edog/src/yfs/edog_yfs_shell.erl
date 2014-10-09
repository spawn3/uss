-module(edog_yfs_shell).
-compile(export_all).

-include("edog_common.hrl").
-include("edog_const.hrl").

-define(NO_SUCH_FILE, "No such file or directory").

get_xattr(File, Key) ->
    Cmd = io_lib:format("~s/yattr -g ~s ~s", [?YFS_BIN, Key, File]),
    Result = ?EXECUTE(Cmd),
    Last = lists:last(string:tokens(Result, "\n")),
    case lists:reverse(string:tokens(Last, " ")) of
        [Value, "value"|_] ->
            {ok, Value};
        _ ->
            {error, Last}
    end.

set_xattr(File, Key, Value) ->
    Cmd = io_lib:format("~s/yattr -s ~s -V ~s ~s", [?YFS_BIN, Key, Value, File]),
    Result = ?EXECUTE(Cmd),
    Last = lists:last(string:tokens(Result, "\n")),
    case lists:reverse(string:tokens(Last, " ")) of
        [Value, "value"|_] ->
            {ok, Value};
        _ ->
            {error, Value}
    end.

ensure_dir(Dir) ->
    case mkdir(Dir) of
        {ok, _} ->
            true;
        {error, _} ->
            false
    end.

ensure_dir_p(Path) ->
    Tokens = filename:split(Path),
    % TODO
    Seq = lists:seq(3, length(Tokens)-1),
    [ensure_dir(filename:join(lists:sublist(Tokens, 1, X))) || X <- Seq].

mkdir(Path) ->
    Cmd = io_lib:format("~s/ymkdir ~s", [?YFS_BIN, Path]),
    _Result = ?EXECUTE(Cmd),
    Last = lists:last(string:tokens(_Result, "\n")),
    case cclib_utils:check_cmd_result(Last, ["mkdir.*finished", "File exist.*"]) of
        true ->
            {ok, true};
        false ->
            {error, _Result}
    end.

copyfile([Src, Path]) ->
    %ensure_dir_p(Path),
    case clone(Src, Path) of
        {ok, Info} ->
            {ok, Info};
        {error, Info} ->
            {error, Info}
    end.
    %case cclib_utils:is_process_exists("proxy_server") of
    %    true ->
    %        copyfile2(Src, Path);
    %    false ->
    %        {error, proxy_server_not_started}
    %end.

-define(COPY_FILE_TIMEO, 72000000).

copyfile2(Src, Path) ->
    Cmd = io_lib:format("~s/ycp :~s :~s", [?YFS_BIN, Src, Path]),
    cclib_cmd:exec_debug(Cmd, ?COPY_FILE_TIMEO).

clone(Src, Path) ->
    %Log = clone_log_file(),
    %filelib:ensure_dir(Log),
    Cmd = io_lib:format("~s/uss.clone ~s ~s", [?YFS_BIN, Src, Path]),
    cclib_cmd:exec_debug(Cmd, ?COPY_FILE_TIMEO).

clone_log_file() ->
    Name = lists:concat(["uss.clone-", cclib_utils:time_string(), ".log"]),
    filename:join(["/var/log/uss/clone", Name]).

kill_proc(DiskId) ->
    kill_proc("uss.clone", DiskId).

kill_proc(Proc, DiskId) ->
    Script = filename:join([code:lib_dir(?APPLICATION), "script/kill_process_of_cmd.sh"]),
    Cmd = io_lib:format("bash ~s ~s ~s", [Script, Proc, DiskId]),
    Result = ?EXECUTE(Cmd),
    case cclib_utils:check_cmd_result(Result, ["ok"]) of
        true ->
            {ok, true};
        false ->
            {error, Result}
    end.

rm(Path) ->
    Cmd = io_lib:format("~s/yrm ~s", [?YFS_BIN, Path]),
    Result = ?EXECUTE(Cmd),
    case cclib_utils:check_cmd_result(Result, [""]) of
        true ->
            rmdir(filename:dirname(Path)),
            {ok, true};
        false ->
            rmdir(filename:dirname(Path)),
            Last = lists:last(string:tokens(Result, "\n")),
            case cclib_utils:check_cmd_result(Last, [?NO_SUCH_FILE]) of
                true ->
                    {ok, true};
                false ->
                    {error, Result}
            end
    end.

rmdir(Path) ->
    Cmd = io_lib:format("~s/yrmdir ~s", [?YFS_BIN, Path]),
    Result = ?EXECUTE(Cmd),
    case cclib_utils:check_cmd_result(Result, [""]) of
        true ->
            {ok, true};
        false ->
            {error, Result}
    end.

-define(CREATE_IMG_TIMEOUT, 20000).

create_img([Path, Format, Size]) ->
    ensure_dir_p(Path),
    Cmd = io_lib:format("~s create -f ~s ~s ~wG", [
            edog_conf:bin_kvm_img(), Format, Path, Size]),
    case cclib_cmd:run(Cmd, ?CREATE_IMG_TIMEOUT) of
        {0, _Data} ->
            set_xattr(Path, "prealloc", "true"),
            set_xattr(Path, "writeback", "false"),
            {ok, true};
        {_Exit, {timeout, Data}} ->
            {error, iolist_to_binary(Data)};
        {_Exit, Data} ->
            {error, iolist_to_binary(Data)};
        _ ->
            {error, oops}
    end.

mdstat() ->
    Cmd = filename:join([?YFS_BIN, "uss.mdstat"]),
    case cclib_cmd:run(Cmd, 15000) of
        {0, Data} ->
            Entries = [to_mds_entry(X) || X <- Data],
            Entries2 = [X || #mds_entry{} = X <- Entries],
            {ok, #mdstat{mds_entries=Entries2}};
        {ExitCode, _Data} ->
            {error, {ExitCode, _Data}}
    end.

to_cluster(Line) ->
    ["cluster", Cluster|_] = string:tokens(Line, " :"),
    Cluster.

% cluster : yfsyfsyfs
% 192.168.2.19:mds/0 id:24_v1318510639 status:master peer:5 sync:100
% 192.168.2.18:mds/0 id:1_v1318510639 status:slave peer:5 sync:100
% 192.168.2.15:mds/0 id:2_v1318510639 status:slave peer:5 sync:100
% 192.168.2.17:mds/0 id:3_v1318510639 status:slave peer:5 sync:100
% 192.168.2.21:mds/0 id:NA status:NA peer:NA sync:NA
to_mds_entry(Line) ->
    case string:tokens(Line, " :/") of
        [Ip, "mds", N, "id", Id, "status", Status, "peer", Peer, "sync", Sync|_] ->
            #mds_entry{
                ip=Ip,
                n=list_to_integer(N),
                id=Id,
                status=Status,
                peer=to_integer(Peer),
                sync=to_integer(Sync)
            };
        _ ->
            false
    end.

to_integer(Sth) ->
    try
        list_to_integer(Sth)
    catch
        _:_ -> 0
    end.

is_yfs_ready() ->
    Cmd = io_lib:format("bash ~s/script/is_local_yfs_ready.sh", [?APP_SRC]),
    ?INFO(iolist_to_binary(Cmd)),
    case cclib_cmd:run(Cmd, 16000) of
        {0, _} -> true;
        _      -> false
    end.

is_yfs_ready2() ->
    case mdstat() of
        {ok, #mdstat{mds_entries=Entries}} ->
            case Entries of
                [#mds_entry{peer=Peer}|_] ->
                    (length(Entries) > (Peer div 2)) andalso lists:any(fun is_mds_master/1, Entries);
                _ ->
                    false
            end;
        _ ->
            false
    end.

is_mds_master(#mds_entry{status="master"}) -> true;
is_mds_master(_) -> false.

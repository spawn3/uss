-module(uss_yfs_log).
-export([
        clean_log/2,
        dump_log/2,

        do_clean_log/1,
        do_dump_log/1
    ]).

-include("uss_common.hrl").

-define(DEFAULT_LOG_DIR,    "/var/log/uss/").
-define(DEFAULT_BACKUP_DIR, "/var/log/uss-backup@").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clean_log(Ip, Options) ->
    Node = clib_utils:get_node(Ip),
    case rpc:call(Node, ?MODULE, do_clean_log, [Options]) of
        {badrpc, Reason} -> {error, Reason};
        Res -> {ok, Res}
    end.

dump_log(Ip, Options) ->
    Node = clib_utils:get_node(Ip),
    case rpc:call(Node, ?MODULE, do_dump_log, [Options]) of
        {badrpc, Reason} -> {error, Reason};
        Res -> {ok, Res}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Impl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_clean_log(_Options) ->
    case file:list_dir(?DEFAULT_LOG_DIR) of
        {ok, L} ->
            L2 = [filename:join([?DEFAULT_LOG_DIR, X]) || X <- L],
            lists:foreach(fun truncate_file/1, L2);
        {error, Reason} ->
            {error, Reason}
    end.

do_dump_log(Options) ->
    Ts = proplists:get_value(ts, Options, cclib_utils:time_string()),
    LogDir = ?DEFAULT_LOG_DIR,
    BackDir = ?DEFAULT_BACKUP_DIR ++ Ts,
    Cmd = io_lib:format("mkdir -p ~s; cp -rf ~s/* ~s", [BackDir, LogDir, BackDir]),
    ?os_cmd(Cmd).

truncate_file(File) ->
    case file:open(File, [write]) of
        {ok, IoDev} ->
            file:truncate(IoDev),
            file:close(IoDev);
        {error, Reason} ->
            {error, Reason}
    end.

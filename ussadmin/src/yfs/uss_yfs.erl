%% ------------------------------------------------------------
%% @author Dongguanjun <dongguanjun@meidisen.com>
%% @copyright 2010 Meidisen Co.,Ltd
%% @doc
%% @end
%% ------------------------------------------------------------
-module(uss_yfs).
-behaviour(gen_server).
-export([
        start_link/0,

        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2,

        % public
        start_nfs/1,
        start_proxy/1,
        start/1,
        stop/1,
        stop/2,

        lvm_create/4,
        lvm_resize/4,
        lvm_list/2,

        get_log/2,
        get_conf/0,
        get_status/1,
        get_status/2,

        % private
        do_start_2/1,
        do_stop_2/2
    ]).
-compile(export_all).

-include("uss_common.hrl").

-record(state, {
        indent
    }).

-define(USS_YFS_CMD_TABLE, uss_yfs_cmd_table).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------
server_ref(Node) when is_atom(Node) ->
    {?MODULE, Node};
server_ref(Ip) ->
    {?MODULE, clib_utils:get_node(Ip)}.

start_nfs(Ip) ->
    start(#service_id{ip=Ip, type=nfs, n=0}).

start_proxy(Ip) ->
    start(#service_id{ip=Ip, type=proxy, n=0}).

start(#service_id{ip=Ip} = Sid) ->
    gen_server:cast(server_ref(Ip), {start, Sid});
start({Ip, Type, N}) ->
    start(#service_id{ip=Ip, type=Type, n=N}).

stop({Ip, Type, N}) ->
    stop(#service_id{ip=Ip, type=Type, n=N}, false).

stop(#service_id{ip=Ip} = Sid, Force) ->
    gen_server:cast(server_ref(Ip), {stop, Sid, Force});
stop({Ip, Type, N}, Force) ->
    stop(#service_id{ip=Ip, type=Type, n=N}, Force).

lvm_create(Ip, Name, Size, Options) ->
    gen_server:call(server_ref(Ip), {lvm_create, Name, Size, Options}).

lvm_resize(Ip, Name, Size, Options) ->
    gen_server:call(server_ref(Ip), {lvm_resize, Name, Size, Options}).

lvm_list(Ip, Options) ->
    gen_server:call(server_ref(Ip), {lvm_list, Options}).

get_log({Ip, Type, N}, Options) ->
    gen_server:call(server_ref(Ip), {get_log, Type, N, Options}).

get_status({Ip, Type, N}, Options) ->
    gen_server:call(server_ref(Ip), {get_status, Type, N, Options}).

get_status({Ip, Type, N}) ->
    get_status({Ip, Type, N}, []).

% private
%% -------------------------------------------------------------------
%% Callback
%% -------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    _ = ets:new(?USS_YFS_CMD_TABLE, [set, named_table, protected]),
    {ok, #state{}}.

% Volume
handle_call({lvm_create, Name, Size, Options}, _From, _State) ->
    Reply = do_lvm_create(Name, Size, Options),
    {reply, Reply, _State};
handle_call({lvm_resize, Name, Size, Options}, _From, _State) ->
    Reply = do_lvm_resize(Name, Size, Options),
    {reply, Reply, _State};
handle_call({lvm_list, Options}, _From, _State) ->
    Reply = do_lvm_list(Options),
    {reply, Reply, _State};
%
handle_call({get_log, Type, N, Options}, _From, _State) ->
    Reply = do_get_log({Type, N}, Options),
    {reply, Reply, _State};

handle_call({get_status, Type, N, Options}, _From, _State) ->
    Reply = do_get_status({Type, N}, Options),
    {reply, Reply, _State};

handle_call(_Msg, _From, _State) ->
    ?ERROR_REPORT(_Msg),
    Reply = ok,
    {reply, Reply, _State}.

handle_cast({start, #service_id{type=Type, n=N}}, _State) ->
    do_start({Type, N}),
    {noreply, #state{indent=start}};
handle_cast({stop, #service_id{type=Type, n=N}, Force}, _State) ->
    do_stop({Type, N}, Force),
    {noreply, #state{indent=stop}};
handle_cast(_Msg, _State) ->
    {noreply, _State}.

handle_info({'EXIT', _From, _Reason} = _Msg, _State) ->
    ?INFO_REPORT(_Msg),
    {noreply, _State};
handle_info(_Msg, _State) ->
    {noreply, _State}.

code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.

terminate(_Reason, _State) ->
    {noreply, _State}.

%% ---------------------------------------------------------
%% DO method
%% ---------------------------------------------------------
do_get_status({Type, N}, _Options) ->
    case uss_yfs_status:get_status({Type, N}) of
        {ok, {cannot_lock, Status}} ->
            {ok, Status};
        {ok, _} ->
            {ok, stopped};
        {error, enoent} ->
            {ok, stopped};
        {error, Reason} ->
            {error, Reason}
    end.

do_start({Type, N}) ->
    case uss_yfs_status:get_status({Type, N}) of
        % TODO
        {ok, {cannot_lock, running}} ->
            ?SEND_TRAP(Type, N, yfs_start, running, running, already_started),
            {ok, {already_started, running}};
        {ok, {cannot_lock, Status}} ->
            ?SEND_TRAP(Type, N, yfs_start, Status, Status, already_started),
            {ok, {already_started, Status}};
        _ ->
            cclib_async:async_apply(?MODULE, do_start_2, [{Type, N}])
    end.

do_start_2({Type, N}) ->
    case local_start({Type, N}) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            % TODO
            ?SEND_TRAP(Type, N, local_start, null, shutoff, Reason),
            {error, Reason}
    end.

local_start({Type, N}) ->
    Cmd = uss_yfs_service:start_cmd_string({Type, N}),
    Result = cclib_utils:cmd(Cmd),
    case cclib_utils:check_cmd_result(Result, [""]) of
        true ->
            {ok, begin_start};
        false ->
            {error, Result}
    end.

do_stop({Type, N}, Force) ->
    case uss_yfs_status:get_status({Type, N}) of
        {ok, {cannot_lock, _Status}} ->
            cclib_async:async_apply(?MODULE, do_stop_2, [{Type, N}, Force]);
        _Other ->
            _Other
    end.

do_stop_2({Type, N}, Force) ->
    case local_stop({Type, N}, Force) of
        {ok, begin_stop} ->
            {ok, begin_stop};
        _Other ->
            _Other
    end.

local_stop({Type, N}, Force) ->
    case uss_yfs_status:get_pid({Type, N}) of
        {ok, Pid} when is_integer(Pid) ->
            Cmd = uss_yfs_service:stop_cmd_string(Pid, Force),
            cclib_utils:cmd(Cmd),
            {ok, begin_stop};
        {error, Reason} ->
            ?INFO_REPORT(Reason),
            {error, Reason}
    end.

%% ---------------------------------------------
%% volume management
%% ---------------------------------------------
lvm_callback(#callback_cb{from=From, result={_Flag, _Info}=Result}) ->
    cclib_async:send(From, Result),
    case Result of
        {ok, Info} ->
            {ok, Info};
        {error, timeout} ->
            kill_lvm(),
            {error, timeout};
        {error, Reason} ->
            {error, Reason}
    end.

kill_lvm() ->
    Cmd = io_lib:format("pkill -9 ylvm", []),
    cclib_utils:cmd(Cmd).

do_lvm_create(Name, Size, Options)->
    From = proplists:get_value(from, Options),
    cclib_async:async_apply(?MODULE, local_lvm_create, [Name, Size],
        {fun lvm_callback/1, #callback_cb{from=From, context='lvm_create'}},
        ?TIMEOUT_LVM).

do_lvm_resize(Name, Size, Options) ->
    From = proplists:get_value(from, Options),
    cclib_async:async_apply(?MODULE, local_lvm_resize, [Name, Size],
        {fun lvm_callback/1, #callback_cb{from=From, context='lvm_create'}},
        ?TIMEOUT_LVM).

do_lvm_list(Options) ->
    From = proplists:get_value(from, Options),
    cclib_async:async_apply(?MODULE, local_lvm_list, [],
        {fun lvm_callback/1, #callback_cb{from=From, context='lvm_create'}},
        ?TIMEOUT_LVM).

% --------------------------------------
%-define(TJ_TEST, true).

-ifdef(TJ_TEST).

local_lvm_create(_Name, _Size) ->
    {ok, true}.

local_lvm_resize(_Name, _Size) ->
    {ok, true}.

local_lvm_list() ->
    L = [#volume{name="test1",id=1,size=1024,fileid="1_v1000"},
         #volume{name="test2",id=2,size=1024,fileid="1_v1001"}],
     {ok, [uss_json:eterm_to_json(R) || R <- L]}.

-else.

local_lvm_create(Name, Size) ->
    Cmd = if
        Size =< 0 ->
            io_lib:format("~s/app/bin/ylvm --create ~s", [?YFS_PREFIX, Name]);
        true ->
            io_lib:format("~s/app/bin/ylvm --create ~s ~BG", [?YFS_PREFIX, Name, Size])
    end,
    Result = cclib_utils:cmd(Cmd),
    case cclib_utils:check_cmd_result(Result, [""]) of
        true -> {ok, true};
        false -> {error, Result}
    end.

local_lvm_resize(Name, Size) ->
    Cmd = io_lib:format("~s/app/bin/ylvm --resize ~s ~B", [?YFS_PREFIX, Name, Size]),
    Result = cclib_utils:cmd(Cmd),
    case cclib_utils:check_cmd_result(Result, [""]) of
        true -> {ok, true};
        false -> {error, Result}
    end.

local_lvm_list() ->
    Cmd = io_lib:format("~s/app/bin/ylvm --list|grep 'name.*id'", [?YFS_PREFIX]),
    Result = cclib_utils:cmd(Cmd),
    L = case cclib_utils:check_cmd_result(Result, [""]) of
        true  -> [];
        false -> parse_lvm(Result)
    end,
    {ok, [uss_json:eterm_to_json(R) || R <- L]}.

parse_lvm(Result) ->
    L = string:tokens(Result, "\n"),
    [Vol || Vol <- [parse_lvm_line(Line) || Line <- L], Vol =/= false].

parse_lvm_line(Line) ->
    parse_lvm_list(string:tokens(Line, " \t")).

parse_lvm_list(L) when length(L) =:= 9 ->
    #volume{
        name   = lists:nth(3,L),
        id     = lists:nth(5,L),
        size   = lists:nth(7,L),
        fileid = lists:nth(9,L)
    };
parse_lvm_list(_) ->
    false.

-endif.

do_get_log({Type, N}, Options) ->
    Lines = proplists:get_value(lines, Options, 30),
    Level = proplists:get_value(level, Options, all),
    Cmd =
    case Level of
        all ->
            io_lib:format("tail -n ~B ~s", [
                    Lines, uss_yfs_service:log_file({Type, N})]);
        _ ->
            io_lib:format("tail -n ~B ~s|grep ~s", [
                    Lines, uss_yfs_service:log_file({Type, N}),
                    map_level(Level)])
    end,
    Result = cclib_utils:cmd(Cmd),
    {ok, cclib_utils:to_binary(Result)}.

map_level(error)   -> "ERROR";
map_level(warning) -> "WARN";
map_level(_)       -> "INFO".

%% -----------------------------------------------------------
get_conf() ->
    RootDir = code:lib_dir(?APPLICATION),
    case uss_yfs_conf:get_conf(filename:join([RootDir, "conf/yfs.conf.tpl"])) of
        {ok, L} ->
            L;
        {error, _Reason} ->
            []
    end.

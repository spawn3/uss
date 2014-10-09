-module(edog_storage).
-behaviour(gen_server).
-export([
        start_link/0,
        init/1,
        terminate/2,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,

        customer_create/1,
        disk_create/4,
        copy_file/3,
        rm/1
    ]).
-compile(export_all).

-include("edog_common.hrl").

-define(NOTIFY_MASTER(Op, Key, Result),
    begin
        edog_master:notify(#notify_spec{op=Op, key=Key, reply=Result}),
        Result
    end).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------
customer_create(VolName) ->
    gen_server:call(?MODULE, {customer_create, VolName}).

disk_create(Ns, DiskId, Format, Size) ->
    gen_server:call(?MODULE, {disk_create, Ns, DiskId, Format, Size}, ?TO_CALL).

copy_file(Src, Ns, DiskId) ->
    gen_server:call(?MODULE, {copy_file, Src, Ns, DiskId}).

rm(Path) ->
    gen_server:call(?MODULE, {rm, Path}, ?TO_CALL).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% -------------------------------------------------------------------
%% Callback
%% -------------------------------------------------------------------
init(_Args) ->
    process_flag(trap_exit, true),
    _ = ets:new(edog_copy_procs, [set, named_table, public]),
    {ok, []}.

terminate(_Reason, _State) ->
    ok.

handle_call({customer_create, VolName} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    ?PROFILE_BEGIN(),
    Reply = user_create(VolName),
    ?PROFILE_END(customer_create),
    {reply, Reply, _State};
handle_call({disk_create, Ns, DiskId, Format, Size} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    % Reply = async_disk_create(Ns, DiskId, Format, Size),
    ?PROFILE_BEGIN(),
    Reply = sync_disk_create(Ns, DiskId, Format, Size),
    ?PROFILE_END(disk_create),
    {reply, Reply, _State};
handle_call({rm, Path}, _From, _State) ->
    Reply = sync_rm(Path),
    {reply, Reply, _State};
handle_call({copy_file, Src, Ns, DiskId} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = async_copyfile(Src, Ns, DiskId),
    {reply, Reply, _State};

handle_call(_Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = ok,
    {reply, Reply, _State}.

handle_cast(_Msg, _State) ->
    {noreply, _State}.

handle_info({'EXIT', _From, _Reason} = _Msg, _State) ->
    ?INFO(_Msg),
    {noreply, _State};
handle_info(_Msg, _State) ->
    {noreply, _State}.

code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.

%% ---------------------------------------------------------------------
%% Model Method
%% ---------------------------------------------------------------------
sync_disk_create(Ns, DiskId, Format, Size) ->
    yfs_mkdisk([Ns, DiskId, Format, Size]).

sync_rm(Path) ->
    local_rm(Path).

mkdisk_callback(#callback_cb{result=Result, context={Op, Key}}) ->
    ?NOTIFY_MASTER(Op, Key, Result).

async_disk_create(Ns, DiskId, Format, Size) ->
    cclib_async:async_apply(?MODULE, yfs_mkdisk, [[Ns, DiskId, Format, Size]],
        {fun mkdisk_callback/1, #callback_cb{context={disk_create, DiskId}}},
        ?TO_RPC_LONG).

async_copyfile(Src, Ns, DiskId) ->
    case select_slave() of
        {error, _Reason} ->
            {error, _Reason};
        {ok, Node} ->
            Timeout = edog_conf:copyfile_timeout(),
            ?INFO([{action, copyfile}, {node, Node}, {timeout, Timeout}]),
            do_copyfile(Node, Src, Ns, DiskId, Timeout)
    end.

do_copyfile(Node, Src, Ns, DiskId, Timeout) ->
    F = fun() ->
        ?PROFILE_BEGIN(),
        Result =
        case rpc:call(Node, cclib_async, monitor, [?MODULE, yfs_copyfile, [[Src, Ns, DiskId]], Timeout*1000]) of
            {badrpc, Reason} ->
                {error, Reason};
            {error, Reason} ->
                {error, Reason};
            {ok, Res} ->
                {ok, Res};
            ok ->
                {ok, true}
        end,
        unselect_slave(Node),
        ?PROFILE_END(copyfile),
        % TODO
        {GlobalDiskId, _} = edog_storage_util:diskid(Ns, DiskId),
        case Result of
            {error, _} ->
                ?ERROR({copy_file, Src, Ns, DiskId, Result}),
                edog_yfs_shell:kill_proc(DiskId);
                %sync_rm(Dest);
            _ ->
                ok
        end,
        ?NOTIFY_MASTER(yfs_cp, GlobalDiskId, Result)
    end,
    {ok, spawn(F)}.

-define(MAX_COPYFILES, 10).

-spec select_slave() -> {ok, atom()} | {error, any()}.
select_slave() ->
    Slaves = edog_master:slave_get_all(),
    {Node, Min} = select_slave(Slaves, {'nonode', 9999}),
    if
        Node =:= 'nonode' ->
            {error, no_slaves};
        Min >= ?MAX_COPYFILES ->
            {error, {too_many, Min}};
        true ->
            ets:insert(edog_copy_procs, {Node, Min+1}),
            {ok, Node}
    end.

select_slave([], {Node, Min}) ->
    {Node, Min};
select_slave([H|T], {Node, Min}) ->
    case ets:lookup(edog_copy_procs, H) of
        [] ->
            {H, 0};
        [{_, NumCopy}] when NumCopy < Min ->
            select_slave(T, {H, NumCopy});
        [{_, _NumCopy}] ->
            select_slave(T, {Node, Min})
    end.

unselect_slave(Node) ->
    case ets:lookup(edog_copy_procs, Node) of
        [{Node, NumCopy}] when NumCopy > 0 ->
            ets:insert(edog_copy_procs, {Node, NumCopy-1});
        _ ->
            true
    end.

%% ---------------------------------------------------------------
%% Adaptor
%% ---------------------------------------------------------------
storage_module() ->
    edog_conf:storage_module().

user_create(VolName) ->
    Module=storage_module(),
    ClusterName = edog_conf:cluster_name(),
    Path = edog_storage_util:disk_root(ClusterName, VolName),
    Module:ns_create(Path).

yfs_mkdisk([Ns, DiskId, Format, Size]) ->
    Module=storage_module(),
    Module:yfs_mkdisk([Ns, DiskId, Format, Size]).

local_rm(Path) ->
    Module=storage_module(),
    Module:rm(Path).

%% run on agents
yfs_copyfile([Src, Ns, DiskId]) ->
    Module=storage_module(),
    Module:yfs_copyfile([Src, Ns, DiskId]).

-module(uss_pm).
-export([
        is_alive/1,

        cluster_status/1,
        cluster_wait/2,
        cluster_deploy/0,
        cluster_deploy/2,
        cluster_start/1,

        update_etc/4,

        node_status/1,
        node_deploy/1,
        node_start/1
    ]).
-compile(export_all).

-include("uss_common.hrl").

is_alive(Node) when is_atom(Node) ->
    pong =:= net_adm:ping(Node);
is_alive(Ip) when is_binary(Ip), byte_size(Ip) > 7 ->
    is_alive(clib_utils:get_node(Ip));
is_alive(_Ip) ->
    false.

is_ready({_Ip, alive})   -> true;
is_ready({_Ip, can_ssh}) -> true;
is_ready({_Ip, _})       -> false.

% -----------------------------------------------------
% cluster operations
% -----------------------------------------------------
cluster_status() ->
    L = uss_mnesia:all_nodes(),
    cluster_status(L).

cluster_deploy(What) ->
    L = uss_mnesia:all_nodes(),
    cluster_deploy(L, What).

cluster_start() ->
    L = uss_mnesia:all_nodes(),
    cluster_start(L).

cluster_start_without_check() ->
    NodeList = uss_mnesia:all_nodes(),
    case p_cluster_apply(NodeList, ?MODULE, node_start, null) of
        {ok, L} -> {ok, [Ip || {Ip, _} <- L]};
        Other -> Other
    end.

cluster_status(NodeList) ->
    p_cluster_apply(NodeList, ?MODULE, node_status, null).

cluster_wait(_NodeList, 0) ->
    {error, timeout};
cluster_wait(NodeList, N) ->
    {ok, L} = cluster_status(NodeList),
    case lists:all(fun is_ready/1, L) of
        true -> ok;
        false ->
            timer:sleep(1000),
            cluster_wait(NodeList, N-1)
    end.

cluster_deploy(NodeList, What) when is_atom(What) ->
    L = NodeList -- [cclib_utils:get_ip(node())],
    p_cluster_apply(L, ?MODULE, node_deploy, What).

cluster_start(NodeList) when is_list(NodeList) ->
    case cluster_wait(NodeList, 10) of
        ok ->
            case p_cluster_apply(NodeList, ?MODULE, node_start, null) of
                {ok, L} -> {ok, [Ip || {Ip, _} <- L]};
                Other -> Other
            end;
        {error, Reason} ->
            {error, Reason}
    end.

p_cluster_apply(NodeList, M, F, A) ->
    p_cluster_apply(NodeList, M, F, A, []).

p_cluster_apply([], _M, _F, _A, Acc) ->
    {ok, lists:flatten(Acc)};
p_cluster_apply(NodeList, M, F, A, Acc) ->
    SubList = lists:sublist(NodeList, 10),
    L = [cclib_async:barrier_post(Ip, M, F, [{Ip, A}]) || Ip <- SubList],
    Result = cclib_async:barrier_wait(L),

    p_cluster_apply(lists:nthtail(length(SubList), NodeList), M, F, A, [Result|Acc]).

% -----------------------------------------------------
% node operations
% -----------------------------------------------------
node_status({Ip, _PlaceHolder}) ->
    case is_alive(Ip) of
        true -> alive;
        false ->
            case cclib_ssh:is_ssh(Ip) of
                true -> can_ssh;
                false -> cannot_ssh
            end
    end;
node_status(Ip) when is_binary(Ip) ->
    node_status({Ip, null}).

which_dir(What) ->
    case What of
        app ->
            [{"app", ""}];
        etc ->
            [{"etc", ""}];
        conf ->
            [{"ussadmin/data/conf", "ussadmin/data"}];
        ussadmin ->
            [{"ussadmin/ussadmin", "ussadmin"},
             {"ussadmin/data/conf", "ussadmin/data"}
            ];
        _ ->
            [{"app", ""},
             {"etc", ""},
             {"ussadmin/ussadmin", "ussadmin"},
             {"ussadmin/data/conf", "ussadmin/data"}
         ]
    end.

node_deploy({Ip, What}) ->
    case cclib_ssh:is_ssh(Ip) of
        true ->
            p_node_deploy({Ip, What});
        false ->
            case cclib_ssh:enable_ssh(Ip) of
                true  -> p_node_deploy({Ip, What});
                false -> {error, {cannot_connect, Ip}}
            end
    end.

p_node_deploy({Ip, What}) ->
    DirList = which_dir(What),
    lists:foreach(fun(Dir) -> deploy_dir(Ip, Dir) end, DirList),
    deploy_rest(Ip).

etc_file(c60)   -> filename:join([?YFS_PREFIX, "etc/c60_node"]);
etc_file(mds)   -> filename:join([?YFS_PREFIX, "etc/mds_node"]);
etc_file(fence) -> filename:join([?YFS_PREFIX, "etc/fence.conf"]).

p_write_node_file([{_, Type, _}|_] = L) ->
    Path = etc_file(Type),
    Count = length(L),
    F = fun({Ip, _, _}) ->
        file:write_file(Path, lists:concat([binary_to_list(Ip), '\n']), [write,append])
    end,
    file:write_file(Path, lists:concat([Count, '\n']), [write]),
    [F(X) || X <- L];
p_write_node_file(Type) when is_atom(Type) ->
    % TODO
    Path = etc_file(Type),
    Count = uss_mnesia:yfs_count(Type),
    {ok, L} = uss_mnesia:yfs_select(Type),
    F = fun(#uss_yfs_t{sid={Ip, _, _}}) ->
        file:write_file(Path, lists:concat([binary_to_list(Ip), '\n']), [write,append])
    end,
    file:write_file(Path, lists:concat([Count, '\n']), [write]),
    [F(X) || X <- L].

write_fence(NodeList) ->
    Path = etc_file(fence),
    case file:open(Path, [write]) of
        {ok, File} ->
            lists:foreach(
                fun(Ip) -> file:write(File, cclib_utils:to_list(Ip) ++ "\n") end,
                NodeList),
            file:close(File);
        {error, Reason} ->
            {error, Reason}
    end.

update_etc(NodeList, Lc60, _Lmds, TupleList) ->
    write_fence(NodeList),
    % TODO
    p_write_node_file(Lc60),

    clib_script:update_yfs_count(TupleList),

    cluster_deploy(NodeList, etc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cluster_deploy() ->
    NodeList = uss_mnesia:all_nodes() -- [cclib_utils:get_ip(node())],
    cluster_deploy(NodeList, all).

check_all(NodeList) ->
    check_all(NodeList, []).

check_all([], []) ->
    {ok, nodes_ready};
check_all([], Acc) ->
    {error, Acc};
check_all([H|T], Acc) ->
    case check(H) of
        {ok, _} ->
            check_all(T, Acc);
        {error, Reason} ->
            check_all(T, [{H, Reason}|Acc])
    end.

check(Ip) ->
    case rpc:call(clib_utils:get_node(Ip), clib_script, check_node, []) of
        {badrpc, Reason} -> {error, Reason};
        Res -> Res
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SSH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
deploy_dir(Ip, {Src, Dest}) ->
    case uss_mnesia:pm_get_user(Ip) of
        {ok, {User, Password}} ->
            deploy_dir(Ip, User, Password, Src, Dest);
        {error, Reason} ->
            {error, Reason}
    end.

deploy_dir(Ip, _User, Password, Src, Dest) ->
    Root = code:lib_dir(?APPLICATION),
    Cmd = io_lib:format("bash ~s/script/ssh.sh -r ~s ~s ~s/~s ~s/~s", [
            Root, Ip, Password, ?YFS_PREFIX, Src, ?YFS_PREFIX, Dest]),
    ?os_cmd(Cmd).

deploy_rest(Node) when is_atom(Node) ->
    deploy_rest(cclib_utils:get_ip(Node));
deploy_rest(Ip) ->
    case uss_mnesia:pm_get_user(Ip) of
        {ok, {User, Password}} ->
            deploy_rest(Ip, User, Password);
        {error, Reason} ->
            {error, Reason}
    end.

deploy_rest(Ip, _User, Password) ->
    Root = code:lib_dir(?APPLICATION),
    Cmd = io_lib:format("bash ~s/script/ssh.sh -c ~s ~s ~s/script/after_deploy.sh",
        [Root, Ip, Password, Root]),
    ?os_cmd(Cmd).

node_start({Ip, _PlaceHolder}) ->
    case cclib_ssh:is_ssh(Ip) of
        true ->
            case uss_mnesia:pm_get_user(Ip) of
                {ok, {User, Password}} ->
                    node_start(Ip, User, Password);
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, {Ip, cannot_connect}}
    end.

node_start(Ip, _User, Password) ->
    Root = code:lib_dir(?APPLICATION),
    Cmd = io_lib:format("bash ~s/script/ssh.sh -c ~s ~s ~s/script/start_agent.sh ~s 2>/dev/null",
        [Root, Ip, Password, Root, Ip]),
    Result = ?os_cmd(Cmd),
    case cclib_utils:check_cmd_result(Result, ["", "Address.*maps.*"]) of
        true -> ok;
        false -> {error, {Ip, Result}}
    end.

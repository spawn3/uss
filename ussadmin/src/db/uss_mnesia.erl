-module(uss_mnesia).
-export([
        get_all_tables/0,
        cluster_setflag/1,
        cluster_getflag/0,
        cluster_deploy/0,
        cluster_check/0,

        rack_add/1,
        rack_delete/1,
        rack_is_exists/1,
        rack_info/1,

        pm_add/1,
        pm_delete/1,
        pm_update/3,
        pm_info/1,

        yfs_start/1,
        yfs_stop/1,
        yfs_add/1,
        yfs_delete/1,
        yfs_update/1,
        yfs_update_status/2,
        yfs_select/1,

        option_init/0,
        option_set/2,
        option_get/1,
        option_get/2,
        option_del/1,
        option_increment/1,
        option_increment/2,

        select/1,
        start/0
    ]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("uss_common.hrl").

-define(MAX_C60_NUM, 5).
-define(MAX_MDS_NUM, 5).

-define(UPDATE_TABLE(Table, Id, RefTupleList),
    p_transaction(
        fun() ->
            case mnesia:wread({Table, Id}) of
                [R] when is_record(R, Table) ->
                    R1 = ?record_update(Table, R, RefTupleList),
                    mnesia:write(R1),
                    R1;
                Other ->
                    Other
            end
        end)
    ).

-define(TRANSFORM_TABLE(Tab, F),
    begin
        case mnesia:transform_table(Tab, F, record_info(fields, Tab)) of
            {atomic, Res} -> {ok, Res};
            {aborted, Res} -> {error, Res}
        end
    end).

p_select(Pattern) ->
    cclib_mnesia:select(Pattern).

p_transaction(F) ->
    cclib_mnesia:transaction(F).

p_transaction(F, IsReturnValue) ->
    cclib_mnesia:transaction(F, IsReturnValue).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    try
        ?PROFILE_BEGIN(),
        create_schema(),
        ok = mnesia:wait_for_tables(get_all_tables(), 10000),
        ?PROFILE_END(wait_for_tables),

        % TODO
        mnesia:clear_table(uss_yfs_t),
        mnesia:clear_table(uss_queue_t),
        option_set(private_queue_counter, 1)
    catch
        Class:Reason ->
            ?TTY_ERROR_REPORT({Class, Reason, erlang:get_stacktrace()}),
            init:stop()
    end.

create_schema() ->
    % NOTE: all masters must be started to finish THIS!!!
    {ok, NodeList} = clib_utils:get_managers(),
    rpc:multicall(NodeList, mnesia, stop, []),

    Dir = mnesia:system_info(directory),

    ?TTY_INFO_REPORT({"Masters: ", NodeList}),
    ?TTY_INFO_REPORT({"Db directory: ", Dir}),

    rpc:multicall(NodeList, filelib, ensure_dir, [Dir]),
    case cclib_mnesia:is_schema_inited(Dir) of
        false ->
            ?TTY_INFO_REPORT({create_schema, NodeList, Dir}),
            cclib_nodes:wait_nodes(NodeList),
            mnesia:create_schema(NodeList);
        true ->
            ok
    end,

    rpc:multicall(NodeList, mnesia, start, []),

    Tables = mnesia:system_info(tables),

    ?TTY_INFO_REPORT({NodeList, Dir}),
    ?TTY_INFO_REPORT({Tables}),

    ?CREATE_TABLE(Tables, NodeList, uss_rack_t, ordered_set),
    ?CREATE_TABLE(Tables, NodeList, uss_pm_t, ordered_set),
    ?CREATE_TABLE(Tables, NodeList, uss_yfs_t, ordered_set),
    ?CREATE_TABLE(Tables, NodeList, uss_option_t, ordered_set),
    ?CREATE_TABLE(Tables, NodeList, uss_queue_t, ordered_set),
    %?CREATE_TABLE(Tables, NodeList, group, ordered_set),
    %?CREATE_TABLE(Tables, NodeList, user, ordered_set),

    %mnesia:clear_table(uss_yfs_t),

    ok.

get_all_tables() ->
    [uss_rack_t, uss_pm_t, uss_yfs_t, uss_option_t, uss_queue_t].

clear_tables() ->
    mnesia:clear_table(uss_queue_t),
    mnesia:clear_table(uss_rack_t),
    mnesia:clear_table(uss_pm_t),
    mnesia:clear_table(uss_yfs_t),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CLUSTER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cluster_stop_node(Ip) when is_binary(Ip) ->
    cluster_stop_node(clib_utils:get_node(Ip));
cluster_stop_node(Node) ->
    rpc:call(Node, init, stop, []).

cluster_setflag(true)  -> option_set(opt_uss_status, 'on');
cluster_setflag(false) -> option_set(opt_uss_status, 'off').

cluster_getflag() ->
    case option_get(opt_uss_status, 'off') of
        {ok, 'on'} -> true;
        _ -> false
    end.

cluster_deploy() ->
    NodeList = all_nodes_in_cluster(),
    uss_pm:cluster_deploy(NodeList, all),
    uss_pm:cluster_start(NodeList),

    {ok, L} = uss_pm:cluster_status(NodeList),
    NotAlive = [Ip||{Ip, Status}<-L, Status =/= alive],
    case NotAlive of
        [] -> {ok, NodeList};
        _ ->  {error, {deplay_failed, NotAlive}}
    end.

cluster_check() ->
    yfs_check_start_pre().

% TODO
set_list2(c60, IdList) ->
    yfs_set_master(c60, lists:usort(IdList));
set_list2(mds, IdList) ->
    yfs_set_master(mds, lists:usort(IdList)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RACK
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rack_add(RackName) ->
    case rack_is_exists(RackName) of
        true -> {error, exists};
        false ->
            F = fun() ->
                Id = p_rack_newid(),
                Row = #uss_rack_t{
                    id=Id,
                    name=RackName,
                    atime=cclib_utils:now_to_integer()},
                mnesia:write(Row),
                Row
            end,
            case p_transaction(F) of
                {ok, #uss_rack_t{id=Id}} -> {ok, Id};
                _Other -> _Other
            end
    end.

rack_update_sql({Id, RackName}) ->
    ?UPDATE_TABLE(uss_rack_t, Id, [{name, RackName},
            {atime, cclib_utils:now_to_integer()}]).

rack_delete(Id) ->
    Pattern = #uss_pm_t{rack=Id, _='_'},
    F = fun() ->
        case p_select(Pattern) of
            {ok, []} ->
                mnesia:delete({uss_rack_t, Id});
            _ ->
                mnesia:abort(rack_cannot_be_deleted)
        end
    end,
    p_transaction(F).

rack_is_exists(Id) when is_integer(Id) ->
    Pattern = #uss_rack_t{id=Id, _='_'},
    case p_select(Pattern) of
        {error, _} -> false;
        {ok, []} -> false;
        {ok, _} -> true
    end;
rack_is_exists(RackName) ->
    Pattern = #uss_rack_t{name=RackName, _='_'},
    case p_select(Pattern) of
        {error, _} -> false;
        {ok, []} -> false;
        {ok, _} -> true
    end.

rack_info(Id) ->
    Pattern = if
        is_integer(Id) -> #uss_rack_t{id=Id, _='_'};
        Id =:= undefined -> #uss_rack_t{_='_'};
        true -> #uss_rack_t{_='_'}
    end,
    case p_select(Pattern) of
        {ok, L} -> {ok, L};
        {error, Reason} -> {error, Reason}
    end.

p_rack_initid() ->
    option_set(private_rack_counter, 1).

p_rack_newid() ->
    option_increment(private_rack_counter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NODE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pm_add({Rack, Ip, Hostname, User, Passwd}) when is_integer(Rack) ->
    case is_node_exist(Ip) of
        true  ->
            {error, exists};
        false ->
            case rack_is_exists(Rack) of
                true ->
                    case cclib_ssh:enable_ssh(Ip, User, Passwd) of
                        true ->
                            Flag = 0,
                            Id = p_pm_newid(),
                            p_pm_add({Id, Rack, Ip, Hostname, User, Passwd, Flag});
                        false ->
                            {error, ssh_failed}
                    end;
                false ->
                    {error, no_such_rack}
            end
    end;
pm_add({Rack, Ip}) ->
    pm_add({Rack, Ip, "", "", ""}).

% no check
p_pm_add({Id, Rack, Ip, Hostname, User, Passwd, Flag}) ->
    F = fun() ->
        Row = #uss_pm_t{
            id=Id,
            cluster=?CLUSTER_NULL,
            rack=Rack,
            ip=Ip,
            hostname=Hostname,
            user=User,
            password=Passwd,
            status='shutoff',
            flag=Flag,
            c60_info=false,
            mds_info=false,
            nfs_info=true,
            proxy_info=true,
            atime=cclib_utils:now_to_integer()},
        mnesia:write(Row),
        Row
    end,
    case p_transaction(F) of
        {ok, #uss_pm_t{id=Id}} -> {ok, Id};
        {error, Reason} -> {error, Reason}
    end.

% TODO When from ?CLUSTER_0 to ?CLUSTER_NULL
pm_update_cluster(Id, ?CLUSTER_NULL) ->
    ?UPDATE_TABLE(uss_pm_t, Id, [{cluster, ?CLUSTER_NULL}]);
pm_update_cluster(Id, ?CLUSTER_0) ->
    ?UPDATE_TABLE(uss_pm_t, Id, [{cluster, ?CLUSTER_0}]).

pm_update_cluster() ->
    L = all_nodes(?CLUSTER_NULL, false),
    lists:foreach(fun(#uss_pm_t{id=Id}) -> pm_update_cluster(Id, ?CLUSTER_0) end, L).

% check args
pm_update_sql(#node_request{id=Id, rack=Rack, ip=Ip,
        hostname=Hostname, user=User, password=Passwd}) when is_integer(Rack) ->
    case rack_is_exists(Rack) of
        true ->
            Flag = 0,
            p_pm_add({Id, Rack, Ip, Hostname, User, Passwd, Flag});
        false ->
            {error, no_such_rack}
    end.

pm_delete(Id) ->
    case cluster_getflag() of
        false ->
            F = fun() ->
                case mnesia:wread({uss_pm_t, Id}) of
                    [#uss_pm_t{ip=Ip}] ->
                        pm_delete_assoc(Ip),
                        mnesia:delete({uss_pm_t, Id});
                    _ ->
                        mnesia:delete({uss_pm_t, Id})
                end
            end,
            p_transaction(F);
        true ->
            F = fun() ->
                case mnesia:wread({uss_pm_t, Id}) of
                    [#uss_pm_t{ip=Ip, cluster=?CLUSTER_0}] ->
                        Pattern = #uss_yfs_t{sid={Ip,'_','_'},_='_'},
                        case mnesia:match_object(Pattern) of
                            {atomic, []} ->
                                pm_delete_assoc(Ip),
                                mnesia:delete({uss_pm_t, Id});
                            _ ->
                                mnesia:abort(node_cannot_be_deleted)
                        end;
                    [#uss_pm_t{ip=Ip, cluster=?CLUSTER_NULL}] ->
                        pm_delete_assoc(Ip),
                        mnesia:delete({uss_pm_t, Id});
                    _ ->
                        mnesia:delete({uss_pm_t, Id})
                end
            end,
            p_transaction(F)
    end.

pm_delete_assoc(Ip) ->
    cluster_stop_node(Ip),
    yfs_delete(Ip).

pm_info(L) when is_list(L) ->
    Id = proplists:get_value(<<"id">>, L),
    if
        is_integer(Id) -> pm_info(Id);
        true ->
            case pm_info(undefined) of
                {ok, RL} -> {ok, do_limit(L, RL)};
                {error, Reason} -> {error, Reason}
            end
    end;
pm_info(Id) ->
    Pattern = if
        is_integer(Id) -> #uss_pm_t{id=Id, _='_'};
        Id =:= undefined -> #uss_pm_t{_='_'};
        true -> #uss_pm_t{_='_'}
    end,
    case p_select(Pattern) of
        {ok, L} when is_list(L) -> {ok, L};
        {error, Reason} -> {error, Reason}
    end.

pm_get_user(Ip) ->
    Pattern = #uss_pm_t{ip=Ip, _='_'},
    case p_select(Pattern) of
        {ok, [#uss_pm_t{user=User, password=Password}]} ->
            {ok, {User, Password}};
        {ok, []} ->
            {error, no_such_entry};
        {error, Reason} ->
            {error, Reason}
    end.

p_pm_initid() ->
    option_set(private_pm_counter, 1).

p_pm_newid() ->
    option_increment(private_pm_counter).

pm_update(Node, Time, Data) when is_atom(Node) ->
    Ip = cclib_utils:get_ip(Node),
    Pattern = #uss_pm_t{ip=Ip, _='_'},
    case p_select(Pattern) of
        {ok, [#uss_pm_t{id=Id}]} -> pm_update(Id, Time, Data);
        {ok, []} -> {error, no_such_entry};
        {error, Reason} -> {error, Reason}
    end;
pm_update(NodeId, Time, Data) when is_integer(NodeId) ->
    F = fun() ->
        {IsOld, Row} = case mnesia:wread({uss_pm_t, NodeId}) of
            [Pm] -> {true, Pm#uss_pm_t{status=running, time=Time, info=Data}};
            [] -> {false, #uss_pm_t{status=running, time=Time, info=Data}}
        end,
        mnesia:write(Row),
        IsOld
    end,
    case mnesia:transaction(F) of
        {atomic, true} -> ok;
        {atomic, false} -> do_agent_join(NodeId);
        {aborted, _Reason} -> {error, _Reason}
    end.

pm_update_status(Ip, Status) when is_binary(Ip) ->
    Pattern = #uss_pm_t{ip=Ip, _='_'},
    case p_select(Pattern) of
        {ok, [#uss_pm_t{id=Id}]} ->
            pm_update_status(Id, Status);
        Other ->
            Other
    end;
pm_update_status(Id, Status) when is_integer(Id) ->
    case ?UPDATE_TABLE(uss_pm_t, Id, [{status, Status}]) of
        {ok, #uss_pm_t{ip=Ip}} ->
            case Status of
                unavailable -> yfs_update_status(Ip, unknown);
                _ -> ok
            end;
        {error, Reason} ->
            {error, Reason}
    end.

all_nodes(Cluster, IsReturnIp) when is_integer(Cluster) ->
    Pattern = #uss_pm_t{cluster=Cluster, _='_'},
    case p_select(Pattern) of
        {ok, L} when IsReturnIp ->
            [Ip || #uss_pm_t{ip=Ip} <- L];
        {ok, L} -> L;
        {error, _} -> []
    end.

all_nodes() -> all_nodes(?CLUSTER_0, true) ++ all_nodes(?CLUSTER_NULL, true).

all_nodes_in_cluster() -> all_nodes(?CLUSTER_0, true).

any_node() ->
    case all_nodes_in_cluster() of
        [] -> [];
        L  -> hd(L)
    end.

all_type(Type) ->
    Pattern =
    case Type of
        c60   -> #uss_pm_t{cluster=?CLUSTER_0, c60_info=true, _='_'};
        mds   -> #uss_pm_t{cluster=?CLUSTER_0, mds_info=true, _='_'};
        nfs   -> #uss_pm_t{cluster=?CLUSTER_0, nfs_info=true, _='_'};
        proxy -> #uss_pm_t{cluster=?CLUSTER_0, proxy_info=true, _='_'}
    end,
    case p_select(Pattern) of
        {error, _} -> [];
        {ok, []} -> [];
        {ok, L} -> [ip_to_sid(Type, Ip) || #uss_pm_t{ip=Ip} <- L]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SERVICE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
yfs_add({_IP, Type, _Num} = Sid) when is_atom(Type) ->
    F = fun() ->
        case mnesia:read(uss_yfs_t, Sid) of
            [] ->
                mnesia:write(#uss_yfs_t{sid=Sid}),
                Sid;
            [_] ->
                mnesia:abort({uss_yfs_t, Sid})
        end
    end,
    p_transaction(F).

yfs_delete(#uss_yfs_t{sid=Sid}) ->
    yfs_delete(Sid);
yfs_delete({_IP, Type, _Num} = Sid) when is_atom(Type) ->
    F = fun() -> mnesia:delete({uss_yfs_t, Sid}) end,
    p_transaction(F);
yfs_delete(Ip) when is_binary(Ip) ->
    case yfs_select(Ip) of
        {ok, L} -> lists:foreach(fun(R) -> yfs_delete(R) end, L);
        _ -> ok
    end.

yfs_get_master(c60) -> all_type(c60);
yfs_get_master(mds) -> all_type(mds).

yfs_set_master(c60, []) ->
    F = fun(#uss_pm_t{} = Old) -> Old#uss_pm_t{c60_info=false} end,
    ?TRANSFORM_TABLE(uss_pm_t, F);
yfs_set_master(mds, []) ->
    F = fun(#uss_pm_t{} = Old) -> Old#uss_pm_t{mds_info=false} end,
    ?TRANSFORM_TABLE(uss_pm_t, F);
%
yfs_set_master(c60, [H|_]=L) when is_integer(H) ->
    F = fun(#uss_pm_t{id=Id} = Old) ->
            case lists:member(Id, L) of
                true  -> Old#uss_pm_t{c60_info=true};
                false -> Old#uss_pm_t{c60_info=false}
            end
    end,
    ?TRANSFORM_TABLE(uss_pm_t, F);
yfs_set_master(mds, [H|_]=L) when is_integer(H) ->
    F = fun(#uss_pm_t{id=Id} = Old) ->
            case lists:member(Id, L) of
                true  -> Old#uss_pm_t{mds_info=true};
                false -> Old#uss_pm_t{mds_info=false}
            end
    end,
    ?TRANSFORM_TABLE(uss_pm_t, F);
%
yfs_set_master(c60, L) ->
    L1 = [Ip||{Ip,c60,_} <- L],
    F = fun(#uss_pm_t{ip=Ip} = Old) ->
            case lists:member(Ip, L1) of
                true  -> Old#uss_pm_t{c60_info=true};
                false -> Old#uss_pm_t{c60_info=false}
            end
    end,
    ?TRANSFORM_TABLE(uss_pm_t, F);
yfs_set_master(mds, L) ->
    L1 = [Ip||{Ip,mds,_} <- L],
    F = fun(#uss_pm_t{ip=Ip} = Old) ->
            case lists:member(Ip, L1) of
                true  -> Old#uss_pm_t{mds_info=true};
                false -> Old#uss_pm_t{mds_info=false}
            end
    end,
    ?TRANSFORM_TABLE(uss_pm_t, F).

yfs_select(Ip) when is_list(Ip) ->
    yfs_select(list_to_binary(Ip));
yfs_select(Ip) when is_binary(Ip) ->
    Pattern = #uss_yfs_t{sid={Ip, '_', '_'}, _ = '_'},
    p_select(Pattern);
yfs_select(Type) when is_atom(Type) ->
    Pattern = #uss_yfs_t{sid={'_', Type, '_'}, _ = '_'},
    p_select(Pattern);
yfs_select({Ip, Type}) when is_binary(Ip), is_atom(Type) ->
    Pattern = #uss_yfs_t{sid={Ip, Type, '_'}, _ = '_'},
    p_select(Pattern).

yfs_update_status(Ip, Status) ->
    case yfs_select(Ip) of
        {ok, L} ->
            F = fun(R) ->
                p_transaction(fun() -> mnesia:write(R#uss_yfs_t{status=Status}) end)
            end,
            lists:foreach(F, L);
        {error, Reason} ->
            {error, Reason}
    end.

yfs_update(#uss_yfs_t{sid=Key, status=_NewStatus, pid=_NewPid} = New) ->
    Ip = element(1, Key),
    case is_node_exist(Ip) of
        true ->
            case is_valid_service(Key) of
                true ->
                    yfs_update2(New);
                false ->
                    % TODO
                    % F = fun() -> mnesia:write(New) end,
                    % p_transaction(F)
                    {error, {invalid_service, New}}
            end;
        false ->
            ok
    end;
yfs_update({Node, Services}) when is_list(Services) ->
    Ip = cclib_utils:get_ip(Node),
    case yfs_select(Ip) of
        {ok, OldServices} ->
            {A, I, B} = p_handle_yfs_update(OldServices, Services),
            lists:foreach(fun yfs_delete/1, A),
            lists:foreach(fun yfs_update/1, I ++ B);
        {error, Reason} ->
            {error, Reason}
    end.

yfs_update2(#uss_yfs_t{sid=Key, status=NewStatus, pid=_NewPid} = New) ->
    Flag = cluster_getflag(),
    F = fun() ->
        case mnesia:wread({uss_yfs_t, Key}) of
            [#uss_yfs_t{status=_OldStatus, pid=_OldPid} = _Old] ->
                if
                    Flag =:= true andalso NewStatus =:= shutoff ->
                        %?EVENT(#event_service{
                        %        ip=element(1, Key),
                        %        type=element(2, Key),
                        %        n=element(3, Key),
                        %        event=yfs_need_start,
                        %        cluster='on',
                        %        prev_status=OldStatus,
                        %        status=NewStatus,
                        %        info=NewPid
                        %    }),
                        %yfs_start(Key),
                        ok;
                    Flag =:= false andalso NewStatus =/= shutoff ->
                        %?EVENT(#event_service{
                        %        ip=element(1, Key),
                        %        type=element(2, Key),
                        %        n=element(3, Key),
                        %        event=yfs_need_stop,
                        %        cluster='off',
                        %        prev_status=OldStatus,
                        %        status=NewStatus,
                        %        info=NewPid
                        %    }),
                        %yfs_stop(Key),
                        ok;
                    true ->
                        ok
                end;
            [] ->
                ok
        end,
        mnesia:write(New)
    end,
    p_transaction(F).

p_handle_yfs_update(OldServices, NewServices) ->
    N = 2,
    I = p_lists_intersection(NewServices, OldServices, N),
    A = [Tuple || Tuple <- OldServices, not lists:keymember(element(N, Tuple), N, I) ],
    B = [Tuple || Tuple <- NewServices, not lists:keymember(element(N, Tuple), N, I) ],
    {A, I, B}.

p_lists_intersection(TupleList1, TupleList2, N) ->
    p_lists_intersection(TupleList1, TupleList2, N, []).

p_lists_intersection([], _TupleList, _N, Acc) ->
    lists:reverse(Acc);
p_lists_intersection([H|T], TupleList, N, Acc) ->
    Key = element(N, H),
    case lists:keymember(Key, N, TupleList) of
        true  -> p_lists_intersection(T, TupleList, N, [H|Acc]);
        false -> p_lists_intersection(T, TupleList, N, Acc)
    end.

% ----------------------------------------------------
-spec yfs_services(all | service_type(), boolean()) -> list().
yfs_services(Type, IpOnly) ->
    NewProc = case Type of all -> '_'; _ -> Type end,
    Pattern = #uss_yfs_t{sid={'_', NewProc, '_'}, _ = '_'},
    case p_select(Pattern) of
        {ok, L} ->
            case IpOnly of
                true  -> lists:usort([Ip || #uss_yfs_t{sid={Ip, _, _}} <- L]);
                false -> L
            end;
        {error, _} -> []
    end.

yfs_all_services() ->
    yfs_services(all, false).

% ----------------------------------------------------
yfs_load_c60() ->
    {ok, N} = option_get(opt_c60_num, ?MAX_C60_NUM),
    yfs_load_or_select_masters(c60, N).

yfs_load_mds() ->
    {ok, N} = option_get(opt_mds_num, ?MAX_MDS_NUM),
    yfs_load_or_select_masters(mds, N).

% c60 and mds cannot be on the same machine
yfs_load_or_select_masters(Type, N) when is_atom(Type) ->
    case yfs_load_masters(Type, N) of
        [] -> yfs_select_masters(Type, N);
        L -> L
    end.

yfs_load_masters(Type, N) ->
    case yfs_get_master(Type) of
        [] -> [];
        L ->
            if
                N > 0 -> lists:sublist(L, N);
                true -> L
            end
    end.

yfs_select_masters(Type, N) ->
    case yfs_services(Type, false) of
        [] -> [];
        L when is_list(L) ->
            L2 = get_services_on_diff_nodes([Sid||#uss_yfs_t{sid=Sid}<-L], N),
            yfs_set_master(Type, L2),
            L2
    end.

get_services_on_diff_nodes(L, N) when is_integer(N) ->
    L2 = p_on_diff_nodes(L, dict:new()),
    lists:sublist(L2, N).

p_on_diff_nodes([], D) ->
    L = [{Ip, lists:sort(X)} || {Ip, X} <- dict:to_list(D)],
    [{Ip, Type, N} || {Ip, [{Type, N}|_]} <- L];
p_on_diff_nodes([{Ip, Type, N}|T], D) ->
    D2 = case dict:find(Ip, D) of
        {ok, Value} ->
            dict:store(Ip, [{Type, N}|Value], D);
        error ->
            dict:store(Ip, [{Type, N}], D)
    end,
    p_on_diff_nodes(T, D2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% step 1: deployment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_service_list(Type) ->
    [X || #uss_yfs_t{sid=X} <- yfs_services(Type, false)].

collect_all_services(NodeList) ->
    Lc60 = yfs_load_c60(),
    Lmds = yfs_load_mds(),
    Lcds = [X || #uss_yfs_t{sid=X} <- yfs_services(cds, false)],

    % TODO
    Lrjnld = get_service_list(rjnld),
    Lnfs   = get_service_list(nfs),
    Lproxy = get_service_list(proxy),

    L1 = Lc60 ++ Lmds ++ Lrjnld ++ Lcds ++ Lproxy ++ Lnfs,
    L2 = [{Ip, Type, N} || {Ip, Type, N} <- L1, lists:member(Ip, NodeList)],
    {ok, L2}.

update_etc(NodeList, Lservices) ->
    Lc60   = [X||X<-Lservices, is_type(c60, X)],
    Lmds   = [X||X<-Lservices, is_type(mds, X)],
    Lcds   = [X||X<-Lservices, is_type(cds, X)],
    Lnfs   = [X||X<-Lservices, is_type(nfs, X)],
    Lproxy = [X||X<-Lservices, is_type(proxy, X)],

    Nc60 = length(Lc60),
    Nmds = length(Lmds),
    Ncds = length(Lcds),
    Nclient = length(Lnfs) + length(Lproxy),

    option_set(c60_count, Nc60),
    option_set(mds_count, Nmds),
    option_set(cds_count, Ncds),
    option_set(client_count, Nclient),

    uss_pm:update_etc(NodeList, Lc60, Lmds, [{c60, Nc60},
            {mds, Nmds}, {cds, Ncds}, {client, Nclient}]),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% step 2: check
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
yfs_check_start_pre() ->
    Tests = [
            fun yfs_check_c60/0,
            fun yfs_check_mds/0,
            fun yfs_check_cluster_status/0,
            fun yfs_check_every_node/0
        ],
    Results = [X()||X <- Tests],
    Errors = [X|| {error, X} <- Results],
    case Errors of
        [] ->
            {ok, []};
        _ ->
            {error, lists:flatten(Errors)}
    end.

yfs_check_start_pre([]) ->
    {ok, true};
yfs_check_start_pre([H|T]) ->
    case H() of
        {error, Reason} ->
            {error, Reason};
        {ok, _} ->
            yfs_check_start_pre(T)
    end.

% c60/mds selected
yfs_check_c60() ->
    case yfs_check(c60) of
        {error, ErrL} ->
            {error, [{c60_error, ErrL}]};
        Res ->
            Res
    end.

yfs_check_mds() ->
    case yfs_check(mds) of
        {error, ErrL} ->
            {error, [{mds_error, ErrL}]};
        Res ->
            Res
    end.

yfs_check(c60) ->
    case yfs_get_master(c60) of
        [] ->
            {error, no_c60};
        L ->
            is_valid_nodes([Ip||{Ip,_,_}<-L])
    end;
yfs_check(mds) ->
    case yfs_get_master(mds) of
        [] ->
            {error, no_mds};
        L ->
            is_valid_nodes([Ip||{Ip,_,_}<-L])
    end.

% every node in cluster must be in 'alive' status.
yfs_check_cluster_status() ->
    NodeList = all_nodes_in_cluster(),
    {ok, L} = uss_pm:cluster_status(NodeList),
    case [Ip|| {Ip, Status} <- L, Status =/= alive] of
        [] ->
            {ok, NodeList};
        ErrL ->
            {error, [{unavailable_nodes, ErrL}]}
    end.

yfs_check_every_node() ->
    NodeList = all_nodes_in_cluster(),
    case NodeList of
        [] ->
            {error, no_nodes};
        _  ->
            case uss_pm:check_all(NodeList) of
                {error, ErrL} ->
                    {error, [{yfs_check_every_node, ErrL}]};
                Res ->
                    Res
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% step 3: start
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec yfs_start('all'|service_type()|service_id()|#uss_yfs_t{}) -> any().
yfs_start(all) ->
    case cluster_getflag() of
        true ->
            {error, already_started};
        false ->
            case ensure_all_services_in_status(stopped) of
                false ->
                    {error, not_all_services_stopped};
                true ->
                    yfs_load_c60(),
                    yfs_load_mds(),
                    case cluster_check() of
                        {error, Reason} ->
                            {error, Reason};
                        {ok, _} ->
                            % TODO
                            cluster_setflag(true),
                            spawn(fun() -> yfs_start_all() end),
                            {ok, processing}
                    end
            end
    end;
yfs_start(#uss_yfs_t{sid=Sid}) ->
    yfs_start(Sid);
yfs_start({_IP, _Type, _N} = Sid) ->
    F = fun() ->
        case mnesia:read(uss_yfs_t, Sid) of
            [#uss_yfs_t{} = Yfs] -> Yfs;
            [] -> mnesia:abort(no_such_server)
        end
    end,
    case p_transaction(F) of
        {ok, _} -> yfs_start_1(Sid);
        {error, _Reason} -> {error, _Reason}
    end;
yfs_start(Type) when is_atom(Type) ->
    case yfs_services(Type, false) of
        false -> {error, Type};
        RowL ->
            lists:foreach(fun(Row) -> yfs_start_1(Row#uss_yfs_t.sid) end, RowL),
            {ok, RowL}
    end;
yfs_start(Ip) when is_binary(Ip) ->
    case yfs_select(Ip) of
        {ok, RowL} when length(RowL) > 0 ->
            L = [Sid || #uss_yfs_t{sid=Sid} <- RowL, is_valid_service(Sid)],
            lists:foreach(fun(Sid) -> yfs_start_1(Sid) end, L),
            {ok, L};
        _ ->
            {error, Ip}
    end.

ensure_all_services_in_status(Status) ->
    NodeList = all_nodes_in_cluster(),
    {ok, L} = collect_all_services(NodeList),
    lists:all(fun(X) -> is_in_status(X, Status) end, L).

yfs_start_all() ->
    NodeList = all_nodes_in_cluster(),
    {ok, L} = collect_all_services(NodeList),
    update_etc(NodeList, L),
    lists:foreach(fun yfs_start/1, L).

yfs_start_1({_IP, _Type, _N} = Sid) ->
    case uss_yfs:start(Sid) of
        ok ->
            {ok, starting};
        {ok, Pid} when is_pid(Pid) ->
            {ok, Pid};
        {ok, Result} ->
            {ok, Result};
        {error, _Reason} ->
            {error, _Reason}
    end.

%%----------------------------------------------------------------
%% yfs_stop
%%----------------------------------------------------------------
yfs_stop(all) ->
    mnesia:clear_table(uss_queue_t),
    case cluster_getflag() of
        false ->
            Services = yfs_all_services(),
            F = fun() -> yfs_stop_stage(Services) end,
            spawn(F),
            {ok, already_stoped};
        true ->
            cluster_setflag(false),
            Services = yfs_all_services(),
            F = fun() -> yfs_stop_stage(Services) end,
            spawn(F),
            {ok, stopping}
    end;
yfs_stop(#uss_yfs_t{sid=Sid}) ->
    yfs_stop(Sid, false);
yfs_stop({_Ip, _Type, _N} = Sid) ->
    yfs_stop(Sid, false);
yfs_stop(Type) when is_atom(Type) ->
    case yfs_services(Type, false) of
        false -> false;
        RowL -> lists:foreach(fun(Row) -> yfs_stop(Row#uss_yfs_t.sid) end, RowL)
    end;
yfs_stop(Ip) when is_binary(Ip) ->
    case yfs_select(Ip) of
        {ok, RowL} when length(RowL) > 0 ->
            L = [Sid || #uss_yfs_t{sid=Sid} <- RowL, is_valid_service(Sid)],
            lists:foreach(fun(Sid) -> yfs_stop(Sid) end, L),
            {ok, L};
        _ ->
            {error, Ip}
    end.

yfs_stop(#uss_yfs_t{sid=Sid}, Force) ->
    yfs_stop(Sid, Force);
yfs_stop({_Ip, _Type, _N} = Sid, Force) ->
    case uss_yfs:stop(Sid, Force) of
        ok ->
            {ok, stopping};
        {ok, Pid} when is_pid(Pid) ->
            {ok, Pid};
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    end.

-define(TO_WAIT,       30).
-define(TO_CDS_WAIT,   300).

% FIXME first stop all FAS instances,
% and then stop c60/mds/cds.
yfs_stop_stage(Services) ->
    {ok, Wait}    = clib_utils:get_env(yfs_stop_wait, ?TO_WAIT),
    {ok, CdsWait} = clib_utils:get_env(yfs_stop_cds_wait, ?TO_CDS_WAIT),

    yfs_stop_type([X||X<-Services, uss_yfs_service:is_fas(X)], Wait),
    yfs_stop_type([X||X<-Services, is_type(cds,X)], CdsWait),
    yfs_stop_type([X||X<-Services, is_type(rjnld,X)], Wait),
    yfs_stop_type([X||X<-Services, is_type(mds,X)], Wait),
    yfs_stop_type([X||X<-Services, is_type(c60,X)], Wait),

    ok.

% 2s
yfs_stop_type(L, Timeout) ->
    Force = if
        Timeout > 0 ->
            false;
        true ->
            ?WARN_REPORT({upgrade_to_force, L}),
            true
    end,
    F = fun(X) -> yfs_stop(X, Force) end,
    lists:foreach(F, L),
    Step = 3,
    timer:sleep(Step*1000),
    case [X|| X<-L, not is_stopped(X)] of
        [] ->
            ok;
        NotShutOffL ->
            yfs_stop_type(NotShutOffL, Timeout - Step)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% uss_option_t
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option_init() ->
    %% common options
    % option_from_env(root_dir),
    % option_from_env(managers),

    %% for master only
    option_from_env(ws_ip),
    option_from_env(ws_port),
    option_from_env(manager_web_port),
    option_from_env(manager_timeout),
    option_from_env(manager_nodedown_timeout),

    %% for agent only
    option_from_env(agent_timeout),

    option_set_if_not_exists(opt_c60_num, ?MAX_C60_NUM),
    option_set_if_not_exists(opt_mds_num, ?MAX_MDS_NUM),
    option_set_if_not_exists(opt_uss_status, 'off'),

    %option_from_env(manager_trace_port),
    %option_from_env(agent_trace_port),

    ok.

option_set(Key, Value) ->
    Row = #uss_option_t{key=Key, value=Value},
    F = fun() -> mnesia:write(Row) end,
    case p_transaction(F) of
        {ok, Res} -> {ok, Res};
        {error, Reason} -> {error, Reason}
    end.

option_set_if_not_exists(Key, Value) ->
    Row = #uss_option_t{key=Key, value=Value},
    F = fun() ->
            case mnesia:wread({uss_option_t, Key}) of
                [] ->
                    mnesia:write(Row);
                _ ->
                    mnesia:abort(exists)
            end
        end,
    case p_transaction(F) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

option_get(Key) ->
    F = fun() -> mnesia:read({uss_option_t, Key}) end,
    case p_transaction(F) of
        {ok, [#uss_option_t{value=Value}]} -> {ok, Value};
        {ok, []} -> false
    end.

option_get(Key, Default) ->
    F = fun() -> mnesia:read({uss_option_t, Key}) end,
    case p_transaction(F) of
        {ok, [#uss_option_t{value=Value}]} -> {ok, Value};
        _ -> {ok, Default}
    end.

option_increment(Key, _N) ->
    F = fun() ->
            CurRow = case mnesia:wread({uss_option_t, Key}) of
                [Row] -> Row;
                [] -> #uss_option_t{key=Key, value=1}
            end,
            mnesia:write(CurRow#uss_option_t{value=CurRow#uss_option_t.value+1}),
            CurRow
    end,
    case mnesia:transaction(F) of
        {atomic, #uss_option_t{value=Value}} -> Value
    end.

option_increment(Key) -> option_increment(Key, 1).

option_del(Key) ->
    F = fun() -> mnesia:delete({uss_option_t, Key}) end,
    p_transaction(F).

option_from_env(Key) ->
    case application:get_env(?APPLICATION, Key) of
        {ok, Value} -> option_set(Key, Value)
    end.

option_from_env(Key, Default) ->
    case application:get_env(?APPLICATION, Key) of
        {ok, Value} -> option_set(Key, Value);
        _ -> option_set(Key, Default)
    end.

%% ----------------------------------------------------------------
%% uss_queue_t
%% ----------------------------------------------------------------
queue_push(#uss_queue_t{et=Et, event=Event} = E) ->
    case {Et, Event} of
        {'service', 'start'} ->
            case cluster_getflag() of
                true -> p_queue_push(E);
                false -> ok
            end;
        _ ->
            ok
    end.

p_queue_push(#uss_queue_t{} = E) ->
    Id = option_increment(private_queue_counter),
    F = fun() -> mnesia:write(E#uss_queue_t{id=Id}) end,
    p_transaction(F).

p_queue_pop(Id) ->
    F = fun() -> mnesia:delete({uss_queue_t, Id}) end,
    p_transaction(F).

queue_run() ->
    case cluster_getflag() of
        true ->
            Pattern = #uss_queue_t{_='_'},
            case p_select(Pattern) of
                {ok, L} ->
                    lists:foreach(fun(E) -> do_queue(E) end, L);
                _ -> ok
            end;
        false ->
            mnesia:clear_table(uss_queue_t)
    end.

do_queue(#uss_queue_t{id=Id, et='service', event='start', info=Info}) ->
    case yfs_start(Info) of
        {ok, _} -> p_queue_pop(Id);
        {error, _} -> ok
    end.

%% ----------------------------------------------------------------
%% User Management
%% ----------------------------------------------------------------
role_add() ->
    ok.

role_delete() ->
    ok.

role_update() ->
    ok.

user_add() ->
    ok.

user_delete() ->
    ok.

user_update() ->
    ok.

user_assign() ->
    ok.

%% ----------------------------------------------------------------
%% Cluster Member
%% ----------------------------------------------------------------
do_agent_join(Id) when is_integer(Id) ->
    ?ALERT_INFO(alert, agent_join, io_lib:format("~B joined", [Id]));
do_agent_join(Ip) ->
    ?ALERT_INFO(alert, agent_join, io_lib:format("~s joined", [Ip])).

do_agent_leave(Node, InfoList) when is_atom(Node) ->
    Ip = cclib_utils:get_ip(Node),
    NodedownReason = proplists:get_value(nodedown_reason, InfoList),
    ?ALERT_WARN(alert, agent_leave, io_lib:format("~s leaved, reason: ~p",
            [Ip, NodedownReason])),
    pm_update_status(Ip, unavailable),

    %% FIXME
    {ok, Timeout} = option_get(opt_nodedown_timeout, 10),
    case cclib_nodes:node_ping(Node, Timeout) of
        true ->
            pm_update_status(Ip, running),
            ok;
        false ->
            ?ALERT_WARN(alert, agent_leave,
                io_lib:format("~s leaved, cannot connect", [Ip])),
            {error, cannot_connect}
    end.

% TODO
%   managers: ['ussadmin_master@xxx']
%
% limit: list
% @retval [tuplelist]
select(#sql_query{table=Table, fields=_What, where=Where, limit=Limit, orderby=_Orderby}) ->
    Result =
    case Table of
        uss_cluster_t ->
            {ok, Uss} = option_get(opt_uss_status, 'off'),
            {ok, [{id, ?CLUSTER_0}, {status, Uss}]};
        uss_option_t ->
            Pattern = #uss_option_t{_='_'},
            case p_select(Pattern) of
                {ok, L} when is_list(L) ->
                    {ok, [uss_json:eterm_to_json(R) || R <- L]};
                {error, _} ->
                    {error, []}
            end;
        uss_rack_t ->
            Pattern =
            case proplists:get_value(<<"id">>, Where) of
                Id when is_integer(Id) -> #uss_rack_t{id=Id, _='_'};
                _ -> #uss_rack_t{_='_'}
            end,
            case p_select(Pattern) of
                {ok, L} when is_list(L) ->
                    {ok, [uss_json:eterm_to_json(R) || R <- L]};
                {error, _} ->
                    {error, []}
            end;
        uss_pm_t ->
            % TODO
            Cl = proplists:get_value(<<"cluster">>, Where),
            Id = proplists:get_value(<<"id">>, Where),
            Ip = proplists:get_value(<<"ip">>, Where),
            Pattern = if
                is_integer(Cl) -> #uss_pm_t{cluster=Cl,_='_'};
                is_integer(Id) -> #uss_pm_t{id=Id,_='_'};
                is_binary(Ip)  -> #uss_pm_t{ip=Ip,_='_'};
                true -> #uss_pm_t{_='_'}
            end,
            case p_select(Pattern) of
                {ok, L} ->
                    L2 = [uss_types:pm_to_node(R) || R <- L],
                    {ok, [uss_json:eterm_to_json(R) || R <- L2]};
                {error, _} ->
                    {error, []}
            end;
        uss_yfs_t ->
            Ip   = proplists:get_value(<<"ip">>, Where),
            Type = proplists:get_value(<<"type">>, Where),
            Pattern =
            case {Ip, Type} of
                {undefined, undefined} ->
                    #uss_yfs_t{_='_'};
                {undefined, _} ->
                    #uss_yfs_t{sid={'_',cclib_utils:to_atom(Type),'_'}, _='_'};
                {_, undefined} ->
                    #uss_yfs_t{sid={Ip,'_','_'}, _='_'};
                {_,_} ->
                    #uss_yfs_t{sid={Ip,cclib_utils:to_atom(Type),'_'}, _='_'}
            end,
            case p_select(Pattern) of
                {ok, L} ->
                    L2 = [uss_types:uss_yfs_to_service(R) || R <- yfs_filter(L)],
                    {ok, [uss_json:eterm_to_json(R) || R <- L2]};
                {error, _} ->
                    {error, []}
            end
    end,
    case Result of
        {ok, LN0} ->
            {ok, do_limit(Limit, LN0)};
        {error, Reason} ->
            {error, Reason}
    end.

yfs_filter(L) ->
    F = fun(#uss_yfs_t{sid=Sid}) ->
            Ip = element(1, Sid),
            case is_valid_node(Ip) of
                true -> is_valid_service(Sid);
                false -> false
            end
    end,
    lists:filter(F, L).

do_limit(Limit, L) ->
    Offset = case proplists:get_value(<<"offset">>, Limit, 1) of
        V when V < 1 -> 1;
        V -> V
    end,
    Count  = proplists:get_value(<<"count">>, Limit, 0),
    Len    = length(L),
    if
        Count < 1 -> lists:sublist(L, Offset, Len-Offset+1);
        true -> lists:sublist(L, Offset, Count)
    end.

%% ----------------------------------------------------------------
%% @deprecated Please use the module {@link edog_vm} instead.
%% ----------------------------------------------------------------
%% -----------------------------------------------------------
%% PRIVATE
%% -----------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_valid_service({_Ip, c60, _N} = Sid) ->
    L = yfs_get_master(c60),
    lists:member(Sid, L);
is_valid_service({_Ip, mds, _N} = Sid) ->
    L = yfs_get_master(mds),
    lists:member(Sid, L);
is_valid_service({_Ip, nfs, _N} = _Sid) ->
    case clib_utils:get_env(enable_nfs, true) of
        {ok, true} -> true;
        _ -> false
    end;
is_valid_service({_Ip, proxy, _N} = _Sid) ->
    case clib_utils:get_env(enable_proxy, true) of
        {ok, true} -> true;
        _ -> false
    end;
is_valid_service(_Sid) ->
    true.

is_type(Type, #uss_yfs_t{sid={_Ip,Type,_N}}) -> true;
is_type(Type, {_Ip,Type,_N}) -> true;
is_type(_, _) -> false.

is_in_status(#uss_yfs_t{sid=Sid}, Status) ->
    is_in_status(Sid, Status);
is_in_status(Sid, Status) ->
    try
        case uss_yfs:get_status(Sid) of
            {ok, Status} -> true;
            _ -> false
        end
    catch
        _:_ ->
            true
    end.

is_stopped(#uss_yfs_t{sid=Sid}) ->
    is_stopped(Sid);
is_stopped(Sid) ->
    is_in_status(Sid, stopped).

is_node_exist(Ip) ->
    Pattern = #uss_pm_t{ip=Ip, _='_'},
    case p_select(Pattern) of
        {ok, []} -> false;
        {ok, [#uss_pm_t{}]} -> true
    end.

is_valid_node(Ip) ->
    Pattern = #uss_pm_t{ip=Ip, cluster=?CLUSTER_0, _='_'},
    case p_select(Pattern) of
        {ok, []} -> false;
        {ok, [#uss_pm_t{}]} -> true
    end.

is_valid_nodes(L) ->
    case [X||X<-L, not is_valid_node(X)] of
        [] -> {ok, ready};
        ErrL -> {error, ErrL}
    end.

ip_to_sid(c60, Ip) -> {Ip, c60, 0};
ip_to_sid(mds, Ip) -> {Ip, mds, 1};
ip_to_sid(nfs, Ip) -> {Ip, nfs, 0};
ip_to_sid(proxy, Ip) -> {Ip, proxy, 0}.

idlist_to_iplist(IdList) ->
    F = fun(Id) ->
        {ok, [#uss_pm_t{ip=Ip}]} = pm_info(Id),
        Ip
    end,
    lists:map(F, lists:usort(IdList)).

set_list(c60, IdList) ->
    L = idlist_to_iplist(IdList),
    set_c60(L);
set_list(mds, IdList) ->
    L = idlist_to_iplist(IdList),
    L1 = [{Ip, 1}||Ip<-L],
    set_mds(L1).

set_c60(L) ->
    L1 = [{cclib_utils:to_binary(Ip), c60, 0} || Ip <- L],
    yfs_set_master(c60, L1).

set_mds(L) ->
    L1 = [{cclib_utils:to_binary(Ip), mds, N} || {Ip, N} <- L],
    yfs_set_master(mds, L1).

get_conf() ->
    [].

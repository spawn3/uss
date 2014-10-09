-module(sql).
-export([
        insert/2,
        update/2,
        delete/2,
        select/2,
        moni/2,
        syst/2
    ]).
-compile(export_all).

-include("uss_common.hrl").

% get_value
gv(Key, L) -> proplists:get_value(Key, L).

%"{\"what\": {\"foo\": \"foo\", \"name\": \"rack1\"}, \"who\": [\"rack\"]}",
%{struct,
%[{<<"what">>,
%  {struct,
%   [{<<"foo">>, <<"foo">>},
%    {<<"name">>, <<"rack1">>}]}},
% {<<"who">>,[<<"rack">>]}]}

-define(JSON_REQUEST_HANDLER(Env, Input, Handler),
    try
        case proplists:get_value(request_method, Env) of
            "POST" ->
                {struct, Query} = cclib_mochijson2:decode(list_to_binary(Input)),
                Result = Handler(Query),
                uss_json:return(Result)
        end
    catch
        Error:Exception ->
            % TODO
            ?ERROR_REPORT({Input, Handler, Error, Exception, erlang:get_stacktrace()}),
            uss_json:return({Error, Exception})
    end).

insert(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_insert).
select(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_select).
delete(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_delete).
update(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_update).
moni(Env, Input)   -> ?JSON_REQUEST_HANDLER(Env, Input, do_moni).
syst(Env, Input)   -> ?JSON_REQUEST_HANDLER(Env, Input, do_syst).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @see uss_json:encode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_insert(Query) ->
    case uss_json:decode(Query) of
        #sql_query{table=uss_rack_t, fields=L} ->
            Name = gv(<<"name">>, L),
            case uss_mnesia:rack_add(Name) of
                {ok, Id} when is_integer(Id) -> {ok, [{id, Id}]};
                {error, Reason} -> {error, Reason}
            end;
        #sql_query{table=uss_pm_t, fields=L} ->
            Rack     = gv(<<"rack">>, L),
            Ip       = gv(<<"ip">>, L),
            Hostname = gv(<<"hostname">>, L),
            User     = gv(<<"user">>, L),
            Passwd   = gv(<<"passwd">>, L),
            case uss_mnesia:pm_add({Rack, Ip, Hostname, User, Passwd}) of
                {ok, Id} when is_integer(Id) -> {ok, [{id, Id}]};
                {error, Reason} -> {error, Reason}
            end
    end.

do_select(Query) ->
    uss_mnesia:select(uss_json:decode(Query)).

do_delete(Query) ->
    case uss_json:decode(Query) of
        #sql_query{table=uss_rack_t, where=L} ->
            Id = gv(<<"id">>, L),
            case uss_mnesia:rack_delete(Id) of
                {ok, _} -> {ok, [{id, Id}]};
                {error, Reason} -> {error, Reason}
            end;
        #sql_query{table=uss_pm_t, where=L} ->
            Id = gv(<<"id">>, L),
            case uss_mnesia:pm_delete(Id) of
                {ok, _} -> {ok, [{id, Id}]};
                {error, Reason} -> {error, Reason}
            end
    end.

do_update(Query) ->
    case uss_json:decode(Query) of
        #sql_query{table=uss_rack_t, fields=L} ->
            Request = #rack_request{
                id   = gv(<<"id">>, L),
                name = gv(<<"name">>, L)
            },
            case uss_mnesia:rack_update_sql(Request) of
                {ok, Id} when is_integer(Id) -> {ok, [{id, Id}]};
                {error, Reason} -> {error, Reason}
            end;
        #sql_query{table=uss_pm_t, fields=L} ->
            Request = #node_request{
                id       = gv(<<"id">>, L),
                rack     = gv(<<"rack">>, L),
                ip       = gv(<<"ip">>, L),
                hostname = gv(<<"hostname">>, L),
                user     = gv(<<"user">>, L),
                password = gv(<<"passwd">>, L)
            },
            case uss_mnesia:pm_update_sql(Request) of
                {ok, Id} when is_integer(Id) -> {ok, [{id, Id}]};
                {error, Reason} -> {error, Reason}
            end
    end.

% TODO
% [{<<"id">>, Id}, {<<"offset">>, Offset}, {<<"count">>, Count}]
do_moni(Query) ->
    Fields1 = [{<<"id">>, int}],
    L1 = uss_json:decode_fields(Query, Fields1),

    Fields2 = [{<<"offset">>, int}, {<<"count">>, int}],
    L2 = uss_json:decode_fields(Query, <<"limit">>, Fields2),

    case uss_manager:pm_info(L1 ++ L2) of
        {ok, RL} when is_list(RL) ->
            {ok, [uss_json:eterm_to_json(R) || R <- RL]};
        {error, Reason} ->
            {error, Reason}
    end.

do_syst(Query) ->
    Action = gv(<<"action">>, Query),
    case Action of
        %% ---------------------------------------
        <<"lvm_create">> ->
            do_lvm_create(Query);
        <<"lvm_resize">> ->
            do_lvm_resize(Query);
        <<"lvm_list">> ->
            do_lvm_list(Query);
        %% ---------------------------------------
        <<"get_log">> ->
            do_get_log(Query);
        <<"clean_log">> ->
            uss_manager:yfs_clean_log();
        <<"dump_log">> ->
            uss_manager:yfs_dump_log();
        <<"get_conf">> ->
            do_get_conf(Query);
        %% ---------------------------------------
        <<"node_join">> ->
            do_node(Query, ?CLUSTER_0);
        <<"node_exit">> ->
            do_node(Query, ?CLUSTER_NULL);
        <<"set_mds_c60">> ->
            do_set(c60, Query),
            do_set(mds, Query);
        <<"deploy">> ->
            uss_manager:cluster_deploy();
        <<"test">> ->
            do_cluster_check();
        <<"start">> ->
            % @retval {ok, processing}|{error, alread_started}|{error, Reason}
            do_start(Query);
        <<"stop">> ->
            do_stop(Query)
        %% ---------------------------------------
    end.

%% ----------------------------------------------------
do_node(Query, Flags) ->
    Fid = [{<<"id">>, int}],
    case uss_json:decode_fields(Query, Fid) of
        [{<<"id">>, Id}] when is_integer(Id) ->
            case uss_mnesia:pm_update_cluster(Id, Flags) of
                {ok, _} ->
                    {ok, [{id, Id}]};
                Other ->
                    Other
            end;
        _ ->
            {error, badarg}
    end.

do_set(c60, Query) ->
    case gv(<<"c60">>, Query) of
        undefined -> {error, badarg};
        L when is_list(L) -> uss_mnesia:set_list2(c60, L);
        Other -> {error, Other}
    end;
do_set(mds, Query) ->
    case gv(<<"mds">>, Query) of
        undefined -> {error, badarg};
        L when is_list(L) -> uss_mnesia:set_list2(mds, L);
        Other -> {error, Other}
    end.

%
do_get_log(Query) ->
    Ip   = gv(<<"ip">>, Query),
    Type = map_type(gv(<<"type">>, Query)),
    N    = cclib_utils:to_integer(gv(<<"n">>, Query)),

    % options
    Level = map_level(gv(<<"level">>, Query)),
    Lines = map_lines(gv(<<"lines">>, Query)),

    Options = [{lines, Lines}, {level, Level}],
    uss_manager:yfs_get_log({Ip, Type, N}, Options).

%map_type(<<"c60">>) -> c60;
%map_type(<<"mds">>) -> mds;
%map_type(<<"cds">>) -> cds;
%map_type(<<"nfs">>) -> nfs;
%map_type(<<"proxy">>) -> proxy;
map_type(B) when is_binary(B) ->
    cclib_utils:to_atom(erlang:binary_to_list(B)).

map_lines(Lines) when is_integer(Lines) ->
    if
        Lines > ?LOG_MAX_LINES -> ?LOG_MAX_LINES;
        Lines =< 0             -> ?LOG_DEFAULT_LINES;
        true                   -> Lines
    end;
map_lines(_) ->
    ?LOG_DEFAULT_LINES.

map_level(<<"error">>)   -> error;
map_level(<<"warning">>) -> warning;
map_level(<<"info">>)    -> info;
map_level(_)             -> all.

%
% @retval {ok, true} | {error, Reason}
do_cluster_check() ->
    uss_manager:cluster_check().

do_get_conf(_Query) ->
    uss_manager:get_conf().

do_start(Query) ->
    case query_to_sid(Query) of
        {undefined, _, _} ->
            uss_manager:yfs_start();
        {Ip , undefined, _} ->
            uss_mnesia:yfs_start(Ip);
        {Ip, Type, N} ->
            uss_mnesia:yfs_start({Ip, Type, N})
    end.

do_stop(Query) ->
    case query_to_sid(Query) of
        {undefined, _, _} ->
            uss_manager:yfs_stop();
        {Ip , undefined, _} ->
            uss_mnesia:yfs_stop(Ip);
        {Ip, Type, N} ->
            uss_mnesia:yfs_stop({Ip, Type, N})
    end.

query_to_sid(Query) ->
    {gv(<<"ip">>, Query), gv(<<"type">>, Query), gv(<<"n">>, Query)}.

% -----------------------------------------------------------------
-define(TJ_BLOCK, true).

-ifdef(TJ_BLOCK).

do_lvm_create(Query) ->
    Name = gv(<<"name">>, Query),
    Size = gv(<<"size">>, Query),
    case {Name, Size} of
        {_, _} when is_binary(Name), is_integer(Size) ->
            cclib_async:block_apply(uss_manager, lvm_create, [Name, Size], ?TIMEOUT_LVM);
        _ ->
            {error, badarg}
    end.

do_lvm_resize(Query) ->
    Name = gv(<<"name">>, Query),
    Size = gv(<<"size">>, Query),
    case {Name, Size} of
        {_, _} when is_binary(Name), is_integer(Size) ->
            cclib_async:block_apply(uss_manager, lvm_resize, [Name, Size], ?TIMEOUT_LVM);
        _ ->
            {error, badarg}
    end.

do_lvm_list(_Query) ->
    cclib_async:block_apply(uss_manager, lvm_list, [], ?TIMEOUT_LVM).

-else.

do_lvm_create(Query) ->
    Name = gv(<<"name">>, Query),
    Size = gv(<<"size">>, Query),
    case {Name, Size} of
        {_, _} when is_binary(Name), is_integer(Size) ->
            uss_manager:lvm_create(Name, Size, []);
        _ ->
            {error, badarg}
    end.

do_lvm_resize(Query) ->
    Name = gv(<<"name">>, Query),
    Size = gv(<<"size">>, Query),
    case {Name, Size} of
        {undefined, undefined} -> {error, badarg};
        {undefined, _} -> {error, badarg};
        {_, undefined} -> {error, badarg};
        _ -> uss_manager:lvm_resize(Name, Size, [])
    end.

do_lvm_list(Query) ->
    uss_manager:lvm_list([]).

-endif.


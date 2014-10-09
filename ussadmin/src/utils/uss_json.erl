-module(uss_json).
-export([
        gv/2,
        return/1,
        decode/1
    ]).
-compile(export_all).

-include("uss_common.hrl").

gv(Key, {struct, L}) when is_list(L) -> gv(Key, L);
gv(Key, L) when is_list(L) -> proplists:get_value(Key, L).

decode(Query) ->
    #sql_query{
        table   = decode_table(Query),
        fields  = decode_fields(Query, <<"what">>),
        where   = decode_fields(Query, <<"where">>),
        limit   = decode_fields(Query, <<"limit">>),
        orderby = gv(<<"orderby">>, Query)
    }.

decode_table(Query) ->
    Who = gv(<<"who">>, Query),
    case Who of
        <<"cluster">> -> uss_cluster_t;
        <<"rack">>    -> uss_rack_t;
        <<"node">>    -> uss_pm_t;
        <<"service">> -> uss_yfs_t;
        <<"option">>  -> uss_option_t;
        undefined     -> undefined
    end.

% <<"what">>/<<"where">>
decode_fields(Query, Head) when is_binary(Head) ->
    Fields = [
        {<<"id">>       ,int},
        % what & where
        {<<"ip">>       ,binary},
        {<<"type">>,     binary},
        {<<"name">>     ,binary},
        {<<"cluster">>  ,int},
        {<<"rack">>     ,binary},
        {<<"hostname">> ,binary},
        {<<"user">>     ,binary},
        {<<"passwd">>   ,binary},
        % cluster
        {<<"status">>   ,binary},
        % limit
        {<<"offset">>   ,int},
        {<<"count">>    ,int}
    ],
    decode_fields(Query, Head, Fields);
decode_fields(Query, Fields) when is_list(Fields) ->
    F = fun(K, Type) ->
            case gv(K, Query) of
                V when Type =:= int, is_binary(V) ->
                    list_to_integer(binary_to_list(V));
                %undefined when Type =:= int -> -1;
                V -> V
            end
    end,
    [{K, F(K, Type)} || {K, Type} <- Fields].

decode_fields(Query, Head, Fields) ->
    case gv(Head, Query) of
        {struct, Cols} ->
            decode_fields(Cols, Fields);
        Cols when is_list(Cols) ->
            decode_fields(Cols, Fields);
        <<"*">> -> [];
        undefined -> [];
        null -> []
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eterm_to_json(Term) when is_record(Term, uss_rack_t) ->
    ?record_to_json(fun eterm_to_json/1, uss_rack_t, Term);
eterm_to_json(Term) when is_record(Term, uss_pm_t) ->
    ?record_to_json(fun eterm_to_json/1, uss_pm_t, Term);
eterm_to_json(Term) when is_record(Term, uss_yfs_t) ->
    ?record_to_json(fun eterm_to_json/1, uss_yfs_t, Term);
eterm_to_json(Term) when is_record(Term, pm_info) ->
    ?record_to_json(fun eterm_to_json/1, pm_info, Term);
eterm_to_json(Term) when is_record(Term, cpu_info) ->
    ?record_to_json(fun eterm_to_json/1, cpu_info, Term);
eterm_to_json(Term) when is_record(Term, cpu_util) ->
    ?record_to_json(fun eterm_to_json/1, cpu_util, Term);
eterm_to_json(Term) when is_record(Term, disk_info) ->
    ?record_to_json(fun eterm_to_json/1, disk_info, Term);
eterm_to_json(Term) when is_record(Term, if_info) ->
    ?record_to_json(fun eterm_to_json/1, if_info, Term);
eterm_to_json(Term) when is_record(Term, uptime) ->
    ?record_to_json(fun eterm_to_json/1, uptime, Term);
eterm_to_json(Term) when is_record(Term, volume) ->
    ?record_to_json(fun eterm_to_json/1, volume, Term);
eterm_to_json(Term) when is_record(Term, async_event) ->
    ?record_to_json(fun eterm_to_json/1, async_event, Term);
eterm_to_json(Term) when is_record(Term, service) ->
    ?record_to_json(fun eterm_to_json/1, service, Term);
eterm_to_json(Term) when is_record(Term, node) ->
    ?record_to_json(fun eterm_to_json/1, node, Term);
eterm_to_json(Term) when is_record(Term, conf_item) ->
    ?record_to_json(fun eterm_to_json/1, conf_item, Term);

eterm_to_json([H|_] = Term) when is_list(Term) ->
    case is_my_record(H) of
        true ->
            case lists:all(fun is_my_record/1, Term) of
                true ->
                    [eterm_to_json(X) || X <- Term];
                false ->
                    cclib_json:default_eterm_to_json(fun eterm_to_json/1, Term)
            end;
        false ->
            cclib_json:default_eterm_to_json(fun eterm_to_json/1, Term)
    end;
%%%
eterm_to_json(Term) ->
    cclib_json:default_eterm_to_json(fun eterm_to_json/1, Term).

is_my_record(Term) when is_record(Term, uss_rack_t)  -> true;
is_my_record(Term) when is_record(Term, uss_pm_t)    -> true;
is_my_record(Term) when is_record(Term, uss_yfs_t)   -> true;
is_my_record(Term) when is_record(Term, pm_info)     -> true;
is_my_record(Term) when is_record(Term, cpu_info)    -> true;
is_my_record(Term) when is_record(Term, cpu_util)    -> true;
is_my_record(Term) when is_record(Term, disk_info)   -> true;
is_my_record(Term) when is_record(Term, if_info)     -> true;
is_my_record(Term) when is_record(Term, uptime)      -> true;
is_my_record(Term) when is_record(Term, volume)      -> true;
is_my_record(Term) when is_record(Term, async_event) -> true;
is_my_record(Term) when is_record(Term, service)     -> true;
is_my_record(Term) when is_record(Term, node)        -> true;
is_my_record(Term) when is_record(Term, conf_item)   -> true;
is_my_record(_)                                      -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Result is a JSON binary.
return(Result) ->
    Json = cclib_json:encode(normalize(Result)),
    [
        "Content-type: application/json\r\n\r\n",
        Json
    ].

normalize(ok) ->
    [{result, ok}];
normalize({ok, []}) ->
    [{result, empty}];
normalize({ok, Result}) when is_atom(Result) ->
    [{result, Result}];
normalize({ok, Result}) when is_binary(Result) ->
    [{result, Result}];
normalize({ok, Result}) when is_pid(Result) ->
    [{result, processing}];
normalize({ok, Result}) when is_list(Result) ->
    Result;
normalize({error, Result}) ->
    case cclib_utils:is_tuplelist(Result) of
        true ->
            [{error, Result}];
        false ->
            [{error, to_binary(Result)}]
    end.

to_binary(X) ->
    cclib_utils:to_binary(lists:flatten(to_string(X))).

to_string({Tag, L}) when is_atom(Tag), is_list(L) ->
    io_lib:format("{~s, [~s]}", [to_string(Tag), list_to_string(L, "")]);
to_string({Tag, X}) ->
    io_lib:format("{~s, ~s}", [to_string(Tag), to_string(X)]);
to_string(X) when is_atom(X) ->
    atom_to_list(X);
to_string(X) ->
    X.

list_to_string([], Acc) -> Acc;
list_to_string([H|T], Acc) ->
    NewAcc = case T of
        [] -> io_lib:format("~s~s",   [Acc, to_string(H)]);
        _  -> io_lib:format("~s~s, ", [Acc, to_string(H)])
    end,
    list_to_string(T, NewAcc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lists_update(TupleList, RefTupleList) ->
    F = fun({K, V}) ->
        case lists:keyfind(K, 1, RefTupleList) of
            false -> {K, V};
            {K, V2} -> {K, V2}
        end
    end,
    [F(X) || X <- TupleList].

test() ->
    Pm = #uss_pm_t{
        info=#pm_info{cpu=[{spec, [#cpu_info{}]}]}
    },
    eterm_to_json(Pm).

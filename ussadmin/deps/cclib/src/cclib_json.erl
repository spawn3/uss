-module(cclib_json).
-compile(export_all).

-include("cclib.hrl").

encode(TupleList) ->
    try
        iolist_to_binary(cclib_mochijson2:encode(TupleList))
    catch
        Error:Exception ->
            ?ERROR_REPORT({Error, Exception, erlang:get_stacktrace()}),
            throw(Exception)
    end.

default_eterm_to_json(_EncF, []) -> [];
default_eterm_to_json(_EncF, undefined) -> null;
default_eterm_to_json(_EncF, Term) when is_number(Term) -> Term;
default_eterm_to_json(_EncF, Term) when is_atom(Term) -> Term;
default_eterm_to_json(_EncF, Term) when is_binary(Term) -> Term;
default_eterm_to_json(_EncF, Term) when is_pid(Term) ->
    list_to_binary(pid_to_list(Term));
default_eterm_to_json(_EncF, Term) when is_list(Term) ->
    try
        iolist_to_binary(Term)
    catch
        _:_ ->
            case cclib_utils:is_tuplelist(Term) of
                true ->
                    [{_EncF(K), _EncF(V)} || {K, V} <- Term];
                false ->
                    % TODO
                    Term
            end
    end.
%default_eterm_to_json(_EncF, Term) when is_tuple(Term) ->
%    L1 = ["element" ++ integer_to_list(N) || N <- lists:seq(1, tuple_size(Term))],
%    L2 = tuple_to_list(Term),
%    default_eterm_to_json(lists:zip(L1, L2)).

%normalize(ok) ->
%    [{ok, ok}];
%normalize({ok, Value}) ->
%    [{ok, Value}];
%normalize({error, Reason}) ->
%    [{error, Reason}].

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

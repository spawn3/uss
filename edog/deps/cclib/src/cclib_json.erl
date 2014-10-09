-module(cclib_json).
-compile(export_all).

-include("cclib.hrl").

encode(TupleList) ->
    try
        iolist_to_binary(cclib_mochijson2:encode(TupleList))
    catch
        Error:Exception ->
            ?ERROR({Error, Exception, erlang:get_stacktrace()}),
            throw(Exception)
    end.

default_eterm_to_json(_EncF, [])                        -> [];
default_eterm_to_json(_EncF, undefined)                 -> null;
default_eterm_to_json(_EncF, Term) when is_number(Term) -> Term;
default_eterm_to_json(_EncF, Term) when is_atom(Term)   -> Term;
default_eterm_to_json(_EncF, Term) when is_binary(Term) -> Term;
default_eterm_to_json(_EncF, Term) when is_pid(Term)    ->
    list_to_binary(pid_to_list(Term));
default_eterm_to_json(_EncF, Term) when is_list(Term)   ->
    case is_string(Term) of
        true ->
            iolist_to_binary(Term);
        false ->
            case lists:all(fun is_string/1, Term) of
                true ->
                    Term2 = [iolist_to_binary(X) || X <- Term],
                    {array, Term2};
                false ->
                    tuplelist_to_json(_EncF, Term)
            end
    end.

tuplelist_to_json(_EncF, Term) ->
    case cclib_utils:is_tuplelist(Term) of
        true ->
            [{_EncF(K), _EncF(V)} || {K, V} <- Term];
        false ->
            {array, Term}
    end.

is_string([H|_]) when is_integer(H) ->
    true;
is_string(_) ->
    false.

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

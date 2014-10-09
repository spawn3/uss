-module(edog_json).
-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").
-include("edog_common.hrl").

eterm_to_json(Term) when is_record(Term, cust_t) ->
    ?record_to_json(fun eterm_to_json/1, cust_t, Term);
eterm_to_json(Term) when is_record(Term, disk_t) ->
    ?record_to_json(fun eterm_to_json/1, disk_t, Term);
eterm_to_json(Term) when is_record(Term, vm_t) ->
    ?record_to_json(fun eterm_to_json/1, vm_t, Term);
eterm_to_json(Term) when is_record(Term, nic_t) ->
    ?record_to_json(fun eterm_to_json/1, nic_t, Term);
eterm_to_json(Term) when is_record(Term, cmd_info) ->
    ?record_to_json(fun eterm_to_json/1, cmd_info, Term);
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

is_my_record(Term) when is_record(Term, cust_t) -> true;
is_my_record(Term) when is_record(Term, disk_t) -> true;
is_my_record(Term) when is_record(Term, vm_t)   -> true;
is_my_record(Term) when is_record(Term, nic_t)  -> true;
is_my_record(Term) when is_record(Term, cmd_info)  -> true;
is_my_record(_) -> false.

% Result is a JSON binary.
encode(Result) ->
    cclib_json:encode(normalize(Result)).

normalize([]) ->
    [];
normalize([H|_] = Result) when is_list(Result), not is_integer(H) ->
    eterm_to_json(Result);
normalize(ok) ->
    eterm_to_json([{ok, ok}]);
normalize({ok, Value}) ->
    eterm_to_json([{ok, Value}]);
normalize({error, Reason}) ->
    eterm_to_json([{error, Reason}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
    L = [#vm_t{vm_pcis=[#nic_t{}]}],
    Tuplelist = [eterm_to_json(X) || X <- L],
    ?INFO({L, Tuplelist}),
    cclib_json:encode(Tuplelist).

print(RD, Ctx) ->
    ?INFO({raw_path, wrq:raw_path(RD)}),
    ?INFO({path, wrq:path(RD)}),
    ?INFO({path_info, wrq:path_info(RD)}),
    ?INFO({path_info, wrq:path_info(id, RD)}),
    ?INFO({disp_path, wrq:disp_path(RD)}),
    ?INFO({path_tokens, wrq:path_tokens(RD)}),
    ?INFO({req_qs, wrq:req_qs(RD)}),
    ?INFO({req_headers, wrq:req_headers(RD)}),
    ?INFO({req_body, wrq:req_body(RD)}),
    {true, RD, Ctx}.

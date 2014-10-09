-module(cclib_utils).
-compile(export_all).

-include("cclib.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FILE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
file_exist(Which) ->
    case filelib:is_regular(Which) of
        true -> true;
        false -> false
    end.

ensure_dir(Dir) ->
    case lists:last(Dir) of
        $/ -> filelib:ensure_dir(Dir);
        _ -> filelib:ensure_dir(Dir ++ "/")
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
now_to_integer() ->
    Now = now(),
    element(1,Now)*1000000+element(2,Now).

now_to_string() ->
    integer_to_list(now_to_integer()).

time_string(Separator) ->
    {{Y,M,D}, {HH,MM,SS}} = calendar:local_time(),
    lists:flatten(io_lib:format("~4.10.0B~2.10.0B~2.10.0B~s~2.10.0B~2.10.0B~2.10.0B",
            [Y,M,D,Separator, HH,MM,SS])).

time_string() ->
    time_string("").

to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_binary(X) ->
    to_atom(binary_to_list(X));
to_atom(X) when is_list(X) ->
    try list_to_existing_atom(X) of
        Expr ->
            Expr
    catch
        _:_ ->
            list_to_atom(X)
    end.

to_binary(X) when is_binary(X) ->
    X;
to_binary(X) when is_list(X) ->
    list_to_binary(X).

to_integer(X) when is_integer(X) ->
    X;
to_integer(X) when is_binary(X) ->
    to_integer(binary_to_list(X));
to_integer(X) when is_list(X) ->
    list_to_integer(X).

to_list(X) when is_binary(X) ->
    erlang:binary_to_list(X);
to_list(X) ->
    X.

str_equal(S1, S2) ->
    to_list(S1) =:= to_list(S2).

p_binary_to_list(Ip) when is_binary(Ip) -> binary_to_list(Ip);
p_binary_to_list(Ip) when is_list(Ip) -> Ip.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CMD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cmd(Cmd) ->
    Cmd1 = lists:flatten(Cmd),
    ?INFO([cmd, iolist_to_binary(Cmd1)]),
    _Result = os:cmd(Cmd1),
    ?INFO([result, iolist_to_binary(_Result)]),
    _Result.

cmd(Node, Cmd) when is_atom(Node) ->
    Cmd1 = lists:flatten(Cmd),
    ?INFO({cmd, Node, iolist_to_binary(Cmd1)}),
    _Result = rpc:call(Node, os, cmd, [Cmd1]).

cmd(NodeName, Ip, Cmd) when is_list(Ip) ->
    cmd(to_node(NodeName, Ip), Cmd).

cmd_list(Args) when is_list(Args) ->
    Cmd = lists:concat(Args),
    os:cmd(Cmd).

check_cmd_result(_S, []) ->
    false;
check_cmd_result(S, [Pattern|T]) ->
    case Pattern of
        "" -> S =:= "";
        _ ->
            case re:run(S, Pattern) of
                {match, _} ->
                    true;
                nomatch ->
                    check_cmd_result(S, T)
            end
    end.

pgrep(Program) ->
    Result = cmd_list(["pgrep ", Program]),
    Lines = string:tokens(Result, "\n"),
    [erlang:list_to_integer(X) || X <- Lines].

is_process_exists(Program) ->
    case pgrep(Program) of
        [] -> false;
        _ -> true
    end.

get_pids(Program, Key) ->
    Script = filename:join([filename:dirname(code:which(?MODULE)),
            "../script/is_process_exists_of_cmd.sh"]),
    case cclib_cmd:run(["bash ", Script, " ", Program, " ", Key]) of
        {0, _Data} ->
            {ok, _Data};
        {_ExitStatus, {timeout, _Data}} ->
            {error, _ExitStatus};
        {_ExitStatus, _Data} ->
            {error, _ExitStatus}
    end.

is_process_exists(Program, Key) ->
    case get_pids(Program, Key) of
        {ok, [_|_]} -> true;
        _           -> false
    end.

run_cmd_w_info(Cmd) ->
    run_cmd(Cmd, [info]).

run_cmd(Cmd) ->
    run_cmd(Cmd, []).

run_cmd(Cmd, Opts) ->
    IsInfo = proplists:get_bool(info, Opts),
    Cmd1 = lists:flatten(Cmd),
    case IsInfo of
        true  -> ?INFO([cmd, iolist_to_binary(Cmd1)]);
        false -> ok
    end,
    _Result = os:cmd(Cmd1),
    case IsInfo of
        true  -> ?INFO([result, iolist_to_binary(lists:flatten(_Result))]);
        false -> ok
    end,
    _Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lists_intersection(L1, L2) ->
    Set1 = sets:from_list(L1),
    Set2 = sets:from_list(L2),
    sets:to_list(sets:intersection(Set1, Set2)).

lists_unique(L) ->
    L1 = lists:usort(L),
    if
        length(L1) =:= length(L) -> true;
        true -> false
    end.

keyfind(Key, N, TupleList, Default) ->
    case lists:keyfind(Key, N, TupleList) of
        {Key, Value} -> Value;
        false -> Default
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NODE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_localnode(Ip) when is_list(Ip) ->
    is_localnode(list_to_binary(Ip));
is_localnode(Ip) when is_binary(Ip) ->
    LocalIp = get_ip(node()),
    LocalIp =:= Ip.

get_ip(Node) ->
    L = string:tokens(atom_to_list(Node), "@"),
    list_to_binary(lists:nth(2, L)).

get_pair(Node) when is_atom(Node) ->
    L = string:tokens(atom_to_list(Node), "@"),
    list_to_tuple(L).

to_node(NodeName, Ip) ->
    Node = lists:flatten(io_lib:format("~s@~s", [NodeName, Ip])),
    to_atom(Node).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ENV
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_env(Key, Default) ->
    case application:get_env(Key) of
        {ok, V} -> {ok, V};
        _       -> {ok, Default}
    end.

get_env(Key) ->
    application:get_env(Key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_tuplelist(Term) when is_list(Term) ->
    F = fun(X) -> is_tuple(X) andalso tuple_size(X) =:= 2 end,
    lists:all(F, Term);
is_tuplelist(_) ->
    false.

%% -----------------------------------------------------
%% Mail
%% deps:
%%   sendmail
%%   change hostname to use long name
%% -----------------------------------------------------
mail(To, Subject, Header, Body) ->
    Cmd = io_lib:format("echo \"~s\"|mail -s \"~s\" -a \"~s\" ~s", [Body, Subject, Header, To]),
    ?os_cmd(Cmd).

name_to_pid(Name) ->
    case erlang:whereis(Name) of
        Pid when is_pid(Pid) ->
            Pid;
        _ ->
            case global:whereis_name(Name) of
                Gpid when is_pid(Gpid) ->
                    Gpid;
                _ ->
                    undefined
            end
    end.

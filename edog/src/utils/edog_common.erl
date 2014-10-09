-module(edog_common).
-export([
        flush_msg/0,
        get_node/1,
        get_pair/1,
        to_float/1
    ]).
-compile(export_all).

-include("edog_common.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_node(Ip) ->
    get_node(?AGENTNAME, Ip).

get_node(Node, Ip) ->
    try list_to_existing_atom(Node++ "@" ++ Ip) of
        Expr ->
            Expr
    catch
        _:_ ->
            list_to_atom(Node ++ "@" ++ Ip)
    end.

get_pair(Node) when is_atom(Node) ->
    L = string:tokens(atom_to_list(Node), "@"),
    list_to_tuple(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
call(Ip, Mod, Fun, ArgList) ->
    rpc:call(get_node(Ip), Mod, Fun, ArgList).

keyfind(Key, N, TupleList, Default) ->
    case lists:keyfind(Key, N, TupleList) of
        {Key, Value} -> Value;
        false -> Default
    end.

if_cond(Cond, Value, Default) ->
    if
        Cond -> Value;
        true -> Default
    end.

flush_msg() ->
    receive
        _Any ->
            flush_msg()
    after 0 ->
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cmd(Node, Cmd) when is_atom(Node) ->
    Cmd1 = lists:flatten(Cmd),
    ?INFO({cmd, Node, Cmd1}),
    _Result = rpc:call(Node, os, cmd, [Cmd1]);
cmd(Ip, Cmd) when is_list(Ip) ->
    cmd(get_node(Ip), Cmd).

cmd(Cmd) ->
    Cmd1 = lists:flatten(Cmd),
    ?INFO([cmd, Cmd1]),
    _Result = os:cmd(Cmd1),
    ?INFO([result, lists:flatten(_Result)]),
    _Result.

is_match(S, Pattern) ->
    case re:run(S, Pattern) of
        {match, _} -> true;
        nomatch -> false
    end.

rpc_reply(Reply) ->
    case Reply of
        {badrpc, Reason} -> {error, Reason};
        {error, Reason} -> {error, Reason};
        {ok, Res} -> {ok, Res};
        ok -> {ok, true}
    end.

reply(Reply) ->
    ?INFO({reply, Reply}),
    Reply.

-spec to_float(String::list()) -> float().

to_float(String) ->
    case string:to_float(String) of
        {error, _} ->
            case string:to_integer(String) of
                {error, _} -> 0.0;
                {Int, _}   -> Int / 1.0
            end;
        {Float, _} ->
            Float
    end.

proc_info(Program) ->
    Cmd = lists:concat(["ps --no-headers -o pid,pcpu,pmem,cmd -C ", Program]),
    Result = os:cmd(Cmd),
    F = fun(Line) ->
        [Pid, Cpu, Mem|T] = string:tokens(Line, " \t"),
        #proc_info{
            pid=list_to_integer(Pid),
            cpu=to_float(Cpu),
            mem=to_float(Mem),
            cmd=T
        }
    end,
    Lines = string:tokens(Result, "\n"),
    [F(X) || X <- Lines].

qemu_proc_info() ->
    % L = proc_info("qemu-kvm"),
    L = proc_info(filename:basename(edog_conf:bin_kvm())),
    F = fun(#proc_info{cmd=T}=Info) ->
        case lists:dropwhile(fun(Elem) -> Elem =/= "-uuid" end, T) of
            [] ->
                Info;
            Rest ->
                Info#proc_info{uuid=lists:nth(2, Rest)}
        end
    end,
    [F(X) || X <- L].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).

float_test() ->
    ?assertEqual(193.0, to_float("193")),
    ?assertEqual(193.0, to_float("193.0")),
    ?assertEqual(0.0, to_float("aaa0")),
    ok.

-endif.

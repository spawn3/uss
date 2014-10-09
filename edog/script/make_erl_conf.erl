#!/usr/bin/env escript
%% -*- erlang -*-

-define(APPLICATION_ROOT,  "/sysy/yfs/ussadmin").
-define(MASTER_NAME,       "edog_master").

main(Args) ->
    IpList = Args,
    Pred = fun(Ip) ->
        case inet_parse:address(Ip) of
            {ok, _Value} ->
                true;
            {error, _Reason} ->
                false
        end
    end,
    case lists:all(Pred, IpList) of
        true when length(IpList) > 0 ->
            Options = [
                {managers, [list_to_atom(?MASTER_NAME ++ "@" ++ X)||X<-IpList]},
                {plugins, get_plugins()}
            ],
            DstDir = filename:join([?APPLICATION_ROOT, "edog_runtime/conf"]),
            gen_master(filename:join([?APPLICATION_ROOT, "edog/conf/master.config.tpl"]), DstDir, Options),
            gen_agent(filename:join([?APPLICATION_ROOT, "edog/conf/master.config.tpl"]), DstDir, Options),
            ok;
        _ ->
            usage(),
            halt(1)
    end.

usage() ->
    io:format("Usage:~n"),
    io:format("   ./make_erl_conf.erl [master_ip|...]~n"),
    io:format("For example:~n"),
    io:format("   ./make_erl_conf.erl 192.168.1.14 192.168.1.15~n").

get_plugins() ->
    case file:read_file(filename:join([?APPLICATION_ROOT, "edog/conf/plugins.tpl"])) of
        {ok, B} ->
            string:tokens(binary_to_list(B), " \t\n");
        {error, Reason} ->
            io:format("error: get_plugins, ~p~n", [Reason]),
            []
    end.

gen_master(Path, DstDir, Options) ->
    {ok, [Terms]} = file:consult(Path),
    Masters = proplists:get_value(managers, Options),
    F = fun(N) ->
        NewTerms = parse_conf(Terms, [{seq, N}|Options], []),
        File = filename:join([DstDir, "master" ++ integer_to_list(N) ++ ".config"]),
        filelib:ensure_dir(File),
        unconsult(File, [NewTerms])
    end,
    lists:foreach(F, lists:seq(1,length(Masters))).

gen_agent(Path, DstDir, Options) ->
    {ok, [Terms]} = file:consult(Path),
    L = parse_conf(Terms, [{seq, 1}|Options], []),
    NewTerms = manager_to_agent(L, []),
    File = filename:join([DstDir, "agent.config"]),
    filelib:ensure_dir(File),
    unconsult(File, [NewTerms]).

manager_to_agent([], Acc) ->
    lists:reverse(Acc);
manager_to_agent([{kernel, Config}|T], Acc) ->
    SkipL = [distributed, sync_nodes_mandatory, sync_nodes_optional, sync_nodes_timeout],
    L = [X || X <- Config, not lists:member(element(1, X), SkipL)],
    NewConfig = [manager_to_agent(X) || X <- L],
    manager_to_agent(T, [{kernel, NewConfig}|Acc]);
manager_to_agent([{sasl, Config}|T], Acc) ->
    L = Config,
    NewConfig = [manager_to_agent(X) || X <- L],
    manager_to_agent(T, [{sasl, NewConfig}|Acc]);
manager_to_agent([{edog, Config}|T], Acc) ->
    L = Config,
    NewConfig = [manager_to_agent(X) || X <- L],
    manager_to_agent(T, [{edog, NewConfig}|Acc]);
manager_to_agent([Other|T], Acc) ->
    manager_to_agent(T, [Other|Acc]).

manager_to_agent({error_logger, {file, File}}) ->
    File2 = filename:join([filename:dirname(File), "agent.log"]),
    {error_logger, {file, File2}};
manager_to_agent({error_logger_mf_dir, File}) ->
    File2 = filename:join([filename:dirname(File), "agent_logs"]),
    {error_logger_mf_dir, File2};
manager_to_agent({asmaster, 1}) ->
    {asagent, 1};
manager_to_agent(Other) ->
    Other.

parse_conf([], _Options, Acc) ->
    lists:reverse(Acc);
%
parse_conf([{kernel, Config}|T], Options, Acc) ->
    NewConfig = parse(kernel, Config, Options, Acc),
    parse_conf(T, Options, [{kernel, NewConfig}|Acc]);
%
parse_conf([{ussadmin, Config}|T], Options, Acc) ->
    NewConfig = parse(ussadmin, Config, Options, []),
    parse_conf(T, Options, [{ussadmin, NewConfig}|Acc]);
%
parse_conf([{edog, Config}|T], Options, Acc) ->
    NewConfig = parse(edog, Config, Options, []),
    parse_conf(T, Options, [{edog, NewConfig}|Acc]);
parse_conf([H|T], Options, Acc) ->
    parse_conf(T, Options, [H|Acc]).

parse(_, [], _Options, Acc) ->
    lists:reverse(Acc);
% TODO
parse(edog, [{edog_masters, _OldValue}|T], Options, Acc) ->
    NewValue = proplists:get_value(managers, Options),
    parse(edog, T, Options, [{edog_masters, NewValue}|Acc]);
%
parse(ussadmin, [{managers, _OldValue}|T], Options, Acc) ->
    NewValue = proplists:get_value(managers, Options),
    parse(ussadmin, T, Options, [{managers, NewValue}|Acc]);
parse(ussadmin, [{plugins, _OldValue}|T], Options, Acc) ->
    NewValue = proplists:get_value(plugins, Options),
    parse(ussadmin, T, Options, [{plugins, NewValue}|Acc]);
%
parse(kernel, [{distributed, [{App, _OldValue}]}|T], Options, Acc) ->
    [Primary|Others] = proplists:get_value(managers, Options),
    NewValue = [{App, [list_to_tuple([Primary|Others])]}],
    parse(kernel, T, Options, [{distributed, NewValue}|Acc]);
parse(kernel, [{sync_nodes_mandatory, _OldValue}|T], Options, Acc) ->
    Masters = proplists:get_value(managers, Options),
    N = proplists:get_value(seq, Options) - 1,
    {L1, L2} = lists:split(N, Masters),
    NewValue = L1 ++ tl(L2),
    parse(kernel, T, Options, [{sync_nodes_mandatory, NewValue}|Acc]);
parse(kernel, [{edog_masters, _OldValue}|T], Options, Acc) ->
    NewValue = proplists:get_value(managers, Options),
    parse(kernel, T, Options, [{edog_masters, NewValue}|Acc]);
%
parse(Kind, [H|T], Options, Acc) ->
    parse(Kind, T, Options, [H|Acc]).

unconsult(File, L) ->
    io:format("writing ~p to ~p~n", [L, File]),
    {ok, S} = file:open(File, write),
    lists:foreach(fun(X) -> io:format(S, "~p.~n" ,[X]) end, L),
    file:close(S).

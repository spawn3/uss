#!/usr/bin/env escript
%% -*- erlang -*-
%%! -setcookie ussadmin

main(_Args) ->
    L = init:get_arguments(),
    [Root] = proplists:get_value(root, L),
    io:format("~p~n", [Root]).

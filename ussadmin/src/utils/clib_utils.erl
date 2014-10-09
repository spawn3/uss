-module(clib_utils).
-export([
        get_node/1,
        get_env/1,
        get_env/2
    ]).
-compile(export_all).

-include("uss_common.hrl").

get_node(Ip) ->
    case cclib_utils:is_localnode(Ip) of
        true ->
            case application:get_env(?APPLICATION, asagent) of
                {ok, 1} -> node();
                _ -> cclib_utils:to_node(?NODE_NAME, Ip)
            end;
        false ->
            cclib_utils:to_node(?NODE_NAME, Ip)
    end.

%% -----------------------------------------------------
%% Configuration
%% -----------------------------------------------------
get_env(Key, Default) ->
    case application:get_env(?APPLICATION, Key) of
        {ok, V} -> {ok, V};
        _ -> {ok, Default}
    end.

get_env(Key) ->
    case application:get_env(?APPLICATION, Key) of
        {ok, V} -> {ok, V};
        V -> V
    end.

get_managers() ->
    case application:get_env(kernel, managers) of
        {ok, Managers} ->
            {ok, Managers}
    end.

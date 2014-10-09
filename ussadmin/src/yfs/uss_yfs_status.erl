-module(uss_yfs_status).
-export([
        info/1,
        get_status/1,
        get_pid/1
    ]).

-include("uss_common.hrl").

info({Type, N}) ->
    case get_status({Type, N}) of
        {ok, {cannot_lock, Status}} ->
            case uss_yfs_service:pid_file({Type, N}) of
                {ok, Path} ->
                    {ok, Pid} = read_pid(Path),
                    {ok, {Status, Pid}}
            end;
        _Other ->
            {error, stopped}
    end.

%% -----------------------------------------------------------
%% status file
%% -----------------------------------------------------------
read_status(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            L = string:tokens(binary_to_list(Bin), "\n"),
            case length(L) of
                0 -> {error, emptyfile};
                _ ->
                    case lists:nth(1, L) of
                        "starting" -> {ok, starting};
                        "running"  -> {ok, running};
                        "stopping" -> {ok, stopping};
                        "stopped" -> {ok, stopped}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_status({Type, N}) ->
    case uss_yfs_service:status_file({Type, N}) of
        {ok, Path} -> get_status(Path)
    end;
get_status(Path) ->
    Status = element(2, read_status(Path)),
    uss_nif:start(),
    case uss_nif:trylock(Path) of
        1  -> {ok, {can_lock, Status}};
        0  -> {ok, {cannot_lock, Status}};
        -1 -> {error, Status}
    end.

%% -----------------------------------------------------------
%% PID file
%% -----------------------------------------------------------
read_pid(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            L = string:tokens(binary_to_list(Bin), "\n"),
            case length(L) of
                0 -> {error, emptyfile};
                _ -> {ok, list_to_integer(lists:nth(1,L))}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_pid({Type, N}) ->
    case get_status({Type, N}) of
        {ok, {cannot_lock, _Status}} ->
            case uss_yfs_service:pid_file({Type, N}) of
                {ok, Path} ->
                    read_pid(Path)
            end;
        _Other ->
            {error, stopped}
    end.

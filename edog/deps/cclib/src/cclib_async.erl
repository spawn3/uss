-module(cclib_async).
-export([
        monitor/4,
        monitor/5,
        async_apply/3,
        async_apply/4,
        async_apply/5
    ]).
-compile(export_all).

-include("cclib.hrl").

-define(TIMEOUT_APPLY, 30000).

default_callback(#callback_cb{result=Result}) ->
    case Result of
        ok -> {ok, true};
        {ok, _} -> {ok, true};
        {error, Reason} -> {error, Reason};
        Other -> ?ERROR(Other)
    end.

async_apply(M, F, A) ->
    async_apply(M, F, A, {fun default_callback/1, #callback_cb{}}, ?TIMEOUT_APPLY).

async_apply(M, F, A, CallbackF) ->
    async_apply(M, F, A, CallbackF, ?TIMEOUT_APPLY).

async_apply(M, F, A, CallbackF, Timeout) ->
    {ok, spawn(fun() -> monitor(M, F, A, CallbackF, Timeout) end)}.

monitor(M, F, A, Timeout) when is_integer(Timeout) ->
    monitor(M, F, A, {fun default_callback/1, #callback_cb{}}, Timeout).

monitor(M, F, A, {CallbackF, #callback_cb{}=Cb}, Timeout) ->
    Tag = make_ref(),
    {Receiver, Mref} =
    erlang:spawn_monitor(
        fun() ->
            process_flag(trap_exit, true),
            Result = apply(M, F, A),
            exit({self(), Tag, Result})
        end),
    receive
        {'DOWN', Mref, _, _, {Receiver, Tag, Result}} ->
            CallbackF(Cb#callback_cb{result=Result});
        {'DOWN', Mref, _, _, Reason} ->
            CallbackF(Cb#callback_cb{result={error,Reason}})
    after Timeout ->
        exit(Receiver, kill),
        CallbackF(Cb#callback_cb{result={error, timeout}})
    end.

barrier_post(Key, M, F, A) ->
    {Receiver, Mref} =
    erlang:spawn_monitor(
        fun() ->
            process_flag(trap_exit, true),
            Result = apply(M, F, A),
            exit({self(), Key, Result})
        end),
    {Mref, Receiver}.

barrier_wait(List) -> barrier_wait(List, []).

barrier_wait([], Acc) -> Acc;
barrier_wait(List, Acc) ->
    receive
        {'DOWN', Mref, _, _, {Receiver, Key, Result}} ->
            barrier_wait(List -- [{Mref, Receiver}], [{Key, Result}|Acc]);
        {'DOWN', _Mref, _, _, Reason} ->
            {error, Reason}
    end.

% ------------------------------------------------------
block_apply(M, F, A, Timeout) ->
    From = self(),
    case erlang:apply(M, F, A ++ [[{from, From}]]) of
        {ok, _Res} ->
            loop(From, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

loop(From, Timeout) when is_pid(From) ->
    receive
        {From, Msg} ->
            Msg;
        _ ->
            loop(From, Timeout)
    after Timeout ->
        {error, timeout}
    end;
loop(_, _) ->
    {error, badarg}.

send(Pid, Msg) when is_pid(Pid) -> Pid ! {Pid, Msg};
send(_, _) -> {error, badarg}.


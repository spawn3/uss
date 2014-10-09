-module(uss_event_logger).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
        terminate/2, code_change/3]).

-include("uss_common.hrl").

-record(state, {}).

init([]) ->
    {ok, #state{}}.

handle_event(_Event, State) ->
    ?TTY_REPORT(_Event),
    {ok, State}.

handle_call(_Reqeust, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

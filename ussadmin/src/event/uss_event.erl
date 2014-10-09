%% ------------------------------------------------------------
%% @doc
%% Event:
%%   Node Event
%%   Service Event
%% @end
%% ------------------------------------------------------------
-module(uss_event).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
        terminate/2, code_change/3]).
-export([url/0, post/1, do_mail/2]).

-include("uss_common.hrl").

-define(POST_EVENT(Url, Event),
    begin uss_fg:post(Url, uss_json:eterm_to_json(Event)) end).

-record(state, {}).

%start_link() ->
%    gen_event:start_link({local, ?MODULE}).

%add_handler() ->
%    gen_event:add_handler(?MODULE, ?MODULE, []).

init([]) ->
    {ok, #state{}}.

handle_event({_From, #async_event{} = Event}, State) ->
    ?POST_EVENT(url(), Event),
    {ok, State};
handle_event({_From, {_From2, #async_event{} = Event}}, State) ->
    ?POST_EVENT(url(), Event),
    {ok, State};
handle_event(_Event, State) ->
    % ignore
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
url() ->
    {ok, Host} = uss_mnesia:option_get(ws_ip),
    {ok, Port} = uss_mnesia:option_get(ws_port),
    lists:flatten(io_lib:format("http://~s:~B/ear/srvc/stat", [Host, Port])).

post(#async_event{} = Event) ->
    TupleList = uss_json:eterm_to_json(Event),
    uss_fg:post(url(), TupleList).

do_mail(_From, #async_event{event=need_start} = Event) ->
    To = "dongguanjun@meidisen.com",
    Subject = "async_event:need_start",
    Header = "From: uss@meidisen.com",
    Body = cclib_json:encode(uss_json:eterm_to_json(Event)),
    F = fun() -> cclib_utils:mail(To, Subject, Header, Body) end,
    spawn(F);
do_mail(_From, _Event) ->
    ok.

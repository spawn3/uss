-module(edog_ops).
-behaviour(gen_server).
-export([
        start_link/0,
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
    ]).
-compile(export_all).

-include("edog_common.hrl").

-spec insert({string(), atom(), any()}) -> any().

insert({_VmID, _Action, _Info} = Args) ->
    gen_server:call(?MODULE, {insert, Args}).

delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

lookup(Key) ->
    gen_server:call(?MODULE, {lookup, Key}).

is_processing(VmID) ->
    case lookup(VmID) of
        []  -> false;
        [_] -> true
    end.

%% -----------------------------------------------------------
%% Callback
%% -----------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    _ = ets:new(edog_ops, [set, named_table, protected]),
    {ok, []}.

handle_call({insert, {_VmID, vm_start, _Info} = Arg} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = ets:insert(edog_ops, Arg),
    {reply, Reply, _State};
handle_call({insert, {_VmID, vm_migrate, _Info} = Arg} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = ets:insert(edog_ops, Arg),
    {reply, Reply, _State};
handle_call({delete, Key} = _Msg, _From, _State) ->
    ?INFO(_Msg),
    Reply = ets:delete(edog_ops, Key),
    {reply, Reply, _State};
handle_call({lookup, Key} = _Msg, _From, _State) ->
    %% ?INFO(_Msg),
    Reply = ets:lookup(edog_ops, Key),
    {reply, Reply, _State};

handle_call(_Msg, _From, _State) ->
    ?ERROR(_Msg),
    Reply = ok,
    {reply, Reply, _State}.

handle_cast(_Msg, _State) ->
    ?ERROR(_Msg),
    {noreply, _State}.

handle_info(_Msg, _State) ->
    ?ERROR(_Msg),
    {noreply, _State}.

terminate(_Reason, _State) ->
    {noreply, _State}.

code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.

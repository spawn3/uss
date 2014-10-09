-module(edog_select).
-behaviour(gen_server).
-export([
        start_link/0,
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3,

        push/1,

        select/0,
        tpl/0
    ]).
%-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("edog_common.hrl").

push({vm_start, VmId}) ->
    push({vm_start, VmId, []});
push({vm_start, VmId, Options}=Event) ->
    ?INFO(Event),
    gen_server:cast(?MODULE, {vm_start, VmId, Options});
push({vm_migrate, VmId}) ->
    gen_server:cast(?MODULE, {vm_migrate, VmId}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% -----------------------------------------------------------------
%% Callbacks
%% -----------------------------------------------------------------
init(_Args) ->
    process_flag(trap_exit, true),

    ?INFO({?MODULE, "inited"}),
    {ok, []}.

terminate(_Reason, _State) ->
    ?WARN({_Reason, _State}),
    ok.

handle_call(_Msg, _From, _State) ->
    ?ERROR(_Msg),
    Reply = ok,
    {reply, Reply, _State}.

%% -----------------------------------------------------------------------
%% Options: []
%%   * {wait_time, Milliseconds}
%%   * self_adaptive
%% -----------------------------------------------------------------------
handle_cast({vm_start, VmId, Options} = _Msg, _State) ->
    ?INFO({_Msg, self()}),
    Wait = proplists:get_value(wait_time, Options, 0),
    timer:sleep(Wait * 1000),
    case select_node(VmId, Options) of
        {ok, [H|_]=Info} when is_list(Info), is_tuple(H) ->
            edog_vm:vm_action({vm_start, VmId, Info});
        {ok, DestIp} ->
            Options2 = [{ip, DestIp}],
            edog_vm:vm_action({vm_start, VmId, Options2});
        {error, _Reason} ->
            edog_fg:notify(#notify_spec{op=vm_start, key=VmId, reply={error, _Reason}})
    end,
    {noreply, _State};
handle_cast({vm_migrate, VmId} = _Msg, _State) ->
    ?INFO(_Msg),
    case select_node(VmId, []) of
        {ok, [H|_]=Info} when is_list(Info), is_tuple(H) ->
            DestIp = proplists:get_value(ip, Info),
            edog_vm:vm_action({vm_migrate, VmId, DestIp});
        {ok, DestIp} ->
            edog_vm:vm_action({vm_migrate, VmId, DestIp});
        {error, _Reason} ->
            edog_fg:notify(#notify_spec{op=vm_migrate, key=VmId, reply={error, _Reason}})
    end,
    {noreply, _State};
handle_cast(_Msg, _State) ->
    ?ERROR({_Msg, _State}),
    {noreply, _State}.

handle_info(_Msg, _State) ->
    ?ERROR({_Msg, _State}),
    {noreply, _State}.

code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
-spec select_node(VmId, Options) ->
    {ok, Value} |
    {error, Reason}
    when
    VmId    :: uuid(),
    Options :: [T],
    Value   :: T,
    Reason  :: T,
    T :: term().

select_node(VmId, Options) ->
    LoadFlag = edog_option:option_get(edog_start_flag, max_memory),
    case edog_table_vm:lookup(VmId) of
        [#vm_t{vm_mem=Mem, vm_cpu=Cpu}] ->
            case edog_load:select_node({LoadFlag, Mem, Cpu}, Options) of
                {error, Reason} ->
                    ?ERROR({error, Reason}),
                    edog_table_vm:clear_restart(VmId),
                    {error, Reason};
                Res -> Res
            end;
        [] ->
            {error, noent}
    end.

tpl() ->
    cclib_dbg:tpl(?MODULE).

select() ->
    edog_select:push({vm_start,"8d4f5a65-0127-4b84-ba49-01a5189fbac3", [{wait_time,1},{self_adaptive,true},{try_node,true},{node,'edog@192.168.2.17'}]}),
    edog_select:push({vm_start,"93b94c5f-00f6-4118-a387-36e8925efce2", [{wait_time,1},{self_adaptive,true},{try_node,true},{node,'edog@192.168.2.17'}]}),
    ok.

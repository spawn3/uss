-module(edog_sup).
-behaviour(supervisor).
-export([
        start_link/1,
        init/1,

        web_ssl/2
    ]).

-include("edog_common.hrl").

start_link(_Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Args).

init(_Args) ->
    ?INFO(_Args),
    Flags = {one_for_one, 30, 60},
    Processes =
    case edog_app:is_manager() of
        true ->
            init_manager(),
            [{edog_master, {edog_master, start_link, []}, permanent, 10000, worker, [edog_master]}] ++
            [{edog_master_rt, {edog_master_rt, start_link, [_Args]}, permanent, 10000, worker, [edog_master_rt]}] ++
            [{edog_node_ping, {edog_node_ping, start_link, [_Args]}, permanent, 10000, worker, [edog_node_ping]}] ++
            [{edog_select, {edog_select, start_link, []}, permanent, 10000, worker, [edog_select]}] ++
            [{edog_vmsup, {edog_vmsup, start_link, [[]]}, permanent, 10000, supervisor, [edog_vmsup]}] ++
            [{edog_storage, {edog_storage, start_link, []}, permanent, 10000, worker, [edog_storage]}] ++
            webmachine() ++
            target_gc();
        false ->
            init_agent(),
            [{edog_slaves, {edog_slaves, start_link, []}, permanent, 10000, worker, [edog_slaves]}] ++
            [{edog_slaves_rt, {edog_slaves_rt, start_link, []}, permanent, 10000, worker, [edog_slaves_rt]}]
    end,
    Common = [{edog_libvirt, {edog_libvirt, start_link, []}, permanent, 10000, worker, [edog_libvirt]}],
    ?INFO({Processes, Common}),
    {ok, {Flags, Processes ++ Common}}.

target_gc() ->
    case {edog_conf:storage_module(), edog_conf:target_gc()} of
        {edog_storage_iscsi, true} ->
            [{edog_target_gc, {edog_target_gc, start_link, []}, permanent, 10000, worker, [edog_target_gc]}];
        _ ->
            []
    end.

webmachine() ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    Port = edog_conf:listen(),
    Priv = code:priv_dir(?APPLICATION),
    DispatchConf = filename:join([Priv, "dispatch.conf"]),
    {ok, Dispatch} = file:consult(DispatchConf),
    [web(Ip, Port, Dispatch)].
    %[web(Ip, Port, Dispatch)] ++ [web_ssl(Ip, Dispatch)].

web(Ip, Port, Dispatch) ->
    LogDir = filename:join([cclib:log_dir(), "webmachine"]),
    Config = [
        {name, webmachine_one},
        {ip, Ip},
        {port, Port},
        {log_dir, LogDir},
        {dispatch, Dispatch}],
    {webmachine_one, {webmachine_mochiweb, start, [Config]}, permanent, 5000, worker, dynamic}.

web_ssl(Ip, Dispatch) ->
    Config = [
        {name, webmachine_two},
        {ip, Ip},
        {port, 8443},
        {ssl, true},
        {ssl_opts, [
                {certfile, "/tmp/api_server.crt"},
                {cacertfile, "/tmp/api_server.crt"},
                {keyfile, "/tmp/api_server.key"}]},
        {log_dir, "priv/log"},
        {dispach, Dispatch}],
    {webmachine_two, {webmachine_mochiweb, start, [Config]}, permanent, 5000, warker, dynamic}.


init_manager() ->
    ok.

init_agent() ->
    ok.

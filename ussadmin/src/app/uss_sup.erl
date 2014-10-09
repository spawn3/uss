-module(uss_sup).
-behaviour(supervisor).
-export([
        start_link/1,
        init/1,
        start_agent/0,
        delete_agent/0,

        web_ssl/2
    ]).

-include("uss_common.hrl").

start_link(_Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Args).

init(_Args) ->
    Flags = {one_for_one, 30, 60},
    Master = case application:get_env(?APPLICATION, asmaster) of
        {ok, 1} ->
            init_manager(),
            webmachine() ++
            [{uss_event, {gen_event, start_link, [{local, uss_event}]}, permanent, 10000, worker, [dynamic]}] ++
            [{uss_manager, {uss_manager, start_link, []}, permanent, 10000, worker, [uss_manager]}] ++
            [{uss_manager_rt, {uss_manager_rt, start_link, []}, permanent, 10000, worker, [uss_manager_rt]}];
        _ ->
            []
    end,
    Slaves = case application:get_env(?APPLICATION, asagent) of
        {ok, 1} ->
            init_agent(),
            [{uss_agent, {uss_agent, start_link, []}, permanent, 10000, worker, [uss_agent]}] ++
            [{uss_agent_rt, {uss_agent_rt, start_link, []}, permanent, 10000, worker, [uss_agent_rt]}] ++
            [{uss_yfs, {uss_yfs, start_link, []}, permanent, 10000, worker, [uss_yfs]}];
        _ ->
            []
    end,
    Common = [],
    ?TTY_INFO_REPORT({Master, Slaves, Common}),
    {ok, {Flags, Master ++ Slaves ++ Common}}.

webmachine() ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    Port = case application:get_env(?APPLICATION, listen) of
        undefined -> 9602;
        {ok, Val} -> Val
    end,
    Priv = code:priv_dir(?APPLICATION),
    DispatchConf = filename:join([Priv, "dispatch.conf"]),
    {ok, Dispatch} = file:consult(DispatchConf),
    [web(Ip, Port, Dispatch)].
    %[web(Ip, Port, Dispatch)] ++ [web_ssl(Ip, Dispatch)].

web(Ip, Port, Dispatch) ->
    DataDir = filename:join([code:lib_dir(?APPLICATION), "../data"]),
    Config = [
        {name, webmachine_one},
        {ip, Ip},
        {port, Port},
        {log_dir, DataDir ++ "/webmachine"},
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
    DataDir = filename:join([code:lib_dir(?APPLICATION), "../data"]),
    ?TTY_INFO_REPORT({datadir, DataDir}),

    uss_web:start(9601),

    ok.

init_agent() ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_agent() ->
    ChildSpec1 = {uss_agent, {uss_agent, start_link, []}, permanent, 10000, worker, [uss_agent]},
    ChildSpec2 = {uss_agent_rt, {uss_agent_rt, start_link, []}, permanent, 10000, worker, [uss_agent_rt]},
    supervisor:start_child(?MODULE, ChildSpec1),
    supervisor:start_child(?MODULE, ChildSpec2),

    ok.

delete_agent() ->
    supervisor:terminate_child(?MODULE, uss_agent),
    supervisor:terminate_child(?MODULE, uss_agent_rt),
    supervisor:delete_child(?MODULE, uss_agent),
    supervisor:delete_child(?MODULE, uss_agent_rt),

    ok.

-module(edog_shell).
-compile(export_all).

-include("edog_common.hrl").

start_agent(Ip) ->
    Cmd = io_lib:format("ssh root@~s bash ~s/script/start_agent.sh ~s",
        [Ip, ?APP_SRC, Ip]),
    cclib_cmd:run(Cmd, 5000).

stop_agent(Ip) ->
    Cmd = io_lib:format("ssh root@~s bash ~s/script/start_agent.sh ~s stop",
        [Ip, ?APP_SRC, Ip]),
    cclib_cmd:run(Cmd, 5000).


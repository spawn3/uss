-module(edogdb_app).
-export([start/2, stop/1]).

-behaviour(application).

-include("edogdb.hrl").

start(_Type, _Args) ->
    try
        cclib:start_logger(true),
        start2(_Type, _Args)
    catch
        Class:Exception ->
            ?ERROR({Class, Exception, _Type, _Args, erlang:get_stacktrace()}),
            ?INIT_STOP()
    end.

start2(_Type, _Args) ->
    application:start(cclib),
    Log = filename:join([cclib:src_dir(), "conf/l4e_manager.conf"]),
    log4erl:conf(Log),

    ?INFO({start, _Type, _Args}),
    Res = edogdb_sup:start_link([_Type|_Args]),
    ?INFO(Res),
    Res.

stop(_State) ->
    ?INFO({stop, _State}),
    edogdb_cluster:stop_web(),
    ok.

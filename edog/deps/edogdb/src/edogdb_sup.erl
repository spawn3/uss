-module(edogdb_sup).
-behaviour(supervisor).
-export([
        start_link/1,
        init/1,

        info/0
    ]).

-include("edogdb.hrl").

start_link(_Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Args).

init(_Args) ->
    ?INFO(_Args),
    Flags = {one_for_one, 3, 30},
    Processes = [
        {edogdb, {edogdb, start_link, []}, permanent, 10000, worker, [edogdb]},
        {edogdb_monitor, {edogdb_monitor, start_link, []}, permanent, 10000, worker, [edogdb_monitor]},
        {edogdb_cluster, {edogdb_cluster, start_link, []}, permanent, 10000, worker, [edogdb_cluster]}
    ],
    ?INFO({Processes}),
    {ok, {Flags, Processes}}.

info() ->
    [
        {edogdb, whereis(edogdb)},
        {edogdb_monitor, whereis(edogdb_monitor)},
        {edogdb_cluster, whereis(edogdb_cluster)}
    ].

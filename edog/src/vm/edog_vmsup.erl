-module(edog_vmsup).
-behaviour(supervisor).
-export([
        start_link/1,
        init/1
    ]).

start_link(_Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Args).

init(_Args) ->
    Flags = {one_for_one, 30, 60},
    {ok, {Flags, []}}.

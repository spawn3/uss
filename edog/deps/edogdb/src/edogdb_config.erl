-module(edogdb_config).
-compile(export_all).

-include("edogdb.hrl").

all() ->
    L = [
        db_backup_interval
    ],
    [{X, erlang:apply(?MODULE, X, [])} || X <- L].

db_backup_interval() -> get_env(db_backup_interval, 7200). % s

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE
get_env(Key, Default) ->
    case application:get_env(edog, Key) of
        {ok, Value} -> Value;
        _           -> Default
    end.

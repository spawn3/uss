-module(cclib_app).
-export([ensure_started/1]).
-export([get_env/2, get_env/3]).
-compile(export_all).

-include("cclib.hrl").

ensure_started(App) ->
    %?INFO({ensure_started, App}),
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        {error, {not_started, App2}} ->
            ensure_started(App2);
        Other ->
            ?INFO(Other)
    end.

get_env(App, Key) ->
    application:get_env(App, Key).

get_env(App, Key, Default) ->
    case application:get_env(App, Key) of
        {ok, Value} ->
            {ok, Value};
        undefined ->
            {ok, Default}
    end.

-define(APP_ROOT, "/sysy/yfs/ussadmin").
-define(APP_SHM_DIR, "/dev/shm").

disk_file(App) ->
    filename:join([?APP_ROOT,  [$.|atom_to_list(App)]]).

shm_file(App) ->
    filename:join([?APP_SHM_DIR,  [$.|atom_to_list(App)]]).

exit_info_file(App) ->
    filename:join([?APP_ROOT, [$., atom_to_list(App), "_exit_info"]]).

create_flag(App) ->
    Time = cclib_utils:time_string("-"),
    file:write_file(disk_file(App), Time),
    file:write_file(shm_file(App), Time),
    file:write_file(exit_info_file(App), "").

delete_flag(App) ->
    file:delete(disk_file(App)),
    file:delete(shm_file(App)).

check_flag(App) ->
    case check_disk_flag(App) of
        true ->
            case check_shm_flag(App) of
                true  ->
                    proc_exit_abnormally;
                false ->
                    proc_exit_nodedown
            end;
        false ->
            proc_exit_normally
    end.

check_disk_flag(App) ->
    filelib:is_regular(disk_file(App)).

check_shm_flag(App) ->
    filelib:is_regular(shm_file(App)).

write_exit_info(App, Info) ->
    file:write_file(exit_info_file(App), Info).

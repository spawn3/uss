-module(cclib_http).
-compile(export_all).

start(Port, ServerName, ServerRoot, Mods) ->
    start(Port, ServerName, ServerRoot, Mods, []).

start(Port, ServerName, ServerRoot, Mods, Options) ->
    inets:start(httpd, [
            {modules, [mod_alias, mod_auth, mod_esi, mod_actions, mod_cgi,
                    mod_include, mod_dir, mod_get, mod_head,
                    mod_log, mod_disk_log]},
            {erl_script_alias, {"/uss", Mods}},
            {bind_address, "0.0.0.0"},
            {port, Port},
            {ipfamily, inet},
            {socket_type, ip_comm},
            {keep_alive, true},
            {keep_alive_timeout, 15},
            {server_name, ServerName},
            {server_root, ServerRoot},
            {document_root, ServerRoot},
            {directory_index, ["index.html", "welcome.html"]},
            {default_type, "text/plain"}|Options]),
    httpc_start().

% FIXME httpc_manager
httpc_start() ->
    httpc:set_options([
            {keep_alive_timeout, 0},
            {max_keep_alive_length, 120000}
        ]),
    ok.

-module(edog_resource_actions).
-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").
-include("edog_common.hrl").

init([]) ->
    {ok, undefined}.

allowed_methods(RD, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT', 'DELETE'], RD, Ctx}.

% GET
content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

to_json(RD, Ctx) ->
    Response = [
        {actions, [
                <<"libvirt_start">>,
                <<"cluster_restart">>,
                <<"cluster_stop">>,
                <<"stddisk_reset">>,

                <<"db_backup">>,
                <<"db_recover">>,
                <<"db_clear">>
            ]}
    ],
    {edog_json:encode(Response), RD, Ctx}.

resource_exists(RD, Ctx) ->
    {true, RD, Ctx}.

% PUT
content_types_accepted(RD, Ctx) ->
    {[{"application/json", from_json}], RD, Ctx}.

from_json(RD, Ctx) ->
    {true, RD, Ctx}.

% POST 2
process_post(RD, Ctx) ->
    {struct, Body} = cclib_mochijson2:decode(wrq:req_body(RD)),
    Res = case wrq:path_info(id, RD) of
        undefined ->
            do_actions(Body);
        _Id ->
            {error, <<"url:/actions">>}
    end,
    Response = edog_json:encode(Res),
    {true, wrq:append_to_response_body(Response, RD), Ctx}.

do_actions(Query) ->
    ?INFO(Query),
    case proplists:get_value(<<"action">>, Query) of
        <<"libvirt_start">> ->
            edog_master:libvirt_start(),
            ok;
        <<"stddisk_reset">> ->
            edog_master:stddisk_load(),
            ok;
        <<"cluster_restart">> ->
            edog_master:cluster_restart(),
            ok;
        <<"cluster_stop">> ->
            edog_master:cluster_stop(),
            ok;
        <<"db_backup">> ->
            do_db_backup(Query),
            ok;
        <<"db_recover">> ->
            do_db_recover(Query),
            ok;
        <<"db_clear">> ->
            do_db_clear(),
            ok;
        undefined ->
            {error, <<"no action provided">>};
        Action ->
            {error, list_to_binary("unsupported_action:" ++ binary_to_list(Action))}
    end.

do_db_backup(Query) ->
    case proplists:get_value(<<"file">>, Query) of
        undefined ->
            edog_master:db_backup();
        File ->
            edog_master:db_backup(binary_to_list(File))
    end.

do_db_recover(Query) ->
    case proplists:get_value(<<"file">>, Query) of
        undefined ->
            edog_master:db_recover();
        File ->
            edog_master:db_recover(binary_to_list(File))
    end.

do_db_clear() ->
    edog_master:db_clear().

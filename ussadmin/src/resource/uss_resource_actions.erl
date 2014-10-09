-module(uss_resource_actions).
-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").
-include("uss_common.hrl").

%-define(TABLE, uss_pm_t).

init([]) ->
    {ok, undefined}.

allowed_methods(RD, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT', 'DELETE'], RD, Ctx}.

resource_exists(RD, Ctx) ->
    {true, RD, Ctx}.

% GET
content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

to_json(RD, Ctx) ->
    Response = [
        {actions, [
                <<"libvirt_start">>,
                <<"stddisk_reset">>,
                <<"cluster_restart">>,
                <<"cluster_stop">>]}
    ],
    {cclib_json:encode(Response), RD, Ctx}.

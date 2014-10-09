-module(uss_resource_system).
-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").
-include("uss_common.hrl").

init([]) ->
    {ok, undefined}.

% GET
content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

to_json(RD, Ctx) ->
    Info = uss_manager:info(),
    Response = uss_json:eterm_to_json(Info),
    {cclib_json:encode(Response), RD, Ctx}.

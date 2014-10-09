-module(uss_resource_services).
-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").
-include("uss_common.hrl").

-define(TABLE, uss_yfs_t).

init([]) ->
    {ok, undefined}.

allowed_methods(RD, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT', 'DELETE'], RD, Ctx}.

resource_exists(RD, Ctx) ->
    case wrq:path_info(id, RD) of
        undefined ->
            {true, RD, Ctx};
        Id ->
            {cclib_mnesia:exists(?TABLE, Id), RD, Ctx}
    end.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

to_json(RD, Ctx) ->
    Response =
    case wrq:path_info(id, RD) of
        undefined ->
            L = [uss_types:uss_yfs_to_service(X) || X <- cclib_mnesia:i(?TABLE)],
            [uss_json:eterm_to_json(X) || X <- L]
    end,
    {cclib_json:encode(Response), RD, Ctx}.

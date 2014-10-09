-module(edog_resource_agents).
-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").
-include("edog_common.hrl").

init([]) ->
    {ok, undefined}.

content_types_provided(RD, Ctx) ->
    {[{"application/xml", to_xml}], RD, Ctx}.

to_xml(RD, Ctx) ->
    Xml = edog_master:slaves_dumpxml(),
    {Xml, RD, Ctx}.

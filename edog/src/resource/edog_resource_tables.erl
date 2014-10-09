-module(edog_resource_tables).
-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").
-include("edog_common.hrl").

init([]) ->
    {ok, undefined}.

content_types_provided(RD, Ctx) ->
    {[{"application/xml", to_xml}], RD, Ctx}.

to_xml(RD, Ctx) ->
    Xml =
    case wrq:path_info(table, RD) of
        undefined ->
            edog_master:table_dumpxml();
        Table ->
            edog_master:table_dumpxml([cclib_utils:to_atom(Table)])
    end,
    {Xml, RD, Ctx}.

-module(edog_resource_home).
-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").
-include("edog_common.hrl").

init(_Args) ->
    {ok, undefined}.

% GET
content_types_provided(RD, Ctx) ->
	{[{"application/json", to_json}], RD, Ctx}.

to_json(RD, Ctx) ->
    Info = edog_master:master_info(),
    Response = edog_json:encode(Info),
    {Response, RD, Ctx}.

%to_html(RD, Ctx) ->
%    {"<html><body>Hello, new world</body></html>", RD, Ctx}.
%
%to_xml(RD, Ctx) ->
%    {"<html><body>Hello, new world, will be changed to xml.</body></html>", RD, Ctx}.

%to_text(RD, Ctx) ->
		%Qs = wrq:req_qs(RD),
		%{Status, _} = whs_proxy:check_qs(Qs),
		%case Status of
		%	ok ->
		%		Method = proplists:get_value("Action", Qs),
		%		Method2 = utils:camelcase_to_underscore(Method),
		%		Rep = apply(whs_proxy, Method2, Qs),
		%		Body = io_lib:format("Hello from webmachine. ~w ~n", [Rep]);
		%	_ ->
		%		Body = {halt, 410}
		%end,
		%{Body, RD, Ctx}.

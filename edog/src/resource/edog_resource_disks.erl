-module(edog_resource_disks).
-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").
-include("edog_common.hrl").

-define(TOLIST(X), cclib_utils:to_list(X)).
-define(TOINT(X),  cclib_utils:to_integer(X)).

init([]) ->
    {ok, undefined}.

allowed_methods(RD, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT', 'DELETE'], RD, Ctx}.

resource_exists(RD, Ctx) ->
    case wrq:path_info(id, RD) of
        undefined ->
            {true, RD, Ctx};
        "free" ->
            {true, RD, Ctx};
        Id ->
            {cclib_mnesia:exists(disk_t, Id), RD, Ctx}
    end.

% GET
content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

to_json(RD, Ctx) ->
    Response =
    case wrq:path_info(id, RD) of
        "free" ->
            edog_disk:select_free_disk();
        undefined ->
            cclib_mnesia:i(disk_t)
    end,
    L = [map_disk(X) || X <- Response],
    {edog_json:encode(L), RD, Ctx}.


map_disk(#disk_t{canboot=Boot}=Disk) ->
    F = fun
        (X) when is_atom(X) ->
            {X, true};
        (X) when is_tuple(X) ->
            X
    end,
    NewBoot = [F(X) || X <- Boot],
    Disk#disk_t{canboot=NewBoot}.

% POST 1
post_is_create(RD, Ctx) ->
    {false, RD, Ctx}.

create_path(RD, Ctx) ->
    {undefined, RD, Ctx}.

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
            do_vd_create(Body);
        Id ->
            Hash = [
                {<<"lock">>,    do_vd_lock},
                {<<"unlock">>,  do_vd_unlock}],
            Action = proplists:get_value(<<"action">>, Body),
            case proplists:get_value(Action, Hash) of
                undefined ->
                    {error, <<"unsupported_action">>};
                Method ->
                    erlang:apply(?MODULE, Method, [Id, Body])
            end
    end,
    Response = edog_json:encode(Res),
    {true, wrq:append_to_response_body(Response, RD), Ctx}.

delete_resource(RD, Ctx) ->
    Res =
    case wrq:path_info(id, RD) of
        undefined ->
            {error, <<"id not provided">>};
        Id ->
            edog_disk:disk_action({destroy, Id})
    end,
    Response = edog_json:encode(Res),
    {true, wrq:append_to_response_body(Response, RD), Ctx}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_vd_create(Query) ->
    CustUuid = ?TOLIST(proplists:get_value(<<"custid">>, Query)),
    Alias    = ?TOLIST(proplists:get_value(<<"alias">>, Query)),
    Capacity = ?TOINT(proplists:get_value(<<"capacity">>, Query)),
    Shared   = proplists:get_bool(<<"shared">>, Query),
    case edog_master:disk_create(CustUuid, Alias, Capacity, Shared) of
        {ok, DiskId} ->
            {ok, list_to_binary(DiskId)};
        {error, {timeout, _Reason}} ->
            {error, timeout};
        {error, Reason} ->
            {error, Reason}
    end.

do_vd_lock(Uuid, _Query) ->
    edog_master:disk_lock(Uuid).

do_vd_unlock(Uuid, _Query) ->
    edog_master:disk_unlock(Uuid).

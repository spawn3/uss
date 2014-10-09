-module(edog_resource_users).
-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").
-include("edog_common.hrl").

-define(TOLIST(X), cclib_utils:to_list(X)).
-define(TOINT(X),  cclib_utils:to_integer(X)).

init([]) ->
    {ok, undefined}.

allowed_methods(RD, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT', 'DELETE'], RD, Ctx}.

% GET
content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

to_json(RD, Ctx) ->
    Response = cclib_mnesia:i(cust_t),
    {edog_json:encode(Response), RD, Ctx}.

resource_exists(RD, Ctx) ->
    case wrq:path_info(id, RD) of
        undefined ->
            {true, RD, Ctx};
        UserId ->
            {cclib_mnesia:exists(cust_t, UserId), RD, Ctx}
    end.

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
%
% create
% update
% lock
% unlock
process_post(RD, Ctx) ->
    {struct, Body} = cclib_mochijson2:decode(wrq:req_body(RD)),
    Res = case wrq:path_info(id, RD) of
        undefined ->
            do_user_create(Body);
        Id ->
            Hash = [
                {<<"update">>, do_user_update},
                {<<"lock">>,   do_user_lock},
                {<<"unlock">>, do_user_unlock}],
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_user_create(Query) ->
    Alias     = proplists:get_value(<<"alias">>, Query),
    Company   = proplists:get_value(<<"company">>, Query),
    Address   = proplists:get_value(<<"address">>, Query),
    Contact   = proplists:get_value(<<"contact_person">>, Query),
    Telephone = proplists:get_value(<<"telephone">>, Query),
    Cellphone = proplists:get_value(<<"cellphone">>, Query),
    Email     = proplists:get_value(<<"email">>, Query),

    case edog_master:customer_create({Alias, Company, Address, Contact, Telephone, Cellphone, Email}) of
        {ok, Uuid} ->
            {ok, list_to_binary(Uuid)};
        {error, Reason} ->
            {error, Reason}
    end.

do_user_update(Uuid, Query) ->
    %Uuid      = ?TOLIST(proplists:get_value(<<"uuid">>, Query)),
    Alias     = proplists:get_value(<<"alias">>, Query),
    Company   = proplists:get_value(<<"company">>, Query),
    Address   = proplists:get_value(<<"address">>, Query),
    Contact   = proplists:get_value(<<"contact_person">>, Query),
    Telephone = proplists:get_value(<<"telephone">>, Query),
    Cellphone = proplists:get_value(<<"cellphone">>, Query),
    Email     = proplists:get_value(<<"email">>, Query),

    case edog_master:customer_update({Uuid, Alias, Company, Address, Contact, Telephone, Cellphone, Email}) of
        {ok, Uuid} ->
            {ok, list_to_binary(Uuid)};
        {error, Reason} ->
            {error, Reason}
    end.

do_user_lock(Uuid, _Query) ->
    edog_master:customer_lock(Uuid).

do_user_unlock(Uuid, _Query) ->
    edog_master:customer_unlock(Uuid).

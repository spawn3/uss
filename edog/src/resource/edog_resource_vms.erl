-module(edog_resource_vms).
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
        "running" ->
            {true, RD, Ctx};
        Id ->
            {cclib_mnesia:exists(vm_t, Id), RD, Ctx}
    end.

% GET
content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

to_json(RD, Ctx) ->
    Response =
    case wrq:path_info(id, RD) of
        undefined ->
            cclib_mnesia:i(vm_t);
        "running" ->
            edog_table_vm:select(running);
        Id ->
            edog_table_vm:lookup(Id)
    end,
    {edog_json:encode(Response), RD, Ctx}.

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
            do_vm_create(Body);
        Id ->
            Hash = [
                {<<"update">>,   do_vm_update},
                {<<"destroy">>,  do_vm_destroy},
                {<<"start">>,    do_vm_start},
                {<<"stop">>,     do_vm_stop},
                {<<"pause">>,    do_vm_pause},
                {<<"resume">>,   do_vm_resume},
                {<<"migrate">>,  do_vm_migrate}],
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
    case wrq:path_info(id, RD) of
        undefined ->
            {false, RD, Ctx};
        Id ->
            Res = do_vm_destroy(Id, []),
            Response = edog_json:encode(Res),
            {true, wrq:append_to_response_body(Response, RD), Ctx}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_vm_create(Query) ->
    CustId   = ?TOLIST(proplists:get_value(<<"custid">>, Query)),
    VmName   = proplists:get_value(<<"vmname">>, Query),
    Cpu      = ?TOINT(proplists:get_value(<<"cpu">>, Query)),
    Memory   = ?TOINT(proplists:get_value(<<"memory">>, Query)),
    Pcis     = proplists:get_value(<<"pcis">>, Query),
    BootDisk = ?TOLIST(proplists:get_value(<<"boot_disk">>, Query)),
    Vdisks   = proplists:get_value(<<"vdisks">>, Query),

    Pcis2   = map_pcis(Pcis),
    Vdisks2 = map_vdisks(Vdisks),

    IoDriver = proplists:get_value(<<"iodriver">>, Query),

    Opts = [{iodriver, p_iodriver(IoDriver)}],
    edog_master:vm_create(CustId, VmName, Cpu, Memory, Pcis2, BootDisk, Vdisks2,
        Opts).

do_vm_update(VmId, Query) ->
    VmName   = proplists:get_value(<<"vmname">>, Query),
    Cpu      = ?TOINT(proplists:get_value(<<"cpu">>, Query)),
    Memory   = ?TOINT(proplists:get_value(<<"memory">>, Query)),
    Pcis     = proplists:get_value(<<"pcis">>, Query),
    BootDisk = ?TOLIST(proplists:get_value(<<"boot_disk">>, Query)),
    Vdisks   = proplists:get_value(<<"vdisks">>, Query),

    Pcis2 = map_pcis(Pcis),
    Vdisks2 = map_vdisks(Vdisks),

    IoDriver = proplists:get_value(<<"iodriver">>, Query),
    Opts = [{iodriver, p_iodriver(IoDriver)}],
    edog_master:vm_update(VmId, VmName, Cpu, Memory, Pcis2, BootDisk, Vdisks2,
        Opts).

p_iodriver(<<"ide">>)    -> ide;
p_iodriver(<<"sata">>)   -> sata;
p_iodriver(<<"virtio">>) -> virtio;
p_iodriver(_)            -> virtio.

do_vm_destroy(VmId, _Query) ->
    edog_master:vm_destroy(VmId).

do_vm_start(VmId, Query) ->
    PmId = ?TOLIST(proplists:get_value(<<"pmid">>, Query)),
    case PmId of
        undefined ->
            edog_master:vm_start(VmId);
        _ ->
            edog_master:vm_start(VmId, PmId)
    end.

do_vm_stop(VmId, Query) ->
    Opts =
    case proplists:get_value(<<"method">>, Query, <<"destroy">>) of
        <<"shutdown">> -> [shutdown];
        _ -> []
    end,
    edog_master:vm_stop(VmId, Opts).

do_vm_pause(VmId, _Query) ->
    edog_master:vm_pause(VmId).

do_vm_resume(VmId, _Query) ->
    edog_master:vm_resume(VmId).

do_vm_migrate(VmId, Query) ->
    PmId = ?TOLIST(proplists:get_value(<<"pmid">>, Query)),
    case PmId of
        undefined ->
            edog_master:vm_migrate(VmId);
        _ ->
            edog_master:vm_migrate(VmId, PmId)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
map_pcis(Pcis) ->
    [{ proplists:get_value(<<"bridge">>, L),
       proplists:get_value(<<"mac">>, L),
       proplists:get_value(<<"model">>, L)} || {struct, L} <- Pcis].

map_vdisks(Vdisks) ->
    [?TOLIST(X) || X <- Vdisks].

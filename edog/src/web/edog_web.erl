-module(edog_web).
-export([
        start/1,

        actions/2,
        disks/2,

        customer_create/2,
        customer_update/2,
        customer_lock/2,
        customer_unlock/2,

        vd_create/2,
        vd_lock/2,
        vd_unlock/2,

        vm_create/2,
        vm_destroy/2,
        vm_update/2,
        vm_start/2,
        vm_stop/2,
        vm_pause/2,
        vm_resume/2,
        vm_migrate/2,

        test/2,
        table/2,
        table_dumpxml/2,
        slaves_dumpxml/2,
        mac_dumpxml/2,
        dom_dumpxml/2
    ]).

-include("edog_common.hrl").

-define(TOLIST(X), cclib_utils:to_list(X)).
-define(TOINT(X),  cclib_utils:to_integer(X)).

start(Port) ->
    cclib_http:start(Port, "httpd_edog", filename:join([?APP_SRC, "www"]), [io, edog_web]).

test(Env, Input) ->
    [
        "Content-type: text/html\r\n\r\n",
        "<HTML><HEAD><TITLE>Example</TITLE></HEAD>\r\n",
        "<BODY>\n",
        "<B>Environment中国:</B>", io_lib:format("~p", [Env]), "<BR>\n",
        "<B>Input:</B>", Input, "<BR>\n",
        "</BODY></HTML>\n"
    ].

table(_Env, _Input) ->
    [
        "Content-type: text/xml\r\n\r\n",
        "<xml>中中国国",
        "</xml>"
    ].

table_dumpxml(_Env, _Input) ->
    L = httpd:parse_query(_Input),
    Xml =
    case lists:keyfind("table", 1, L) of
        {"table", Table} -> edog_master:table_dumpxml([cclib_utils:to_atom(Table)]);
        false            -> edog_master:table_dumpxml()
    end,
    [
        "Content-type: text/xml\r\n\r\n",
        Xml
    ].

slaves_dumpxml(_Env, _Input) ->
    Xml = edog_master:slaves_dumpxml(),
    [
        "Content-type: text/xml\r\n\r\n",
        Xml
    ].

mac_dumpxml(_Env, _Input) ->
    Xml = edog_mnesia:mac_dumpxml(),
    [
        "Content-type: text/xml\r\n\r\n",
        Xml
    ].

dom_dumpxml(_Env, _Input) ->
    Xml = edog_libvirt:test_writexml(),
    [
        "Content-type: text/xml\r\n\r\n",
        Xml
    ].

%%-----------------------------------------------------------------------------------
%% JSON
%%-----------------------------------------------------------------------------------
-define(JSON_REQUEST_HANDLER(Env, Input, Handler),
    try
        case proplists:get_value(request_method, Env) of
            "POST" ->
                {struct, Query} = cclib_mochijson2:decode(list_to_binary(Input)),
                Result = Handler(Query),
                return(Result);
            "GET" ->
                L = httpd:parse_query(Input),
                Result = Handler(L),
                return(Result)
        end
    catch
        Error:Exception ->
            ?ERROR({Input, Handler, Error, Exception, erlang:get_stacktrace()})
    end).

return(Result) ->
    Json = edog_json:encode(Result),
    [
        "Content-type: application/json\r\n\r\n",
        Json
    ].

customer_create(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_customer_create).
customer_update(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_customer_update).
customer_lock(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_customer_lock).
customer_unlock(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_customer_unlock).

vd_create(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_vd_create).
vd_lock(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_vd_lock).
vd_unlock(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_vd_unlock).

vm_create(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_vm_create).
vm_destroy(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_vm_destroy).
vm_update(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_vm_update).
vm_start(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_vm_start).
vm_stop(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_vm_stop).
vm_pause(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_vm_pause).
vm_resume(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_vm_resume).
vm_migrate(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_vm_migrate).

actions(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_actions).
disks(Env, Input) -> ?JSON_REQUEST_HANDLER(Env, Input, do_disks).

%%-----------------------------------------------------------------------------------
%% DO
%%-----------------------------------------------------------------------------------
do_customer_create(Query) ->
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

do_customer_update(Query) ->
    Uuid      = ?TOLIST(proplists:get_value(<<"uuid">>, Query)),
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

do_customer_lock(Query) ->
    Uuid = ?TOLIST(proplists:get_value(<<"uuid">>, Query)),
    edog_master:customer_lock(Uuid).

do_customer_unlock(Query) ->
    Uuid = ?TOLIST(proplists:get_value(<<"uuid">>, Query)),
    edog_master:customer_unlock(Uuid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_vd_create(Query) ->
    CustUuid = ?TOLIST(proplists:get_value(<<"custid">>, Query)),
    Alias = proplists:get_value(<<"alias">>, Query),
    Capacity = ?TOINT(proplists:get_value(<<"capacity">>, Query)),
    case edog_master:disk_create(CustUuid, Alias, Capacity) of
        {ok, DiskId} ->
            {ok, list_to_binary(DiskId)};
        {error, {timeout, _Reason}} ->
            {error, timeout};
        {error, Reason} ->
            {error, Reason}
    end.

do_vd_lock(Query) ->
    Uuid = ?TOLIST(proplists:get_value(<<"uuid">>, Query)),
    edog_master:disk_lock(Uuid).

do_vd_unlock(Query) ->
    Uuid = ?TOLIST(proplists:get_value(<<"uuid">>, Query)),
    edog_master:disk_unlock(Uuid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_vm_create(Query) ->
    CustId   = ?TOLIST(proplists:get_value(<<"custid">>, Query)),
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
    edog_master:vm_create(CustId, VmName, Cpu, Memory, Pcis2, BootDisk,
        Vdisks2, Opts).

do_vm_update(Query) ->
    VmId     = ?TOLIST(proplists:get_value(<<"vmid">>, Query)),
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

p_iodriver(<<"sata">>)   -> sata;
p_iodriver(<<"ide">>)    -> ide;
p_iodriver(<<"virtio">>) -> virtio;
p_iodriver(_)            -> virtio.

do_vm_destroy(Query) ->
    VmId = ?TOLIST(proplists:get_value(<<"vmid">>, Query)),
    edog_master:vm_destroy(VmId).

do_vm_start(Query) ->
    VmId = ?TOLIST(proplists:get_value(<<"vmid">>, Query)),
    PmId = ?TOLIST(proplists:get_value(<<"pmid">>, Query)),
    case PmId of
        undefined ->
            edog_master:vm_start(VmId);
        _ ->
            edog_master:vm_start(VmId, PmId)
    end.

do_vm_stop(Query) ->
    VmId = ?TOLIST(proplists:get_value(<<"vmid">>, Query)),
    edog_master:vm_stop(VmId).

do_vm_pause(Query) ->
    VmId = ?TOLIST(proplists:get_value(<<"vmid">>, Query)),
    edog_master:vm_pause(VmId).

do_vm_resume(Query) ->
    VmId = ?TOLIST(proplists:get_value(<<"vmid">>, Query)),
    edog_master:vm_resume(VmId).

do_vm_migrate(Query) ->
    VmId = ?TOLIST(proplists:get_value(<<"vmid">>, Query)),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_actions(Query) ->
    Action = proplists:get_value(<<"action">>, Query),
    case Action of
        <<"libvirt_start">> ->
            edog_master:libvirt_start();
        <<"stddisk_reset">> ->
            edog_master:stddisk_load();
        <<"cluster_restart">> ->
            edog_master:cluster_restart();
        <<"cluster_stop">> ->
            edog_master:cluster_stop();
        _ ->
            ok
    end,
    ok.

do_disks(_Params) ->
    ok.

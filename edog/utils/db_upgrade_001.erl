#!/usr/bin/env escript
%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%% -------------------------------------------------------------------

-define(MYMODULE, 'db_upgrade_001').

-define(FORMAT(Format, X),
        lists:flatten(io_lib:format(Format, X))).
-define(PRINT1(X),
        io:format("[~p,~p] ~p~n", [?MYMODULE, ?LINE, X])).
-define(PRINT2(Format, X),
        ?PRINT1(?FORMAT(Format, X))).

%% --------------------------------------------------------------------
-define(INVALID_UUID, "").
-define(IGNORE, ignore).

-record(cust_t, {
        cust_id             :: uuid(),
        cust_name           :: string(),
        cust_home           :: string(),
        locked      = false :: boolean(),
        locktime    = 0     :: integer(),
        company             :: string(),
        address             :: string(),
        contact             :: string(),
        telephone           :: string(),
        cellphone           :: string(),
        email               :: string()
    }).

-record(stddisk_t, {
        disk_id      :: uuid(),
        path         :: string(),
        size         :: integer(),
        os_type      :: string(),
        os_version   :: string()
    }).

-record(vm_t, {
        vm_id                 :: uuid(),
        vm_name               :: string(),
        cust_id               :: uuid(),
        pm_id = ?INVALID_UUID :: uuid(),
        vm_cpu  = 0           :: number(),
        % KB
        vm_mem  = 0           :: number(),
        vm_imgid              :: uuid(),
        vm_pcis = []          :: list(),
        status                :: vmstate_t(),
        port = -1             :: integer()
    }).

-record(disk_t, {
        disk_id              :: uuid(),
        disk_alias           :: string(),
        cust_id              :: uuid(),
        path                 :: string(),
        % GB
        size        = 10     :: integer(),
        % format      = "raw"  :: string(),
        canboot = [{boot, false}] :: list(), %% opts
        vm_id                :: uuid(),
        target               :: string(),
        createtime  = 0      :: integer(),
        locktime    = 0      :: integer(),
        status               :: diskstate_t()
    }).

-record(disk_vm_t, {
        id,                  % {disk_id, vm_id}
        target,
        extra = []
    }).

unconsult(File, L) ->
    {ok, S} = file:open(File, [write]),
    lists:foreach(fun(X) -> io:format(S, "~p.~n", [X]) end, L),
    file:close(S).

main(Args) ->
    ?PRINT1(Args),
    InFile = lists:nth(1, Args),
    OutFile = lists:nth(2, Args),
    {ok, Content} = file:consult(InFile),
    Out = lists:filter(
        fun(X) -> X =/= ?IGNORE end,
        [handle(X) || X <- Content]),
    DiskVms = get_disk_vms(Content),
    unconsult(OutFile, Out ++ DiskVms),
    %?PRINT1(Out ++ DiskVms),

    ok.

handle({tables, Tables}) ->
    L = [handle_table(X) || X <- Tables],
    L1 = [X || X <- L, X =/= ?IGNORE],
    DiskVm = {disk_vm_t, [
            {record_name, disk_vm_t},
            {attributes, [id,target,extra]}]},
    {tables, [DiskVm|L1]};
handle(#vm_t{} = R) ->
    R1 = R#vm_t{pm_id=?INVALID_UUID, status=shutoff, port=-1},
    erlang:append_element(R1, []);
handle(#cust_t{} = R) ->
    R;
handle(#stddisk_t{} = R) ->
    R;
handle(#disk_t{target=Target} = R) when is_list(Target) ->
    case is_system_disk(Target) of
        true -> R;
        false -> ?IGNORE
    end;
handle(_) ->
    ?IGNORE.

get_disk_vms(Content) ->
    L = [handle_disk(X) || #disk_t{} = X <- Content],
    [X || X <- L, X =/= ?IGNORE].

handle_disk(#disk_t{disk_id=DiskId, vm_id=VmId, target=Target})
when VmId =/= undefined andalso Target =/= undefined ->
    case is_system_disk(Target) of
        true -> #disk_vm_t{id={DiskId, VmId}, target=Target};
        false -> ?IGNORE
    end;
handle_disk(#disk_t{}) ->
    ?IGNORE.

handle_table({cust_t, TableDef}) ->
    {cust_t, TableDef};
handle_table({stddisk_t, TableDef}) ->
    {stddisk_t, TableDef};
handle_table({disk_t, TableDef}) ->
    {disk_t, TableDef};
handle_table({vm_t, TableDef}) ->
    F = fun
        ({record_name, X}) -> {record_name, X};
        ({attributes, X}) -> {attributes, X ++ [extra]}
    end,
    {vm_t, [F(X) || X <- TableDef]};
handle_table({disk_vm_t, TableDef}) ->
    {disk_vm_t, TableDef};
handle_table(_) ->
    ?IGNORE.

is_system_disk(Target) when is_list(Target) ->
    case lists:last(Target) of
        $a -> true;
        _  -> true
    end;
is_system_disk(_) ->
    false.


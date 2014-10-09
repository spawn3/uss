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
        port = -1             :: integer(),
        extra = []
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


-record(env, {
        % cust_id -> cust_name
        custs = [],
        % disk_id -> vm_id
        disk_vm = [],
        % vm_id -> vm_name
        vms = [],
        % disk_id -> new_disk_id
        disks = [],

        include_vms = []
    }).


unconsult(File, L) ->
    {ok, S} = file:open(File, [write]),
    lists:foreach(fun(X) -> io:format(S, "~p.~n", [X]) end, L),
    file:close(S).


main(Args) ->
    %?PRINT1(Args),
    InFile = lists:nth(1, Args),
    OutFile = lists:nth(2, Args),

    IncludeVms =
    if
        length(Args) >= 3 ->
            {ok, L} = file:consult(lists:nth(3, Args)),
            L;
        true ->
            []
    end,
    {ok, Content} = file:consult(InFile),

    pass_00(Content),

    Env01 = pass_01(Content, #env{include_vms=IncludeVms}),
    %?PRINT1(Env01#env.vms),
    %?PRINT1(Env01#env.disk_vm),
    %?PRINT1(Env01#env.custs),
    %?PRINT1(Env01#env.disks),
    Env02 = pass_02(Content, Env01),
    %?PRINT1(Env02),

    Out = lists:filter(fun(X) -> X =/= ?IGNORE end, [handle(X, Env02) || X <- Content]),
    unconsult(OutFile, Out),
    %?PRINT1(Out ++ DiskVms),

    ok.

-define(CHARS, [$., $_]).

pass_00([]) ->
    ok;
pass_00([#vm_t{vm_name=VmName}|T]) ->
    VmName1 = to_list(VmName),
    case member(VmName1, ?CHARS) of
        false -> pass_00(T);
        true -> erlang:error({name_error, VmName1, ?CHARS})
    end;
pass_00([#disk_t{disk_alias=Alias}|T]) ->
    Alias1 = to_list(Alias),
    case member(Alias1, ?CHARS) of
        false -> pass_00(T);
        true  -> erlang:error({name_error, Alias1, ?CHARS})
    end;
pass_00([_Else|T]) ->
    pass_00(T).

member(_Str, []) ->
    false;
member(Str, [Ch|T]) ->
    case string:chr(Str, Ch) of
        0 -> member(Str, T);
        _ -> true
    end.

pass_01([], #env{}=Env) ->
    Env;
pass_01([#cust_t{cust_id=CustId, cust_name=CustName}|T], #env{custs=Custs}=Env) ->
    pass_01(T, Env#env{custs=[{CustId, CustName}|Custs]});
pass_01([#vm_t{vm_id=VmId, vm_name=VmName}|T], #env{vms=Vms}=Env) ->
    pass_01(T, Env#env{vms=[{VmId, VmName}|Vms]});
pass_01([#disk_vm_t{id={DiskId, VmId}}|T], #env{disk_vm=DiskVm}=Env) ->
    pass_01(T, Env#env{disk_vm=[{DiskId, VmId}|DiskVm]});
pass_01([_Else|T], #env{}=Env) ->
    pass_01(T, Env).


pass_02([], Env) ->
    Env;
pass_02([#disk_t{disk_id=DiskId, disk_alias=Alias, cust_id=CustId, status='created'}|T],
    #env{vms=Vms, custs=Custs, disk_vm=DiskVm, disks=Disks}=Env) ->
    %?PRINT1({DiskId, CustId}),
    try
        CustName = to_list(proplists:get_value(CustId, Custs)),
        TargetName =
        if
            Alias =:= "system" ->
                VmId = proplists:get_value(DiskId, DiskVm),
                to_list(proplists:get_value(VmId, Vms));
            true ->
                to_list(Alias)
        end,
        %%
        CustName1 = to_lower(CustName),
        TargetName1 = to_lower(TargetName),
        NewDiskId = string:join([CustName1, TargetName1], "."),
        pass_02(T, Env#env{disks=[{DiskId, NewDiskId}|Disks]})
    catch
        Error:Reason ->
            ?PRINT1({DiskId, Error, Reason}),
            pass_02(T, Env)
    end;
pass_02([_Else|T], #env{}=Env) ->
    pass_02(T, Env).


is_valid_customer(_CustName, #env{include_vms=[]}) ->
    true;
is_valid_customer(_CustName, #env{include_vms=IncludeVms}) ->
    case proplists:get_value(_CustName, IncludeVms) of
        undefined -> false;
        _         -> true
    end.


is_valid_vm(_CustName, _VmName, #env{include_vms=[]}) ->
    true;
is_valid_vm(CustName, VmName, #env{include_vms=IncludeVms}) ->
    CustName1 = to_lower(CustName),
    VmName1 = to_lower(VmName),
    F = fun({C, L}) ->
            case {C =:= CustName1, lists:member(VmName1, L)} of
                {true, true} -> true;
                _            -> false
            end
    end,
    lists:any(F, IncludeVms).


handle({tables, _}=R, _Env) ->
    R;
handle(#stddisk_t{}=R, _Env) ->
    R;
handle(#cust_t{cust_name=CustName}=R, #env{}=_Env) ->
    NewCustName = to_lower(CustName),
    case is_valid_customer(NewCustName, _Env) of
        true ->
            R#cust_t{cust_name=NewCustName};
        false ->
            ?IGNORE
    end;
handle(#disk_t{status=locked}, #env{}) ->
    ?IGNORE;
handle(#disk_t{disk_id=DiskId, disk_alias=Alias, path=OldPath, cust_id=CustId}=R,
    #env{disk_vm=DiskVms, vms=Vms, custs=Custs, disks=Disks}=Env) ->
    try
        CustName = proplists:get_value(CustId, Custs),
        VmId = proplists:get_value(DiskId, DiskVms),
        VmName = proplists:get_value(VmId, Vms),
        case is_valid_vm(CustName, VmName, Env) of
            true ->
                NewDiskId = proplists:get_value(DiskId, Disks),
                Path = diskid_to_path(NewDiskId),
                OldPath1 = case OldPath of
                    [$/|_] -> OldPath;
                    _      -> [$/|OldPath]
                end,
                io:format("~-40s ~-16s ~-16s ~-30s ~-100s~n", [VmId, VmName, Alias, NewDiskId, OldPath1]),
                R#disk_t{disk_id=NewDiskId, disk_alias=disk_alias(NewDiskId), path=Path};
            false ->
                ?IGNORE
        end
    catch
        Error:Reason ->
            ?PRINT1({DiskId, Error, Reason}),
            R
    end;
handle(#vm_t{vm_name=VmName, vm_imgid=DiskId, cust_id=CustId}=R,
    #env{custs=Custs, disks=Disks}=Env) ->
    try
        CustName = proplists:get_value(CustId, Custs),
        case is_valid_vm(CustName, VmName, Env) of
            true ->
                NewDiskId = proplists:get_value(DiskId, Disks),
                R#vm_t{pm_id="", port=-1, status='shutoff',
                    vm_name=to_lower(VmName),
                    extra=[{iodriver, virtio}], vm_imgid=NewDiskId};
            false ->
                ?IGNORE
        end
    catch
        Error:Reason ->
            ?PRINT1({DiskId, Error, Reason}),
            R#vm_t{pm_id="", port=-1, status='shutoff',
                vm_name=to_lower(VmName),
                extra=[{iodriver, virtio}]
            }
    end;
handle(#disk_vm_t{id={DiskId, VmId}}=R, #env{vms=Vms, disks=Disks}=Env) ->
    try
        NewDiskId = proplists:get_value(DiskId, Disks),
        [CustName|_] = string:tokens(NewDiskId, "."),
        VmName = proplists:get_value(VmId, Vms),
        case is_valid_vm(CustName, VmName, Env) of
            true ->
                R#disk_vm_t{id={NewDiskId, VmId}};
            false ->
                ?IGNORE
        end
    catch
        Error:Reason ->
            ?PRINT1({DiskId, Error, Reason}),
            R
    end;
handle(_, _Env) ->
    ?IGNORE.


to_list(X) when is_list(X) ->
    X;
to_list(X) when is_binary(X) ->
    binary_to_list(X).

to_lower(X) when is_list(X) ->
    string:to_lower(X);
to_lower(X) ->
    Str = to_list(X),
    to_lower(Str).

disk_alias(DiskId) ->
    [_, Alias] = string:tokens(DiskId, "."),
    Alias.

diskid_to_path(DiskId) ->
    L = string:tokens(DiskId, "."),
    "/" ++ string:join(L ++ ["0"], "/").

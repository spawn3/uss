-module(edog_iscsi).
-compile(export_all).

-include_lib("kernel/include/file.hrl").
-include("edog_common.hrl").

-define(DIRECTION_IN,  "in").
-define(DIRECTION_OUT, "out").

-define(ISCSI_OP_TIMEOUT, 30000).

init() ->
    target_discover().

%% ---------------------------------------------------------------------------
%% ip-192.168.1.13:3260-iscsi-iqn.2001-04.com.meidisen:ns1.tgt4-lun-0
%% ---------------------------------------------------------------------------
%% IQN
default_iqn() ->
    edog_conf:iscsi_iqn().

%% ns1.tgt1
target_name(Ns, Tgt) ->
    string:join([cclib_utils:to_list(Ns), cclib_utils:to_list(Tgt)], ".").

%% ns1.tgt1/0
lun_name(Ns, Tgt, Lun) ->
    lists:concat([target_name(Ns, Tgt), "/", Lun]).

%% /ns1/tgt1
target_dir(Ns, Tgt) ->
    filename:join(["/", cclib_utils:to_list(Ns), cclib_utils:to_list(Tgt)]).

%% /ns1/tgt1/0
lun_dir(Ns, Tgt, Lun) ->
    filename:join(["/", cclib_utils:to_list(Ns), cclib_utils:to_list(Tgt), integer_to_list(Lun)]).

%% ---------------------------------------------------------------------------
%% Target
%% ---------------------------------------------------------------------------
target_new_with_default_account(Target) ->
    case target_new(Target) of
        {ok, Info} ->
            case edog_conf:iscsi_chap() of
                true ->
                    User = edog_conf:iscsi_user(),
                    Pass = edog_conf:iscsi_pass(),
                    account_new(Target, User, Pass);
                false ->
                    {ok, Info}
            end;
        {error, Info} ->
            {error, Info}
    end.

target_new(Target) ->
    Cmd = io_lib:format("~s/uss.tgtadm --mode target --op new --target ~s", [?YFS_BIN, Target]),
    case cclib_cmd:exec_debug(Cmd) of
        {ok, #cmd_info{}=Info} ->
            {ok, Info};
        {error, #cmd_info{code=?E_FILE_EXISTS}=Info} ->
            ?WARN(Info),
            {ok, Info};
        {error, Info} ->
            ?ERROR(Info),
            {error, Info}
    end.

target_del(Target) ->
    Cmd = io_lib:format("~s/uss.tgtadm --mode target --op del --target ~s", [?YFS_BIN, Target]),
    cclib_cmd:exec_debug(Cmd).

target_list() ->
    Cmd = io_lib:format("~s/uss.tgtadm --mode target --op list", [?YFS_BIN]),
    cclib_cmd:exec_debug(Cmd).

%% ---------------------------------------------------------------------------
%% open_iscsi
%% ---------------------------------------------------------------------------
target_discover() ->
    target_discover([]).

target_discover(Opts) ->
    Ip = proplists:get_value(ip, Opts, ?ISCSI_HOST),
    Cmd = io_lib:format("iscsiadm -m discovery -t sendtargets -p ~s", [Ip]),
    case cclib_cmd:exec_debug(Cmd, ?ISCSI_OP_TIMEOUT) of
        {ok, Info} ->
            {ok, Info};
        {error, Info} ->
            ?ERROR(Info),
            {error, Info}
    end.

target_login(Target) ->
    target_login(Target, []).

target_login(Target, Opts) ->
    Ip = proplists:get_value(ip, Opts, ?ISCSI_HOST),
    Iqn = proplists:get_value(iqn, Opts, default_iqn()),
    Cmd = io_lib:format("iscsiadm -m node -T ~s:~s -p ~s -l", [Iqn, Target, Ip]),
    case cclib_cmd:exec_debug(Cmd, ?ISCSI_OP_TIMEOUT) of
        {ok, Info} ->
            {ok, Info};
        {error, Info} ->
            ?ERROR(Info),
            {error, Info}
    end.

target_logout(Target) ->
    target_logout(Target, []).

target_logout(Target, Opts) ->
    Ip = proplists:get_value(ip, Opts, ?ISCSI_HOST),
    Iqn = proplists:get_value(iqn, Opts, default_iqn()),
    Cmd = io_lib:format("iscsiadm -m node -T ~s:~s -p ~s -u", [Iqn, Target, Ip]),
    case cclib_cmd:exec_debug(Cmd, ?ISCSI_OP_TIMEOUT) of
        {ok, Info} ->
            {ok, Info};
        {error, Info} ->
            ?ERROR(Info),
            {error, Info}
    end.

is_target_login(Target) ->
    is_target_login(Target, []).

is_target_login(Target, Opts) ->
    is_lun_exists(Target, ?ISCSI_LUN, Opts).

%% ---------------------------------------------------------------------------
%% Lun
%% ---------------------------------------------------------------------------
lun_new(Target, LunId, Size) ->
    lun_new(Target, LunId, Size, []).

lun_new(Target, LunId, Size, Opts) ->
    AliasOpt =
    case proplists:get_value(alias, Opts) of
        undefined -> "";
        Alias     -> "--alias " ++ Alias
    end,
    Cmd = io_lib:format("~s/uss.tgtadm --mode lun --op new --target ~s --lun ~B -s ~BG ~s", [
            ?YFS_BIN, Target, LunId, Size, AliasOpt]),
    case cclib_cmd:exec_debug(Cmd) of
        {ok, #cmd_info{}=Info} ->
            {ok, Info};
        {error, #cmd_info{code=?E_FILE_EXISTS} = Info} ->
            {ok, Info};
        {error, Info} ->
            {error, Info}
    end.

lun_del(Target, LunId) ->
    Cmd = io_lib:format("~s/uss.tgtadm --mode lun --op del --target ~s --lun ~B", [?YFS_BIN, Target, LunId]),
    cclib_cmd:exec_debug(Cmd).

lun_list(Target) ->
    Cmd = io_lib:format("~s/uss.tgtadm --mode lun --op list --target ~s", [?YFS_BIN, Target]),
    cclib_cmd:exec_debug(Cmd).

lun_path(Target, LunId) ->
    Opts = [{iqn, default_iqn()}, {ip, ?ISCSI_HOST}],
    lun_path(Target, LunId, Opts).

lun_path(Target, LunId, Opts) ->
    Ip = proplists:get_value(ip, Opts, ?ISCSI_HOST),
    Iqn = proplists:get_value(iqn, Opts, default_iqn()),
    io_lib:format("~s/ip-~s:~B-iscsi-~s:~s-lun-~B", [
            ?ISCSI_ROOT, Ip, ?ISCSI_PORT, Iqn, Target, LunId]).

is_lun_exists(Target, LunId) ->
    is_lun_exists(Target, LunId, []).

is_lun_exists(Target, LunId, Opts) ->
    Link = lun_path(Target, LunId, Opts),
    is_link_exists(Link).

is_lun_ready(Target, LunId) ->
    is_lun_exists(Target, LunId).

is_lun_ready(Target, LunId, Opts) ->
    is_lun_exists(Target, LunId, Opts).

ensure_lun(Target) ->
    ensure_lun(Target, ?ISCSI_LUN).

ensure_lun(Target, LunId) ->
    ensure_lun(Target, LunId, []).

ensure_lun(Target, LunId, Opts) ->
    case is_lun_ready(Target, LunId, Opts) of
        true ->
            true;
        false ->
            target_discover(Opts),
            target_logout(Target, Opts),
            case target_login(Target, Opts) of
                {ok, #cmd_info{}} ->
                    wait_for_link(Target, LunId, Opts);
                {error, #cmd_info{}=Info} ->
                    ?ERROR(Info),
                    false
            end
    end.

-define(WAIT_FOR_LINK_TIMEOUT, 16).

wait_for_link(Target, LunId, Opts) ->
    wait_for_link(Target, LunId, Opts, ?WAIT_FOR_LINK_TIMEOUT).

wait_for_link(Target, LunId, Opts, 0) ->
    is_lun_ready(Target, LunId, Opts);
wait_for_link(Target, LunId, Opts, N) ->
    case is_lun_ready(Target, LunId, Opts) of
        true ->
            ?INFO({ensure_lun, Target, LunId, Opts, N}),
            true;
        false ->
            ?WARN({ensure_lun, Target, LunId, Opts, N}),
            timer:sleep(1000),
            wait_for_link(Target, LunId, Opts, N-1)
    end.

%% ---------------------------------------------------------------------------
%% account
%% ---------------------------------------------------------------------------
account_new(Target, User, Password) ->
    account_new(Target, User, Password, ?DIRECTION_IN).

account_new(Target, User, Password, Direction) ->
    Cmd = io_lib:format("~s/uss.tgtadm --mode account --op new --target ~s --direction ~s --user ~s --pass ~s",
        [?YFS_BIN, Target, Direction, User, Password]),
    case cclib_cmd:exec_debug(Cmd) of
        {ok, Info} ->
            {ok, Info};
        {error, Info} ->
            ?ERROR(Info),
            {error, Info}
    end.

account_del(Target, Direction) ->
    Cmd = io_lib:format("~s/uss.tgtadm --mode account --op del --target ~s --direction ~s",
        [?YFS_BIN, Target, Direction]),
    case cclib_cmd:exec_debug(Cmd) of
        {ok, Info} ->
            {ok, Info};
        {error, Info} ->
            ?ERROR(Info),
            {error, Info}
    end.

account_list(Target, Direction) ->
    Cmd = io_lib:format("~s/uss.tgtadm --mode account --op list --target ~s --direction ~s",
        [?YFS_BIN, Target, Direction]),
    case cclib_cmd:exec_debug(Cmd) of
        {ok, Info} ->
            {ok, Info};
        {error, Info} ->
            ?ERROR(Info),
            {error, Info}
    end.

%% ---------------------------------------------------------------------------
%% PRIVATE
%% ---------------------------------------------------------------------------
%% GC
is_target_free(Target) ->
    is_lun_free(Target, ?ISCSI_LUN).


is_link_exists(Link) ->
    case file:read_link_info(Link) of
        {ok, #file_info{}} ->
            true;
        {error, _Reason} ->
            false
    end.


is_lun_free(Target, LunId) ->
    LunPath = lists:flatten(lun_path(Target, LunId)),
    is_file_free(LunPath).


-define(QEMU, "qemu-kvm").

is_file_free(File) ->
    %L = edog_common:proc_info(filename:basename(edog_conf:bin_kvm())),
    L = edog_common:proc_info(?QEMU),
    F = fun(#proc_info{cmd=Cmd}) ->
        Cmd2 = lists:flatten(Cmd),
        case re:run(Cmd2, File) of
            nomatch -> true;
            _       -> false
        end
    end,
    lists:all(F, L).


%% @deprecated
is_file_free2(File) ->
    Cmd = io_lib:format("lsof ~s", [File]),
    case cclib_cmd:exec(Cmd) of
        {error, #cmd_info{code=1, is_timeout=false}} ->
            true;
        _ ->
            false
    end.


target_gc(Target) ->
    case is_lun_exists(Target, ?ISCSI_LUN) of
        true ->
            case is_lun_free(Target, ?ISCSI_LUN) of
                true ->
                    ?INFO({logout, Target, iolist_to_binary(lun_path(Target, ?ISCSI_LUN))}),
                    target_logout(Target);
                false ->
                    ok
            end;
        false ->
            ok
    end.

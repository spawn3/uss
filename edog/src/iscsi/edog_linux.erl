-module(edog_linux).
-compile(export_all).

-include("edog_common.hrl").

%% node.session.timeo.replacement_timeout = 300
%% node.conn[0].timeo.noop_out_timeout = 300
%%
-define(NODE_SESSION_TIMEO_REPLACEMENT_TIMEOUT, "node.session.timeo.replacement_timeout").
-define(NODE_CONN_TIMEO_NOOP_OUT_TIMEOUT, "node.conn[0].timeo.noop_out_timeout").
-define(NODE_SESSION_AUTH_AUTHMETHOD, "node.session.auth.authmethod").
-define(NODE_SESSION_AUTH_USERNAME, "node.session.auth.username").
-define(NODE_SESSION_AUTH_PASSWORD, "node.session.auth.password").

is_ready() ->
    case edog_conf:check_sys() of
        true ->
            L = edog_linux:check_all(),
            case lists:all(fun(E) -> E =:= true end, L) of
                true  -> true;
                false -> false
            end;
        false ->
            true
    end.

check_all() ->
    L = [
        {is_iscsi_meet,    iscsi,    {edog_conf:iscsi_node_session_timeo_replacement_timeout(),
                edog_conf:iscsi_node_conn_timeo_noop_out_timeout()}},
        {is_dist_meet,     dist,     edog_conf:sys_dist()},
        {is_release_meet,  release,  edog_conf:sys_release()},
        {is_kernel_meet,   kernel,   edog_conf:sys_kernel()}
        %{is_hugepage_meet, hugepage, edog_conf:sys_hugepage()}
    ],
    F = fun({IsMeetF, GetF, Required}) ->
        Value = ?MODULE:GetF(),
        ?INFO({system_check, GetF, Required, Value}),
        case apply(?MODULE, IsMeetF, [Required]) of
            true ->
                true;
            false ->
                ?WARN({system_check_error, GetF, Required, Value}),
                false
        end
    end,
    [F(X) || X <- L].

%% > 2.6.32
%% 2.6.32-71.el6.x86_64
is_kernel_meet(Required) when is_tuple(Required) ->
    kernel() >= Required.

is_dist_meet(Required) ->
    dist() =:= Required.

is_release_meet(Required) ->
    release() >= Required.

is_hugepage_meet(Required) ->
    hugepage() =:= Required.

is_iscsi_meet(Required) ->
    Iscsi = iscsi(),
    ?INFO({get, Iscsi}),
    ?INFO({required, Required, "CHAP", edog_conf:iscsi_user(), edog_conf:iscsi_pass()}),
    V1 = proplists:get_value(?NODE_SESSION_TIMEO_REPLACEMENT_TIMEOUT, Iscsi, 0),
    V2 = proplists:get_value(?NODE_CONN_TIMEO_NOOP_OUT_TIMEOUT, Iscsi, 0),
    case {V1, V2} >= Required of
        true ->
            case edog_conf:iscsi_chap() of
                true ->
                    Method = proplists:get_value(?NODE_SESSION_AUTH_AUTHMETHOD, Iscsi),
                    User = proplists:get_value(?NODE_SESSION_AUTH_USERNAME, Iscsi),
                    Pass = proplists:get_value(?NODE_SESSION_AUTH_PASSWORD, Iscsi),
                    {Method, User, Pass} =:= {"CHAP", edog_conf:iscsi_user(), edog_conf:iscsi_pass()};
                false ->
                    true
            end;
        false ->
            false
    end.

iscsi() ->
    Res = os:cmd("grep '^node' /etc/iscsi/iscsid.conf"),
    L = string:tokens(Res, "\n"),
    F = fun(Line) ->
        case string:tokens(Line, " =") of
            [?NODE_SESSION_TIMEO_REPLACEMENT_TIMEOUT, Value] ->
                {?NODE_SESSION_TIMEO_REPLACEMENT_TIMEOUT, list_to_integer(Value)};
            [?NODE_CONN_TIMEO_NOOP_OUT_TIMEOUT, Value] ->
                {?NODE_CONN_TIMEO_NOOP_OUT_TIMEOUT, list_to_integer(Value)};
            [?NODE_SESSION_AUTH_AUTHMETHOD, Value] ->
                {?NODE_SESSION_AUTH_AUTHMETHOD, Value};
            [?NODE_SESSION_AUTH_USERNAME, Value] ->
                {?NODE_SESSION_AUTH_USERNAME, Value};
            [?NODE_SESSION_AUTH_PASSWORD, Value] ->
                {?NODE_SESSION_AUTH_PASSWORD, Value};
            _ ->
                false
        end
    end,
    KVList = [F(X) || X <- L],
    [{K,V} || {K, V} <- KVList].

%% CentOS
dist() ->
    Res = os:cmd("lsb_release -i"),
    case string:tokens(Res, ": \t\n") of
        ["Distributor", "ID", Dist] ->
            Dist;
        Other ->
            Other
    end.

%% 6.0+
release() ->
    Res = os:cmd("lsb_release -r"),
    case string:tokens(Res, ": \t\n") of
        ["Release", Version] ->
            release_to_tuple(Version);
        Other ->
            ?ERROR(Other),
            {0,0}
    end.

release_to_tuple(Version) ->
    [Major, Minor|_] = string:tokens(Version, "."),
    {list_to_integer(Major), list_to_integer(Minor)}.

kernel() ->
    Res = os:cmd("uname -r"),
    case string:tokens(Res, "-. \t\n") of
        [V1,V2,V3|_] ->
            {list_to_integer(V1), list_to_integer(V2), list_to_integer(V3)};
        _L ->
            {0,0,0}
    end.

%% always madvise [never]
hugepage() ->
    Res = os:cmd("cat /sys/kernel/mm/redhat_transparent_hugepage/enabled"),
    case string:tokens(Res, " \n") of
        ["always", "madvise", "[never]"] ->
            never;
        ["always", "[never]"] ->
            never;
        Other ->
            Other
    end.

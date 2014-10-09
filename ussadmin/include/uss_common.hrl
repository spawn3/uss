-ifndef(__EDOG_COMMON_HRL__).
-define(__EDOG_COMMON_HRL__, true).

-include("cclib.hrl").

-include("uss_const.hrl").
-include("uss_json.hrl").

%%--------------------------------------------------------------
%% types
%%--------------------------------------------------------------
-type uuid() :: string().
-type ip()   :: string().
-type filename() :: string().
-type return_t() :: {ok, _} | {error, _}.
-type return_ok_t() :: ok | {error, _}.
-type nodename() :: {atom(), node()}.
-type pmstate_t() :: 'unavailable'|'running'|'shutoff'.

%%--------------------------------------------------------------
%% records
%--------------------------------------------------------------
-record(conf_item, {
        catagory,
        key,
        value,
        doc,
        type,
        mode
    }).

-record(yfs_conf, {
        cluster_name,
        network,
        mask,
        c60_count,
        mds_count,
        cds_count,
        client_count
    }).

-record(sql_query, {
        table,
        fields,
        where,
        limit,
        orderby
    }).

-record(node_request, {
        id,
        rack,
        ip,
        hostname,
        user,
        password
    }).

-record(rack_request, {
        id,
        name
    }).

-record(uss_rack_t, {
        id    :: integer(),
        name  :: string(),
        atime :: integer()
    }).

-record(pm_if, {
        ifname,
        addr,
        bcast,
        mask
    }).

-record(pm_info, {
        hostname,
        uptime,
        cpu,
        mem,
        disk,
        network,
        netflow
    }).

-record(uss_pm_t, {
        id = -1            :: integer(),
        cluster = -1       :: integer(),
        rack = -1          :: integer(),     % FK
        ip                 :: binary(),
        hostname           :: binary(),
        user               :: binary(),
        password           :: binary(),
        atime = 0          :: integer(),
        status = 'shutoff' :: pmstate_t(),
        time = 0           :: integer(),
        flag = 0           :: integer(),
        c60_info = false   :: boolean(),
        mds_info = false   :: boolean(),
        nfs_info = false   :: boolean(),
        proxy_info = false :: boolean(),
        info = []
    }).

% FOR edog_mnesia:select OUTPUT
-record(node, {
        id = -1            :: integer(),
        cluster,
        rack = -1          :: integer(),
        ip                 :: binary(),
        hostname           :: binary(),
        user               :: binary(),
        password           :: binary(),
        atime = 0          :: integer(),
        status
    }).

-type service_type() ::
    'c60' |
    'mds' |
    'cds' |
    'ynfs' |
    'cifs' |
    'yftp' |
    'http' |
    'proxy'.

-type service_id() :: {binary(), service_type(), integer()}.
-type yfs_status() :: 'shutoff'|'starting'|'running'|'stopping'.

-record(service_id, {
        ip   :: binary(),
        type :: service_type(),
        n    :: integer()
    }).

-record(yfs_info, {
        home,
        cache_size,
        cache_hit,
        diskuse
    }).

-record(yfs_analysis, {
        job_name,
        job_count,
        avg_latency,
        max_latency
    }).

% TODO
-record(uss_yfs_t, {
        sid              :: service_id(),
        status = shutoff :: yfs_status(),
        pid = -1,
        info             :: #yfs_info{},
        analysis = []    :: #yfs_analysis{}
    }).

% return to web
-record(service, {
        ip,
        type,
        n,
        status,
        pid,
        info,
        analysis
    }).

-record(uss_option_t, {
        key,
        value
        %type = 'dynamic' :: 'dynamic' | 'static'
    }).

-record(uss_queue_t, {
        id,
        et,
        event,
        info
    }).

%% ------------------------------------------------------
%% volume management
%% ------------------------------------------------------
-record(volume, {
        name,
        id,
        size,
        fileid
    }).

%% ------------------------------------------------------
%% user management
%% ------------------------------------------------------
-record(group, {
        id,
        parent,
        name,
        permission,
        atime
    }).

-record(user, {
        id,
        name,
        pwhash,
        group,
        atime
    }).

-record(role, {
        id,
        perm_map
    }).

-record(user_role, {
        id                 %{uid,rid}
    }).

-define(EVENT(Event),
    begin
        ?tty_report("EVENT", Event),
        gen_event:notify(uss_event, {{?MODULE, ?LINE}, Event})
    end).

%%%%%%%%%%%%
% END
-endif.

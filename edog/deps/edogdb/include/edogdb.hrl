-ifndef(__EDOGDB_HRL__).
-define(__EDOGDB_HRL__, true).

-include("cclib.hrl").

%% --------------------------------------------------------------
%% INTERVAL
%% --------------------------------------------------------------
-define(EDOGDB_MONITOR_START_INTERVAL, 10000).
-define(EDOGDB_MONITOR_LOOP_INTERVAL,  10000).

-define(EDOGDB_CLUSTER_START_INTERVAL, 30000).
-define(EDOGDB_CLUSTER_LOOP_INTERVAL,  10000).

%% --------------------------------------------------------------
%% TYPES
%% --------------------------------------------------------------
-type uuid() :: string().
-type pmstate_t() :: 'undefined' | 'running'.
-type vmstate_t() ::
        'undefined' |
        'shutoff' |
        'paused' |
        'running' |
        'creating'.
-type diskstate_t() :: 'undefined' | 'creating' | 'created' | 'locked'.

%% --------------------------------------------------------------
%% records
%% --------------------------------------------------------------
-record(option_t, {
        key,
        value
    }).

-record(pm_t, {
        %% ip, need to support ip reconfigablity
        pm_id              :: uuid(),          % ip
        pm_ip              :: string(),        % vm count
        pm_cpu = []        :: list() ,
        pm_mem = []        :: list(),
        pm_bridges = []    :: list(),
        status = running   :: pmstate_t()
    }).

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

-record(disk_t, {
        disk_id              :: uuid(),
        disk_alias           :: string(),
        cust_id              :: uuid(),
        path                 :: string(),
        % GB
        size        = 10     :: integer(),
        canboot = [{boot, false}] :: list(), %% opts
        vm_id                :: uuid(),
        target               :: string(),
        createtime  = 0      :: integer(),
        locktime    = 0      :: integer(),
        status               :: diskstate_t()
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
        % [{start_backup_if_fails, false}, {backup, false}]
        extra = []
    }).

-record(disk_vm_t, {
        id,                  % {disk_id, vm_id}
        target,
        extra = []
    }).

%%
-record(nic_t, {
        bridge = "",
        mac    = "",
        model  = "e1000"
    }).

-endif.

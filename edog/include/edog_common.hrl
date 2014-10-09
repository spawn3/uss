-ifndef(__EDOG_COMMON_HRL__).
-define(__EDOG_COMMON_HRL__, true).

-include("cclib.hrl").
-include("edogdb.hrl").
-include("edog_const.hrl").
-include("edog_debug.hrl").
-include("edog_types.hrl").
-include("edog_yfs.hrl").

-define(VM_STATE_UNDEFINED,  undefined).
-define(VM_STATE_CREATING,   creating).
-define(VM_STATE_SHUTOFF,    shutoff).
-define(VM_STATE_RUNNING,    running).
-define(VM_STATE_PAUSED,     paused).
-define(VM_STATE_PROCESSING, processing).

-define(DISK_STATE_CREATING, creating).
-define(DISK_STATE_CREATED,  created).
-define(DISK_STATE_LOCKED,   locked).

-record(freeip_t, {
        ip,
        is_public
    }).

-record(usedip_t, {
        ip,
        is_public,
        bridge,
        mac,
        vmid,
        vnet,
        tx,
        rx
    }).

-type yfs_server_flag() ::
    'c60' |
    'mds' |
    'cds' |
    'ynfs' |
    'yftp' |
    'cifs' |
    'proxy'.

-type yfs_server() :: {yfs_server_flag(), string(), integer()}.

-record(yfs_server, {
        type :: yfs_server_flag(),
        ip  :: string(),
        num :: integer()
    }).

-record(yfs_t, {
        server :: yfs_server(),
        status = off  :: 'off' | 'on',
        pidlist = [],
        info = []
    }).

-type notify_mod() ::
        'undefined' |
        'pm' |
        'yfs' |
        'vm'.

-type notify_op() ::
        'undefined' |
        'pm_up' |
        'pm_down' |
        'yfs_cp' | 'disk_create' |
        'vm_create' | 'vm_destroy' | 'vm_update' |
        'vm_start' | 'vm_stop' |
        'vm_pause' | 'vm_resume' |
        'vm_migrate'.

-record(notify_spec, {
        op      :: notify_op(),
        key     :: string(),
        reply   :: {ok, _} | {error, _}
    }).

%% LIBVIRT
-record(domain, {
        name    :: string(),
        uuid    :: string(),
        id      :: integer(),
        status  :: domainstate_t(),
        %%
        cpu = 0,
        mem = 0,
        pid = -1
    }).

-record(proc_info, {
        pid  = -1,
        cpu  = 0,
        mem  = 0,
        uuid = "",
        cmd  = ""
    }).

-type load_flag() :: 'avg1' | 'avg5' | 'avg15' | 'max_memory'.

-record(load_policy, {
        on = false      :: boolean(),
        timeout = 10000 :: integer(),
        flag = avg5     :: load_flag(),
        min  = 256      :: integer(),
        diff = 100      :: integer()
    }).

-record(vm_state, {
        actor,
        key,
        prev_state,
        init_op,
        context
    }).

-endif.

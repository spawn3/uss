-ifndef(__EDOG_CONST_HRL__).
-define(__EDOG_CONST_HRL__, true).

-define(APPLICATION,  edog).
-define(DIST_NAME, edog_master).

-define(APP_SRC,    "/sysy/yfs/ussadmin/edog").
-define(DEFAULT_LISTEN_PORT, 9501).

-define(YFS_PREFIX, "/sysy/yfs/").
-define(YFS_APP,    "/sysy/yfs/app/").
-define(YFS_BIN,    "/sysy/yfs/app/bin").
-define(YFS_SBIN,   "/sysy/yfs/app/sbin").

-ifdef(use_as_plugin).

-define(MASTERNAME,   "ussadmin_master").
-define(AGENTNAME,    "ussadmin").
-define(APP_DATA,     "/sysy/yfs/ussadmin/data").
-define(LIBVIRTD_PID, "/sysy/yfs/ussadmin/data/libvirtd.pid").

-else.

-define(MASTERNAME,   "edog_master").
-define(AGENTNAME,    "edog").
-define(APP_DATA,     "/sysy/yfs/ussadmin/edog_runtime").
-define(LIBVIRTD_PID, "/sysy/yfs/ussadmin/edog_runtime/libvirtd.pid").

-endif.

-define(DB_BACKUP_INTERVAL_MIN, 600).

%-define(QEMU_LOG_PATH,     "/sysy/yfs/libvirtd77/var/log/libvirt/qemu").
-define(QEMU_LOG_PATH,     "/var/log/libvirt/qemu").

-define(ISCSI_ROOT, "/dev/disk/by-path").
-define(ISCSI_HOST, "127.0.0.1").
-define(ISCSI_PORT, 3260).
-define(ISCSI_LUN,  0).

%% CHAP
-define(ISCSI_USER, "admin").
-define(ISCSI_PASS, "mdsmds").

%% Error Code
-define(E_NO_SUCH_FILE, 2).
-define(E_FILE_EXISTS, 17).

-define(SYS_DIST,     "CentOS").
-define(SYS_RELEASE,  {6,0}).
-define(SYS_KERNEL,   {2,6,32}).
-define(SYS_HUGEPAGE, never).

%% --------------------------------------------------------------
%% INTERVAL
%% --------------------------------------------------------------
-define(EDOG_MASTER_START_INTERVAL, 1000).
-define(EDOG_MASTER_LOOP_INTERVAL,  10000).

-define(EDOG_AGENT_START_INTERVAL, 30000).
-define(EDOG_AGENT_LOOP_INTERVAL,  15000).

-endif.

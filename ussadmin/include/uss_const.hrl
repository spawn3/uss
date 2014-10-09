-ifndef(__USS_CONST_HRL__).
-define(__USS_CONST_HRL__, true).

-define(YFS_PREFIX,     "/sysy/yfs/").
-define(YFS_LOG_PREFIX, "/var/log/uss").
-define(SKIP_ME,        "__skip_me__").

-define(APPLICATION,   ussadmin).
-define(MASTER_NAME,   "ussadmin_master").
-define(NODE_NAME,     "ussadmin").

-define(USS_MANAGER,   uss_manager).

-define(CLUSTER_NULL, -1).
-define(CLUSTER_0,    0).

-define(LOG_DEFAULT_LINES, 30).
-define(LOG_MAX_LINES,     100).

-define(TIMEOUT_LVM,       10000).
-define(TIMEOUT_YFS_START, 60).
-define(TIMEOUT_YFS_STOP,  60).

%% RPC
-define(EDOG_AGENT_TIMEOUT, 30).

-endif.

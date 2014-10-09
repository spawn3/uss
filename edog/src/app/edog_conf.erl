-module(edog_conf).
-compile(export_all).

-include("edog_common.hrl").

% BINARY
-define(VIRSH,    "/sysy/yfs/libvirtd77/bin/virsh").
-define(LIBVIRTD, "/sysy/yfs/libvirtd77/sbin/libvirtd").
-define(KVM,      "/sysy/yfs/kvm/kvm88patch/bin/qemu-system-x86_64").
-define(KVM_IMG,  "/sysy/yfs/kvm/kvm88patch/bin/qemu-img").

% STORAGE
-define(STORAGE_PREFIX, "/oss/").

all() ->
    L = [
        check_sys,
        iscsi_iqn,
        iscsi_chap,
        iscsi_user,
        iscsi_pass,

        target_gc,
        target_gc_interval,

        bin_kvm,
        bin_kvm_img,
        bin_virsh,
        bin_libvirtd,
        %
        storage_module,
        storage_prefix,
        copyfile_timeout,
        %
        manager_failover_wait_factor,
        agent_kill_vm_factor,
        manager_update_db_factor,
        %
        db_backup_interval,
        cluster_name,
        listen,
        %
        policy_if_vm_fails,
        system_preserved_memory,
        vm_restart_memory_policy,
        vm_restart_memory,
        vm_restart_try_previous
    ],
    [{X, erlang:apply(?MODULE, X, [])} || X <- L].

check_sys() -> get_env(check_sys, true).

%
bin_kvm()         -> get_env(bin_kvm, ?KVM).
bin_kvm_img()     -> get_env(bin_kvm_img, ?KVM_IMG).
bin_virsh()       -> get_env(bin_virsh, ?VIRSH).
bin_libvirtd()    -> get_env(bin_libvirtd, ?LIBVIRTD).
%

%
manager_failover_wait_factor() -> get_env(manager_failover_wait_factor, 1).
agent_kill_vm_factor() -> get_env(agent_kill_vm_factor, 2).
manager_update_db_factor() -> get_env(manager_update_db_factor, 4).

cluster_name() -> get_env(cluster_name, "").

listen() -> get_env(listen, ?DEFAULT_LISTEN_PORT).

db_backup_interval() -> get_env(db_backup_interval, 7200). % s

%%
policy_if_vm_fails() -> get_env(policy_if_vm_fails, update_db).
vm_restart_try_previous() -> get_env(vm_restart_try_previous, true).

%% fixed | max_available | _
vm_restart_memory_policy() -> get_env(vm_restart_memory_policy, fixed).
system_preserved_memory() -> get_env(system_preserved_memory, 4096).
vm_restart_memory() -> get_env(vm_restart_memory, 1024).
vm_restart_memory_min() -> get_env(vm_restart_memory_min, 500).

storage_prefix() -> get_env(storage_prefix, ?STORAGE_PREFIX).

%% edog_storage_proxy | edog_storage_iscsi
storage_module() -> get_env(storage_module, edog_storage_iscsi).

iscsi_iqn() -> get_env(iscsi_iqn, "iqn.2001-04.com.meidisen").
iscsi_node_session_timeo_replacement_timeout() -> get_env(iscsi_node_session_timeo_replacement_timeout, 300).
iscsi_node_conn_timeo_noop_out_timeout() -> get_env(iscsi_node_conn_timeo_noop_out_timeout, 300).
iscsi_chap() -> get_env(iscsi_chap, false).
iscsi_user() -> get_env(iscsi_user, ?ISCSI_USER).
iscsi_pass() -> get_env(iscsi_pass, ?ISCSI_PASS).

target_gc() -> get_env(target_gc, true).
target_gc_interval() -> get_env(target_gc_interval, 600000).

sys_dist() -> get_env(sys_dist, ?SYS_DIST).
sys_release() -> get_env(sys_release, ?SYS_RELEASE).
sys_kernel() -> get_env(sys_kernel, ?SYS_KERNEL).
sys_hugepage() -> get_env(sys_hugepage, ?SYS_HUGEPAGE).

copyfile_timeout() -> get_env(copyfile_timeout, 1800).

debug_level() -> get_env(debug, disable).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE
get_env(Key, Default) ->
    case application:get_env(?APPLICATION, Key) of
        {ok, Value} -> Value;
        _           -> Default
    end.

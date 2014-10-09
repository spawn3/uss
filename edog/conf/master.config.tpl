[
{kernel, [
        %%
	{edog_masters, [
		'edog_master@192.168.1.14',
		'edog_master@192.168.1.15'
		]},
        {net_ticktime, 8},
	{distributed, [{edog, [{'edog_master@192.168.1.14',
		'edog_master@192.168.1.15'
		}]}]},
	{sync_nodes_mandatory, [
		'edog_master@192.168.1.15'
		]},
	{sync_nodes_timeout, 180000}
	]},
{sasl, [
	%{error_logger_mf_dir, "/var/log/ussadmin/edog_runtime/manager_logs"},
	%{error_logger_mf_maxbytes, 10485760},
	%{error_logger_mf_maxfiles, 10},
	{errlog_type, error},
	{sasl_error_logger, tty}
	]},
{mnesia, [
        {dir, "/sysy/yfs/ussadmin/edog_runtime/db"}
        ]},
{edog, [
        {check_sys, true},
        {debug, disable},

        %% BOTH (manager and agent)
        {cluster_name, ""},
	{disk_format, "raw"},
        {storage_module, edog_storage_iscsi},
	{storage_prefix, "/oss/"},
        {copyfile_timeout, 36000},          % seconds

        %% iscsi
        {iscsi_iqn, "iqn.2001-04.com.meidisen"},
        {iscsi_chap, true},
        {iscsi_user, "admin"},
        {iscsi_pass, "mdsmdsmdsmds"},
        {target_gc, true},
        {target_gc_interval, 60000},       % milliseconds

        %% manager ONLY
        {db_backup_interval, 7200},        % seconds

        % update_db | restart
        {system_preserved_memory, 2048},   % MB
        {policy_if_vm_fails, restart},
        {vm_restart_try_previous, true},
        % raw | fixed | max_available
        {vm_restart_memory_policy, fixed},
        {vm_restart_memory, 1024},         % MB

        % HA factor
        {manager_failover_wait_factor, 1},
        {manager_update_db_factor, 5},

        %% agent ONLY
        {agent_kill_vm_factor, 2},

        %% READ-ONLY

	{ws_ip, "127.0.0.1"},
	{ws_port, 9500},
	{listen, 9501},

        {bin_kvm, "/usr/libexec/qemu-kvm"},
        {bin_kvm_img, "/usr/bin/qemu-img"},
        {bin_virsh, "/usr/bin/virsh"},
        {bin_libvirtd, "/usr/sbin/libvirtd"},
	%{bin_kvm, "/sysy/yfs/kvm/kvm88patch/bin/qemu-system-x86_64"},
	%{bin_kvm_img, "/sysy/yfs/kvm/kvm88patch/bin/qemu-img"},
	%{bin_virsh, "/sysy/yfs/libvirtd77/bin/virsh"},
	%{bin_libvirtd, "/sysy/yfs/libvirtd77/sbin/libvirtd"},

        {asmaster, 1}
	]}
].

[
{kernel, [
	{distributed, [{ussadmin, [{
                'ussadmin_master@192.168.1.14',
		'ussadmin_master@192.168.1.15'
		}]}]},
	{sync_nodes_mandatory, [
		'ussadmin_master@192.168.1.15'
		]},
	{sync_nodes_timeout, 15000},
        {net_tick_timeout, 60},
	{managers, [
		'ussadmin_master@192.168.1.14',
		'ussadmin_master@192.168.1.15'
		]},

	{error_logger, {file, "/sysy/yfs/ussadmin/data/manager.log"}}
	]},
{sasl, [
	{sasl_error_logger, false},
	{errlog_type, error},
	{error_logger_mf_dir, "/sysy/yfs/ussadmin/data/manager_logs"},
	{error_logger_mf_maxbytes, 10485760},
	{error_logger_mf_maxfiles, 10}
	]},
{mnesia, [
    {dir, "/sysy/yfs/ussadmin/data/edog_masters"}
    ]},
{ussadmin, [
        {asmaster, 1},
        %
	{enable_nfs, true},
	{enable_proxy, true},

        {yfs_stop_wait, 30},
        {yfs_stop_cds_wait, 300},

        % front-end
	{ws_ip, "127.0.0.1"},
	{ws_port, 9600},

	{manager_admin_email, "admin@meidisen.com"},

        %
	{manager_web_port, 9601},

        % trace
	{manager_trace_port, 9602},
	{agent_trace_port, 9603},

	{manager_timeout, 6},
	{manager_nodedown_timeout, 15},

	{agent_timeout, 30}
	]}
].

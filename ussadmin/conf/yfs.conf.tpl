{global, [
    {cluster_name, yfs},
    {c60_port, 10090},
    {chunk_rep, 2},
    {conn_rep, 1},
    {hb_interval, 30},
    {journal_length, "8MB"},
    {use_shm, on},
    {netrpc_reqtimeout, 60},
    {disk_timeout, 60},
    {md_cache_timeout, 3600},
    {yfs_home_dir, "/sysy/yfs"},
    {check_mountpoint, on},
    {maxcore, on},
    {cache_decrease, 10},
    {weaken_interval, 10},
    {network, "192.168.1.0"},
    {mask, "255.255.255.0"},
    {stand_alone, 1},
    {namei_cache, 0},
    {write_back, off},
    {mds_count, 2},
    {itab_recycle, on},
    {performance_analysis, 1},
    {inode_cache_size, "256M"}              % mds.inode_cache_size*3/cds_num
    ]}.

{mds,[
    {chknew_hardend, on},
    {disk_keep, "10G"},
    {mds_inode_cache_size, "512M"},
    {write_balance, on},
    {snapshot, off}
    ]}.

{cds, [
    {write_crc, 0},
    {read_crc, 0},
    {cache_size, "256M"},
    {unlink_async, on}
    ]}.

{ynfs, [
    {rsize, 1048576},
    {wsize, 1048576},
    {use_export, off},
    {export, [{"/", "127.0.0.1", "(rw)"}, {"/", "0.0.0.0", "(rw)"}]}
    ]}.

{yweb, [
    {webport, 80},
    {use_ratelimit, off},
    {rate_limit, [{"/download", "100KB"}]}
    ]}.

{log, [
    {log_ylib, off},
    {log_yliblock, off},
    {log_ylibmem, off},
    {log_ylibskiplist, off},
    {log_ylibnls, off},
    {log_ysock, off},
    {log_ynet, off},
    {log_yrpc, off},
    {log_yfscdc, off},
    {log_yfsmdc, off},
    {log_fsmachine, off},
    {log_yfslib, off},
    {log_yfsfuse, off},
    {log_yweb, off},
    {log_yftp, off},
    {log_yp2p, off},
    {log_ynfs, off},
    {log_yfsmds, off},
    {log_yoss, off},
    {log_cdsmachine, off},
    {log_yfscds, off},
    {log_yfscds_robot, off},
    {log_ytable, off},
    {log_c60, off},
    {log_proxy, off}
    ]}.

{c60,  [
%    {max_histroy_version, 2},
    {db_cache_size, "10MB"},
    {db_lg_max, "100MB"},
    {db_lg_bsize, "64KB"},
    {db_checkpoint_interval, 60},
    {db_records, 10000000},
    {db_verbose, off}
    ]}.

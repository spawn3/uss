-ifndef(__CCLIB_NETWORK_HRL__).
-define(__CCLIB_NETWORK_HRL__, true).

-record(if_info, {
        ifname,       % ifconfig
        mac,
        ip,
        bcast,
        mask,
        rx_bytes,
        tx_bytes,
        speed,        % ethtool
        info
    }).

-record(netflow, {
        nic,
        rx_bytes,
        tx_bytes
    }).

-record(load_info, {
        node,
        time  = 0,
        avg1  = 99999999,
        avg5  = 99999999,
        avg15 = 99999999,
        free  = 0
    }).

-record(uptime, {
        time,
        user
    }).

-record(uptime_info, {
        currtime,
        time,
        user,
        avg1,
        avg5,
        avg15
    }).

-record(cpu_util, {
        id,
        busy,
        idle
    }).

-record(cpu_info, {
        physical_id,
        model_name,
        count
    }).

-record(mem_info, {
        mem_total,
        mem_free,
        mem_cached,
        mem_buffered,
        swap_total,
        swap_free
    }).

-record(disk_info, {
        device,
        mountpoint,
        capacity,
        used,
        avail,
        util,
        smart
    }).

-endif.


-module(cclib_dhcp).
-compile(export_all).

-define(DHCPD_CONF, "/etc/dhcp3/dhcpd.conf").

start() ->
    Cmd = "/etc/init.d/dhcp3-server start",
    edog_common:cmd(Cmd).

stop() ->
    Cmd = "/etc/init.d/dhcp3-server stop",
    edog_common:cmd(Cmd).

restart() ->
    Cmd = "/etc/init.d/dhcp3-server restart",
    edog_common:cmd(Cmd).

add(Mac, IP) ->
    {Mac, IP}.

del(Mac) ->
    Mac.

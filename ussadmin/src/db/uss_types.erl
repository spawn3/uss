-module(uss_types).
-compile(export_all).

-include("uss_common.hrl").

pm_to_node(#uss_pm_t{} = R) ->
    #node{
        id       = R#uss_pm_t.id,
        rack     = R#uss_pm_t.rack,
        ip       = R#uss_pm_t.ip,
        hostname = R#uss_pm_t.hostname,
        user     = R#uss_pm_t.user,
        password = R#uss_pm_t.password,
        atime    = R#uss_pm_t.atime,
        status   = R#uss_pm_t.status,
        cluster  = R#uss_pm_t.cluster
    }.

uss_yfs_to_service(#uss_yfs_t{sid={Ip, Type, N}} = R) ->
    #service{
        ip       = Ip,
        type     = Type,
        n        = N,
        status   = R#uss_yfs_t.status,
        pid      = R#uss_yfs_t.pid,
        info     = R#uss_yfs_t.info,
        analysis = R#uss_yfs_t.analysis
    }.

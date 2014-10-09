-ifndef(__EDOG_YFS_HRL).
-define(__EDOG_YFS_HRL, true).

-record(mdstat, {
        cluster,
        mds_entries
    }).

-record(mds_entry, {
        ip,
        n,
        id,
        status,
        peer,
        sync,
        extra = []
    }).

-endif.

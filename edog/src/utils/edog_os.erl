-module(edog_os).
-compile(export_all).

-include("edog_common.hrl").


%% --------------------------------------------------------------------
%% useless
%% --------------------------------------------------------------------
%%
check_prerequired() ->
    lists:foreach(fun(X) -> filelib:ensure_dir(X) end, [
            ?YFS_PREFIX,
            ?YFS_APP,
            ?YFS_SBIN,
            ?APP_SRC
            ]),

    L = [
        edog_conf:bin_kvm(),
        edog_conf:bin_kvm_img(),
        edog_conf:bin_virsh(),
        edog_conf:bin_libvirtd(),
        "/sysy/yfs/app/bin/ylvm",
        "/sysy/yfs/app/bin/ymkdir",
        "/sysy/yfs/app/bin/ycp",
        "/sysy/yfs/app/sbin/yfs_mds",
        "/sysy/yfs/app/sbin/yfs_cds",
        "/sysy/yfs/app/sbin/proxy_server"
    ],
    [X || X <- L, not cclib_file:file_exist(X)].


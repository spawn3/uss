#!/usr/bin/env bash

OKKFLAG="[OKK]"
ERRFLAG="[ERR]"
LIBDB=libdb-4.8.so
YFS_PREFIX=/sysy/yfs
ERL_VERSION=5.8.2

echo_okk() { echo $OKKFLAG $1; }
echo_err() { echo $ERRFLAG $1; }

check_db()
{
    if ldconfig -p | grep -q "\<$LIBDB\>"; then
        echo_okk $LIBDB
    else
        echo_err $LIBDB
    fi
}

check_yfs()
{
    declare -a YFS_FILES=(
        $YFS_PREFIX/app/sbin/c60d
        $YFS_PREFIX/app/sbin/yfs_mds
        $YFS_PREFIX/app/sbin/yfs_cds
        $YFS_PREFIX/app/sbin/ynfs_server
        $YFS_PREFIX/app/sbin/proxy_server
        $YFS_PREFIX/app/bin/ylvm
        $YFS_PREFIX/app/bin/yls
        $YFS_PREFIX/app/bin/ycp
        )

    for i in ${YFS_FILES[@]}
    do
        if [ -f $i ]; then
            echo_okk $i
        else
            echo_err $i
        fi
    done
}

check_erlang()
{
    if erl -version 2>&1|grep -q "$ERL_VERSION"; then
        echo_okk "erlang $ERL_VERSION"
    else
        echo_err "erlang $ERL_VERSION"
    fi
}

check_network()
{
    echo_okk "not implemented"
}

check_db
check_yfs
check_erlang
check_network


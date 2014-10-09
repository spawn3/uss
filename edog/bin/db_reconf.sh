#!/usr/bin/env bash

APP_ROOT=/sysy/yfs/ussadmin
APP_SRC=$APP_ROOT/edog
APP_DATA=$APP_ROOT/edog_runtime
DB_DIR=$APP_DATA/db
APP_NAME=edog
MANAGER_NAME=edog_master
MAINT_NAME=edog_master_maint
COOKIE=edog

# set -x

# start_dest <dest_ip>
start_dest()
{
    ssh root@$1 erl -name $MANAGER_NAME@$1 \
        -setcookie $COOKIE \
        -detached \
        -pa $APP_SRC/ebin \
        -pa $APP_SRC/deps/*/ebin \
        -mnesia dir "\\\"$DB_DIR\\\""
}

# stop_dest <src_ip> <dest_ip>
stop_dest()
{
    erl -name $MANAGER_NAME@$1 \
        -setcookie $COOKIE \
        -noshell -noinput \
        -eval "rpc:call('$MANAGER_NAME@$2', init, stop, []), init:stop()"
}

show()
{
    erl -name $MANAGER_NAME@$1 \
        -setcookie $COOKIE \
        -pa $APP_SRC/ebin \
        -pa $APP_SRC/deps/*/ebin \
        -mnesia dir "\"$DB_DIR\"" \
        -eval "edog_reconf:show()"
}

# add_node <src_ip> <dest_ip>
add_node()
{
    erl -name $MANAGER_NAME@$1 \
        -setcookie $COOKIE \
        -noshell -noinput \
        -pa $APP_SRC/ebin \
        -pa $APP_SRC/deps/*/ebin \
        -mnesia dir "\"$DB_DIR\"" \
        -eval "edog_reconf:add_node('$MANAGER_NAME@$2'),init:stop()"
}

# del_node <src_ip> <dest_ip>
del_node()
{
    erl -name $MANAGER_NAME@$1 \
        -setcookie $COOKIE \
        -noshell -noinput \
        -pa $APP_SRC/ebin \
        -pa $APP_SRC/deps/*/ebin \
        -mnesia dir "\"$DB_DIR\"" \
        -eval "edog_reconf:del_node('$MANAGER_NAME@$2'),init:stop()"
}

# use when manager is not online.
backup_offline()
{
    DBFILE=$2
    if [ -z $DBFILE ];
    then
        erl -name $MANAGER_NAME@$1 \
            -setcookie $COOKIE \
            -noshell -noinput \
            -pa $APP_SRC/ebin \
            -pa $APP_SRC/deps/*/ebin \
            -mnesia dir "\"$DB_DIR\"" \
            -eval "edog_reconf:do_backup(),init:stop()"
    else
        erl -name $MANAGER_NAME@$1 \
            -setcookie $COOKIE \
            -noshell -noinput \
            -pa $APP_SRC/ebin \
            -pa $APP_SRC/deps/*/ebin \
            -mnesia dir "\"$DB_DIR\"" \
            -eval "edog_reconf:do_backup(\"$DBFILE\"),init:stop()"
    fi
}

## -------------------------------------------------------------
## managers up
## -------------------------------------------------------------
backup()
{
    erl -name $MAINT_NAME@$1 \
        -setcookie $COOKIE \
        -noshell -noinput \
        -hidden \
        -pa $APP_SRC/ebin \
        -pa $APP_SRC/deps/*/ebin \
        -mnesia dir "\"$DB_DIR\"" \
        -eval "edog_reconf:backup('$MANAGER_NAME@$1'),init:stop()"
}

## -------------------------------------------------------------
## managers up
## recover ip txt
## -------------------------------------------------------------
recover()
{
    IP=$1
    if [ -z $2 ]
    then
        erl -name $MAINT_NAME@$IP \
            -setcookie $COOKIE \
            -noshell -noinput \
            -hidden \
            -pa $APP_SRC/ebin \
            -pa $APP_SRC/deps/*/ebin \
            -mnesia dir "\"$DB_DIR\"" \
            -eval "edog_reconf:recover('$MANAGER_NAME@$IP'), init:stop()"
    else
        erl -name $MAINT_NAME@$IP \
            -setcookie $COOKIE \
            -noshell -noinput \
            -hidden \
            -pa $APP_SRC/ebin \
            -pa $APP_SRC/deps/*/ebin \
            -mnesia dir "\"$DB_DIR\"" \
            -eval "edog_reconf:recover('$MANAGER_NAME@$IP', \"$2\"), init:stop()"
    fi
}

usage()
{
    echo "***Usage:"
    echo "    ./cluster_reconf show            src_ip"
    echo "    ./cluster_reconf backup          src_ip [dbfile]"
    #echo "    ./cluster_reconf backup_offline src_ip [dbfile]"
    #echo "    ./cluster_reconf recover        src_ip"
    #echo "    ./cluster_reconf add            src_ip dest_ip"
    #echo "    ./cluster_reconf del            src_ip dest_ip"
}

check_args()
{
    if [ $1 -lt $2 ]
    then
        usage
        exit 1
    fi
}

## ------------------------------------------------------------------
## MAIN
## ------------------------------------------------------------------
ARGCOUNT=$#
ACTION=$1
SRC_IP=$2
DEST_IP=$3
DBFILE=$3

check_args $ARGCOUNT 2

case $ACTION in
    show)
        show $SRC_IP
        ;;
    backup)
        backup_offline $SRC_IP $DBFILE
        ;;
    #backup)
    #    backup $SRC_IP
    #    ;;
    #recover)
    #    shift
    #    recover $*
    #    ;;
    #add)
    #    check_args $ARGCOUNT 3
    #    start_dest $DEST_IP
    #    add_node $SRC_IP $DEST_IP
    #    stop_dest $SRC_IP $DEST_IP
    #    ;;
    #del)
    #    check_args $ARGCOUNT 3
    #    start_dest $DEST_IP
    #    del_node $SRC_IP $DEST_IP
    #    stop_dest $SRC_IP $DEST_IP
    #    ;;
    *)
        usage
        ;;
esac

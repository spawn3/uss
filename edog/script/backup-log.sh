#!/usr/bin/env bash

# set -x

current_dir=`dirname $0`
source $current_dir/application.sh

LOG_SRC_DIR=/var/log/ussadmin/edog_runtime
LOG_DST_DIR=$LOG_SRC_DIR/backup
DEFAULT_MAX_LOG_NUM=100

##########################################################
## private
##########################################################

# usage: log_backup SrcDir DstDir
log_backup()
{
    Dir=$(date +%Y%m%d-%H%M%S)
    mkdir -p $2/$Dir
    #cp -rf $1/*.log $2/$Dir
    rsync -avz $1/*.log $2/$Dir > /dev/null
}

# usage: log_clear DstDir MaxLogNum
log_clear()
{
    LogDir=$1
    MaxLogNum=$2
    Idx=1
    for Dir in $(ls $LogDir|sort -r)
    do
        if [ $Idx -ge $MaxLogNum ]; then
            rm -rf $LogDir/$Dir
        fi
        let Idx=Idx+1
    done
}

##########################################################
## main
##########################################################
backup_log_main()
{
    mkdir -p $LOG_DST_DIR
    log_clear $LOG_DST_DIR $DEFAULT_MAX_LOG_NUM
    log_backup $LOG_SRC_DIR $LOG_DST_DIR

    #echo log_clear $LOG_DST_DIR $DEFAULT_MAX_LOG_NUM
    #echo log_backup $LOG_SRC_DIR $LOG_DST_DIR
}

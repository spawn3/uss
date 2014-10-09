#!/usr/bin/env bash

# set -x

current_dir=`dirname $0`
source $current_dir/application.sh
source $current_dir/erlctl.sh
source $current_dir/backup-log.sh

usage()
{
    echo "Usage:"
    echo "  start_master.sh -s <ip>             # stop"
    echo "  start_master.sh -f <ip> <config>    # fg start"
    echo "  start_master.sh <ip> <config>       # daemon start"
    echo "For example:"
    echo "  start_master.sh 192.168.1.201 master1"
    echo "  start_master.sh 192.168.1.202 master2"
}

# Ip
check_start()
{
    echo "[check_start] checking ..."
}

check_args()
{
    if [ $1 -lt $2 ]; then
        usage
        exit 1
    fi
}

########################################################
sflag=0
fflag=0

while getopts "sf" flag
do
    case $flag in
        s)
            sflag=1
            ;;
        f)
            fflag=1
            ;;
        *)
            echo "Invalid option $flag"
            usage
            exit 1
            ;;
    esac
done

shift $((OPTIND-1))
# echo "[otheropts]==> $@"

ARGS=$#
IP=$1
CONFIGFILE=$2
DAEMON_OPTIONS=""

if [ $sflag -eq 1 ]; then
    check_args $ARGS 1
    erl_stop $IP $MANAGER_NAME
    exit 0
elif [ $fflag -eq 1 ]; then
    echo
else
    DAEMON_OPTIONS="-heart -detached"
fi

check_args $ARGS 2
check_start $IP
backup_log_main
echo erl_dispatch_manager $IP $MANAGER_NAME $CONFIGFILE "$DAEMON_OPTIONS"
erl_dispatch_manager $IP $MANAGER_NAME $CONFIGFILE "$DAEMON_OPTIONS"

##########################################################
# Verification
##########################################################
if [ $fflag -eq 0 ]; then
    case $(erl_ping $IP $MANAGER_NAME) in
        pong)
            echo "master is started!"
            exit 0
            ;;
        *)
            echo "master is not started!"
            exit 1
            ;;
    esac
fi

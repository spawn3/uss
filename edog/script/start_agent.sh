#!/usr/bin/env bash

# set -x

current_dir=`dirname $0`
source $current_dir/application.sh
source $current_dir/erlctl.sh

usage()
{
    echo "Usage:"
    echo "  start_agent.sh <ip> [start|stop|check|fg]"
}

########################################################
if [ $# -eq 0 ]
then
    usage
    exit 1
fi

IP=$1
ACTION=${2:-start}

case $ACTION in
    start)
        erl_dispatch_agent $IP $AGENT_NAME
        ;;
    stop)
        erl_stop $IP $AGENT_NAME
        ;;
    check)
        erl_check $IP
        ;;
    fg)
        erl_start_agent $IP $AGENT_NAME agent ""
        ;;
esac

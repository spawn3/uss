#!/usr/bin/env bash

#set -x

NODE=$1
ACTION=${2:-show}

BEAM=beam.smp
HEART=heart

beam_pids=$(ps --no-headers -C beam.smp -o pid,cmd|grep "\<$NODE\>"|awk '{print $1}')
for x in $beam_pids
do
    heart=$(ps --no-headers -C heart -o pid,cmd|grep "pid.*$x"|awk '{print $1}')
    #echo "$x $heart"
done

kill_pids()
{
    if [ ! -z "$*" ];
    then
        kill -9 $*
    fi
}

case $ACTION in
    kill)
        echo "= node : $NODE, beam: $beam_pids, heart: $heart"
        kill_pids "$beam_pids"
        kill_pids "$heart"
        ;;
    *)
        echo "= node : $NODE, beam: $beam_pids, heart: $heart"
        ;;
esac

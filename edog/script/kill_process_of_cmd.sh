#!/usr/bin/env bash

# set -x

PIDS=$(ps --no-header -o pid,cmd -C $1|grep "\<$2\>"|awk '{print $1}')
if [ ! -z "$PIDS" ]
then
    kill -9 $PIDS
fi

PIDS=$(ps --no-header -o pid,cmd -C $1|grep "\<$2\>"|awk '{print $1}')
if [ -z "$PIDS" ]
then
    echo "ok"
else
    echo "error"
fi

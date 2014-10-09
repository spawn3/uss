#!/usr/bin/env bash

PIDS=$(ps --no-header -o pid,cmd -C $1|grep "\<$2\>"|awk '{print $1}')
if [ -z "$PIDS" ]
then
    exit 1
fi

echo "$PIDS"
exit 0

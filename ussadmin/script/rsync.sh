#!/usr/bin/env bash

PWD=`pwd`
echo "rsync " $PWD to $1
rsync -av $PWD root@$1:/sysy/yfs/

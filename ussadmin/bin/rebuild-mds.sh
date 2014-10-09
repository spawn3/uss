#!/usr/bin/env bash

N=1

rm -rf /sysy/yfs/mds/$N/current
touch /sysy/yfs/mds/$N/rebuild
/sysy/yfs/app/sbin/yfs_mds -n $N

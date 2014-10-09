#!/usr/bin/env bash

# set -x

YFS_PREFIX=/sysy/yfs

# is_process_running <file>
is_process_running()
{
    file=$1
    if [ -f "$file" ]; then
        flock -nx $file -c "sleep 0.1"
        if [ $? -ne 0 ]; then
            status=$(cat $file)
            if [ "$status" == "running" ]; then
                return 0
            else
                return 1
            fi
        else
            return 2
        fi
    else
        return 3
    fi
}

# 192.168.2.18:mds/0 id:1_v1318510639 status:master peer:5 sync:100
# 192.168.2.19:mds/0 id:24_v1318510639 status:slave peer:5 sync:100
# 192.168.2.21:mds/0 id:23_v1318510639 status:slave peer:5 sync:100
# 192.168.2.17:mds/0 id:3_v1318510639 status:slave peer:5 sync:100
is_mds_ready()
{
    declare arr
    ret=$($YFS_PREFIX/app/bin/uss.mdstat|grep peer|grep -v NA|awk -F: '{print $4,$5}'|awk '{print $1,$3}')
    master=""
    idx=0
    for x in $ret
    do
        if [ "$x" == "master" ]; then
            master=$x
        fi
        arr[$idx]=$x
        let idx=idx+1
    done
    echo ${arr[@]}
    if [ $idx -gt 2 ]; then
        let quorum=${arr[1]}/2
        let online=$idx/2
        # echo $master
        # echo $quorum
        # echo $online
        if [ $online -gt $quorum ]  && [ $master == "master" ]; then
            return 0
        else
            return 1
        fi
    else
        return 2
    fi
}

# is_process_running $YFS_PREFIX/proxy/status/status
proxy_status=0
is_process_running $YFS_PREFIX/iscsi/status/status
iscsi_status=$?
is_mds_ready
mds_status=$?

#echo $proxy_status
#echo $iscsi_status
#echo $mds_status

if [ $mds_status -eq 0 ] && [ $proxy_status -eq 0 ] && [ $iscsi_status -eq 0 ]; then
    exit 0
else
    exit 1
fi

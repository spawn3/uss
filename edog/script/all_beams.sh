#!/usr/bin/env bash

# set -x

current_dir=`dirname $0`
# source $current_dir/find_node.sh

IP=$1

for x in edog_master edog ussadmin ussadmin_master
do
    echo "-----------------------------------------"
    bash $current_dir/find_node.sh "$x@$IP"
done

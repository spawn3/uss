#!/usr/bin/env bash

##########################################################
# Usage:
#   ./gen.sh [web 192.168.1.230] 192.168.1.201 192.168.1.202
#
# This will generate three configuration in ../conf/ dir.
##########################################################

set -x

current_dir=`dirname $0`
. $current_dir/application.sh

gen_master_config()
{
    webserver=${web:-127.0.0.1}
    i=1
    while [ $i -le $# ]
    do
        python $current_dir/confgen.py -w $webserver -m -n $i "$@" > $current_dir/../conf/master$i.config
        let i=i+1
        # i = `expr $i+1`
    done
}

gen_slave_config()
{
    python $current_dir/confgen.py "$@" > $current_dir/../conf/slave.config
}

while [ $# -gt 0 ]
do
    case $1 in
        web)
            web=$2
            shift
            shift
            break
            ;;
        *)
            break
            ;;
    esac
done

if [ ! -d $current_dir/../conf ];
then
    mkdir -p $current_dir/../conf
fi

gen_master_config "$@"
gen_slave_config "$@"

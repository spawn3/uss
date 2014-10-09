#!/usr/bin/env bash

# set -x

current_dir=`dirname $0`
source $current_dir/application.sh

# copy_file file
copy_file()
{
    CONF_DIR=$APP_DATA/conf
    mkdir -p $CONF_DIR
    version=$(date +%Y%m%d-%H%M%S)
    mv $CONF_DIR/$1 $CONF_DIR/$1_$version
    cp $APP_SRC/conf/$1 $CONF_DIR
}

FILES="cluster.xml stddisk.conf"

for i in $FILES
do
    copy_file $i
done

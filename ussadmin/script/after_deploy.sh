#!/usr/bin/env bash

CONF_FILE=/etc/ld.so.conf.d/sysy.conf
DBDIR=/usr/local/db-4.8

# adjust /etc/ld.so.conf.d/sysy.conf
install_conf()
{
    rm $CONF_FILE
    touch $CONF_FILE

    for dir in /sysy/yfs/app/lib $DBDIR/lib /usr/local/lib
    do
        uname -a | grep -Eq gentoo && {
        echo LDPATH=$dir > /etc/env.d/50sysy;
        env-update 2>/dev/null;} || {
        echo $dir >> $CONF_FILE;
        }
    done
    ldconfig 2>&1 /dev/null
}

install_conf

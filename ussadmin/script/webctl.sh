#!/usr/bin/env bash

current_dir=`dirname $0`
source $current_dir/application.sh

WEB_ROOT=$APP_ROOT/ussadmin/priv/ua

# Ip NodeName ConfigFile AsMaster AsSlave
web_start()
{
    echo "python $WEB_ROOT/ua/ua_main.py 0.0.0.0:9600"
    nohup python $WEB_ROOT/ua/ua_main.py 0.0.0.0:9600 > $APP_DATA/uss_web.log 2>&1 &
}

web_start_fg()
{
    echo "python $WEB_ROOT/ua/ua_main.py 0.0.0.0:9600"
    python $WEB_ROOT/ua/ua_main.py 0.0.0.0:9600
}

web_stop()
{
    echo $APP_SRC/bin/stopua
    $APP_SRC/bin/stopua
}

web_dispatch()
{
    web_stop
    web_start
}

usage()
{
    prog=`basename $0`
    echo "usage:"
    echo "    $prog start"
    echo "    $prog fg"
    echo "    $prog stop"
}

#######################################################
# main
#######################################################
case $1 in
    start)
        web_dispatch
        ;;
    fg)
        web_start_fg
        ;;
    stop)
        web_stop
        ;;
    *)
        usage
        ;;
esac

#!/usr/bin/env bash

# set -x

current_dir=`dirname $0`
source $current_dir/application.sh

WEB_ROOT=$APP_SRC/priv/mdsoss

web_start()
{
    echo 'whs starting...'
    echo python $WEB_ROOT/manage.py runserver 0.0.0.0:9500
    nohup python $WEB_ROOT/manage.py runserver 0.0.0.0:9500 > $LOG_DIR/web-edog.log 2>&1 &
}

web_start_fg()
{
    echo 'whs starting...'
    echo python $WEB_ROOT/manage.py runserver 0.0.0.0:9500
    python $WEB_ROOT/manage.py runserver 0.0.0.0:9500
}

# TODO
PIDS=$(ps --no-header -o pid,cmd -C python|grep "\<manage.py.*9500\>"|awk '{print $1}')
web_stop()
{
    echo 'whs stopping...'
    if [ -z "$PIDS" ]
    then
        echo "web not running"
    else
        kill -9 $PIDS
    fi
}

web_conf()
{
    cd $WEB_ROOT && python manage.py syncdb

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

REBAR_CONFIG=$current_dir/../rebar.config
ERLAPI_PY=$current_dir/../priv/mdsoss/oss/erlapi.py

print_msg()
{
    echo '-------------------------------------------------------------'
    echo "***$REBAR_CONFIG***"
    grep 'use_as' $REBAR_CONFIG
    echo "***$ERLAPI_PY***"
    grep 'api_web_host[ \t]*=' $ERLAPI_PY
    grep 'api_web_port[ \t]*=' $ERLAPI_PY
    echo '-------------------------------------------------------------'
}

#######################################################
# main
#######################################################
print_msg

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
    conf)
        web_conf
        ;;
    *)
        usage
        ;;
esac

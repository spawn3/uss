#!/usr/bin/env bash

current_dir=`dirname $0`
source $current_dir/application.sh

NODETOOL=$current_dir/nodetool

######################################################
# Ip NodeName
erl_restart()
{
    $NODETOOL -name $2@$1 -setcookie $COOKIE restart
}

# Ip NodeName
erl_stop()
{
    $NODETOOL -name $2@$1 -setcookie $COOKIE stop
}

# Ip NodeName
erl_ping()
{
    local result=$($NODETOOL -name $MANAGER_NAME@$IP -setcookie $COOKIE ping)
    echo $result
}

# NodeName
erl_check()
{
    if $EPMD -names|grep -q "\<$1\>"
    then
        echo "$1 is running!"
    else
        echo "$1 is not running!"
    fi
}

# Ip NodeName ConfigFile DaemonOptions
erl_start_manager()
{
    export HEART_BEAT_TIMEOUT=5
    export HEART_COMMAND="sh $APP_SRC/script/start_master.sh $1 $3"

    PREPARE_ACTION="uss_app:prepare()"

    $ERL -name $2@$1 -setcookie $COOKIE \
        -boot start_sasl \
        -pa $APP_ROOT/ussadmin/deps/*/ebin \
        -pa $APP_ROOT/ussadmin/plugins/*/ebin \
        -pa $APP_SRC/ebin \
        -config $APP_DATA/conf/$3 \
        $4 \
        -eval "$PREPARE_ACTION,application:start($APPLICATION)" \
        +K true
}

# Ip NodeName ConfigFile DaemonOptions
erl_start_agent()
{
    export HEART_BEAT_TIMEOUT=5
    export HEART_COMMAND="sh $APP_SRC/script/start_agent.sh $1"

    $ERL -name $2@$1 -setcookie $COOKIE \
        -boot start_sasl \
        -pa $APP_ROOT/ussadmin/deps/*/ebin \
        -pa $APP_ROOT/ussadmin/plugins/*/ebin \
        -pa $APP_SRC/ebin \
        -config $APP_DATA/conf/$3 \
        $4 \
        -eval "application:start($APPLICATION)" \
        +K true
}

# Ip NodeName Config Options
erl_dispatch_manager()
{
    case $(erl_ping $1 $2) in
        pong)
            erl_restart $1 $2
            ;;
        *)
            erl_start_manager $1 $2 $3 "$4"
            ;;
    esac
}

# Ip NodeName
erl_dispatch_agent()
{
    if $EPMD -names|grep -q "\<$2\>"
    then
        erl_restart $1 $2
    else
        erl_start_agent $1 $2 agent "-heart -detached"
    fi
}

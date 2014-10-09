#!/usr/bin/env bash

current_dir=`dirname $0`
source $current_dir/application.sh

NODETOOL=$current_dir/nodetool

######################################################
# Ip NodeName
erl_restart()
{
    # $NODETOOL -name $2@$1 -setcookie $COOKIE restart
    bash $current_dir/find_node.sh "$2@$1" kill
}

# Ip NodeName
erl_stop()
{
    # $NODETOOL -name $2@$1 -setcookie $COOKIE stop
    bash $current_dir/find_node.sh "$2@$1" kill
}

# Ip NodeName
erl_ping()
{
    local result=$($NODETOOL -name $MANAGER_NAME@$IP -setcookie $COOKIE ping)
    echo $result
}

# NodeName
erl_ping_beam()
{
    if $EPMD -names|grep -q "\<$1\>"
    then
        return 0
    else
        return 1
    fi
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

    # PREPARE_ACTION="edog_app:prepare()"
    PREPARE_ACTION="application:start(edogdb)"

    $ERL -name $2@$1 -setcookie $COOKIE \
        -shutdown_time 5000 \
        -boot start_sasl \
        -pa $APP_SRC/deps/cclib/ebin \
        -pa $APP_SRC/deps/edogdb/ebin \
        -pa $APP_SRC/plugins/*/ebin \
        -pa $APP_SRC/ebin \
        -config $APP_DATA/conf/$3 \
        $4 \
        -eval "$PREPARE_ACTION,application:start($APPLICATION)" \
        +A 2 \
        +K true
}

# Ip NodeName ConfigFile DaemonOptions
erl_start_agent()
{
    export HEART_BEAT_TIMEOUT=5
    export HEART_COMMAND="sh $APP_SRC/script/start_agent.sh $1"

    $ERL -name $2@$1 -setcookie $COOKIE \
        -shutdown_time 5000 \
        -boot start_sasl \
        -pa $APP_SRC/deps/cclib/ebin \
        -pa $APP_SRC/deps/edogdb/ebin \
        -pa $APP_SRC/plugins/*/ebin \
        -pa $APP_SRC/ebin \
        -config $APP_DATA/conf/$3 \
        $4 \
        -eval "application:start($APPLICATION)" \
        +A 2 \
        +K true
}

# Ip NodeName Config Options
erl_dispatch_manager()
{
    if $EPMD -names|grep -q "\<$2\>"
    then
        bash $current_dir/webctl.sh stop
        erl_stop $1 $2
        erl_start_manager $1 $2 $3 "$4"
    else
        erl_start_manager $1 $2 $3 "$4"
    fi
}

# Ip NodeName
erl_dispatch_agent()
{
    if $EPMD -names|grep -q "\<$2\>"
    then
        erl_stop $1 $2
        erl_start_agent $1 $2 agent "-heart -detached"
    else
        erl_start_agent $1 $2 agent "-heart -detached"
    fi
}

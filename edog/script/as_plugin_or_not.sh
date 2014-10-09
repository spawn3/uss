#/usr/bin/env bash

#set -x

current_dir=$(dirname $0)

REBAR_CONFIG=$current_dir/../rebar.config
ERLAPI_PY=$current_dir/../priv/mdsoss/oss/erlapi.py

print_msg()
{
    echo '--------------------------------------------------'
    grep 'use_as' $REBAR_CONFIG
    grep 'api_web_port.*=' $ERLAPI_PY
    echo '--------------------------------------------------'
}

ACTION=${1:-yes}

case $ACTION in
    yes)
        sed -i 's/use_as_standalone/use_as_plugin/g' $REBAR_CONFIG
        sed -i 's/9501/9601/g' $ERLAPI_PY
        ;;
    *)
        sed -i 's/use_as_plugin/use_as_standalone/g' $REBAR_CONFIG
        sed -i 's/9601/9501/g' $ERLAPI_PY
        ;;
esac

cd $current_dir/.. ; make clean ; make ;

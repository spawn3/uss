#!/usr/bin/env bash

set -x

echo "$*"
echo "$@"
echo $#
echo ${#*}
echo ${#@}
echo $$

EPMD=`which epmd`

erl_check()
{
    r=`$EPMD -names|grep edog`
    echo $r
}

list()
{
    shopt -s nullglob
    textfiles=( *.sh )
    echo $textfiles
}

erl_check
list

echo "r=$r"

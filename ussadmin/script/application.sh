EPMD=`which epmd`
ERL=`which erl`

APPLICATION=ussadmin
MANAGER_NAME=ussadmin_master
AGENT_NAME=ussadmin
COOKIE=ussadmin

APP_ROOT=/sysy/yfs/ussadmin
APP_SRC=$APP_ROOT/ussadmin
APP_DATA=$APP_ROOT/data

#PREPARE_ACTION="uss_app:prepare()"
PREPARE_ACTION="ok"

mkdir -p $APP_ROOT
mkdir -p $APP_SRC
mkdir -p $APP_DATA/conf
mkdir -p $APP_DATA/manager_logs

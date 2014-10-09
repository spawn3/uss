EPMD=`which epmd`
ERL=`which erl`

APPLICATION=edog
MANAGER_NAME=edog_master
AGENT_NAME=edog
COOKIE=edog

APP_ROOT=/sysy/yfs/ussadmin
APP_SRC=$APP_ROOT/edog
APP_DATA=$APP_ROOT/edog_runtime
LOG_DIR=/var/log/ussadmin/edog_runtime/

# PREPARE_ACTION="edog_app:prepare()"
PREPARE_ACTION="ok"

mkdir -p $APP_ROOT
mkdir -p $APP_SRC
mkdir -p $APP_DATA/conf
mkdir -p $APP_DATA/manager_logs

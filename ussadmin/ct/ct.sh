set -x

USSADMIN=/sysy/yfs/ussadmin

export HEART_BEAT_TIMEOUT=5
export HEART_COMMAND="sh /sysy/yfs/ussadmin/ussadmin/script/start_master.sh $1 $2"

usage () {
    echo "Usage:"
    echo "  start_master.sh <ip> <config>"
    echo "For example:"
    echo "  start_master.sh 192.168.1.201 edog_master1"
    echo "  start_master.sh 192.168.1.202 edog_master2"
}

if [ $# -ne 2 ]
then
    usage
    exit 1
fi

rm -rf $USSADMIN/ussadmin/test/*.beam

# -eval "ct:run_test([{dir, \"test\"}, {logdir, \"logs\"}, {include, \"../include\"}])" \
# -eval "application:start(edog)" \
run_test \
    -logdir logs \
    -dir test \
    -include ../include \
    -cover ct/cover.spec \
    -config ct/edog.ctc \
    -erl_args \
    -boot start_sasl \
    -name ussadmin_master@$1 \
    -setcookie ussadmin \
    -pa $USSADMIN/ussadmin/ebin \
    -config $USSADMIN/ussadmin/conf/$2 \
    -mnesia dir '"/sysy/yfs/ussadmin/data/edog_master"' \
    -ussadmin asmaster 1 asagent 1\
    +K true

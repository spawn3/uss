set -x

ROOTDIR=/sysy/yfs/edog
APPLICATION=edog
MASTERNAME=edog_master
NODENAME=edog
COOKIE=edog

export HEART_BEAT_TIMEOUT=5
export HEART_COMMAND="sh $ROOTDIR/script/start_master.sh $1 $2"

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

rm -rf $ROOTDIR/test/*.beam

# -eval "ct:run_test([{dir, \"test\"}, {logdir, \"logs\"}, {include, \"../include\"}])" \
# -eval "application:start(edog)" \
run_test \
    -logdir logs \
    -dir test \
    -include ../src \
    -cover ct/cover.spec \
    -config ct/commontest.ctc \
    -erl_args \
    -boot start_sasl \
    -name $MASTERNAME@$1 \
    -setcookie $COOKIE \
    -pa $ROOTDIR/ebin \
    -config $ROOTDIR/conf/$2 \
    -mnesia dir '"/sysy/yfs/edog_runtime/edog_master"' \
    -$APPLICATION asmaster 1 asagent 1\
    +K true

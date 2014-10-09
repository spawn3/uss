#!/usr/bin/env bash

current_dir=`dirname $0`

YFS_PREFIX=/sysy/yfs

SSHUSER="root"
SSHDIR="/root/.ssh"
ID_RSA="/root/.ssh/id_rsa"
ID_RSA_PUB="/root/.ssh/id_rsa.pub"
PWDFILE=ussadmin.hosts
MAGIC=magicAA55

ssh_usage()
{
    echo "$0 -h"
    echo "$0 -k"
    echo "$0 -c host password"
    echo "$0 -a host password"
    echo "$0 -t hostfile"
    exit 0
}

ssh_keygen()
{
    expect <<- EOF
spawn -noecho ssh-keygen -t rsa
expect {
    "*which to save the key*" {
        send "\r"; exp_continue;
    }
    "*y/n*" {
        send "y\r"; exp_continue;
    }
    "*Enter*passphrase*" {
        send "\r"; exp_continue;
    }
}
EOF
}

ssh_keygen_not_existing()
{
    if [ ! -f $ID_RSA -o ! -f $ID_RSA_PUB ]; then
        ssh_keygen
    fi
}

# usage: ssh_copy_id host password
ssh_copy_id()
{
    expect <<- EOF
spawn -noecho ssh-copy-id -i $ID_RSA_PUB $1
expect {
    "*yes/no" {
        send "yes\r"; exp_continue
    }
    "*password:*" {
        send "$2\r";
        expect {
            "denied" {exit 1}
            eof
        }
    }
    "*No route to host"   {exit 2}
    "*Connection refused" {exit 3}
}
EOF
}

# ssh_copy_file file host password
ssh_copy_file()
{
    expect <<- EOF
spawn -noecho scp $1 $SSHUSER@$2:$SSHDIR
expect {
    "*yes/no" {
        send "yes\r"; exp_continue
    }
    "*password:*" {
        send "$3\r";
        expect {
            "denied" {exit 1}
            eof
        }
    }
    "*No route to host"   {exit 2}
    "*Connection refused" {exit 3}
}
EOF
}

# ssh_rsync host password src dest
ssh_rsync()
{
    expect <<- EOF
spawn -noecho rsync -avz $3 $SSHUSER@$1:$4
expect {
    "*yes/no" {
        send "yes\r"; exp_continue
    }
    "*password:*" {
        send "$2\r";
        expect {
            "denied" {exit 1}
            eof
        }
    }
    "*No route to host"   {exit 2}
    "*Connection refused" {exit 3}
}
EOF
}

# ssh host password cmd
ssh_cmd()
{
    expect <<- EOF
spawn -noecho ssh root@$1 $3
expect {
    "*yes/no" {
        send "yes\r"; exp_continue
    }
    "*password:*" {
        send "$2\r";
        expect {
            "denied" {exit 1}
            eof
        }
    }
    "*No route to host"   {exit 2}
    "*Connection refused" {exit 3}
}
EOF
}

ssh_copy_known_hosts()
{
    ssh_copy_file $SSHDIR/known_hosts $1 $2
}

# usage: ssh_ping host user password
ssh_ping()
{
    if ssh $SSHUSER@$1 echo $MAGIC|grep -q $MAGIC
    then
        echo true
    else
        echo false
    fi
}

# usage: ssh_add host passwd
ssh_add()
{
    ssh_keygen_not_existing
    ssh_copy_id $1 $2
}

# ssh_deploy host password
ssh_deploy()
{
    ssh_add $1 $2

    ssh root@$1 mkdir -p $YFS_PREFIX/app
    ssh root@$1 mkdir -p $YFS_PREFIX/etc
    ssh root@$1 mkdir -p $YFS_PREFIX/ussadmin/ussadmin/

    rsync -avz $YFS_PREFIX/app root@$1:/$YFS_PREFIX/
    rsync -avz $YFS_PREFIX/etc root@$1:/$YFS_PREFIX/
    rsync -avz $YFS_PREFIX/ussadmin/ussadmin/* root@$1:/$YFS_PREFIX/ussadmin/ussadmin

    # rsync -avz $YFS_PREFIX/ussadmin/ussadmin/ root@$1:/$YFS_PREFIX/ussadmin/ussadmin
    # rsync -avz $YFS_PREFIX/ussadmin/ussadmin/ root@$1:/$YFS_PREFIX/ussadmin/ussadmin
    # rsync -avz $YFS_PREFIX/ussadmin/ussadmin/ root@$1:/$YFS_PREFIX/ussadmin/ussadmin
}

# usage: ssh_test hostfile
ssh_test()
{
    if [ $(whoami) != $SSHUSER ]; then
        echo "Must be $SSHUSER user to run!"
        exit 1
    fi

    while read -r host passwd
    do
        ssh_add $host $passwd 2> /dev/null </dev/null
    done < $1
}

while getopts "kcp:arxt:" opt;
do
    case $opt in
        k)
            ssh_keygen_not_existing
            ;;
        p)
            ssh_ping $OPTARG
            ;;
        t)
            ssh_test $OPTARG
            ;;
        a)
            ssh_add $2 $3
            ;;
        r)
            ssh_rsync $2 $3 $4 $5
            ;;
        x)
            ssh_deploy $2 $3
            ;;
        c)
            ssh_cmd $2 $3 "$4 $5"
            ;;
        *)
            ssh_usage
            ;;
    esac
done

# echo $*
# echo $OPTIND
# shift $(($OPTIND-1))
# echo $*

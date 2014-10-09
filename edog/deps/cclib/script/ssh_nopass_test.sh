#!/usr/bin/env bash

ECHO=magicAA55

HOST=$1
USER=$2
PASSWORD=$3

ssh_test()
{
    expect <<- EOF
spawn -noecho ssh $USER@$HOST echo $ECHO
expect {
    "*yes/no" {
        send "yes\r"; exp_continue
    }
    "*password:*" {
        send "$PASSWORD\r";
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

if ssh_test|grep -q $ECHO; then
    echo true
else
    echo false
fi

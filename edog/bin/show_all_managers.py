#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import os
import sys
import subprocess

from usslib import UssConn, UssResource

current_dir = os.path.dirname(os.path.realpath(__file__))

def run_cmd(cmd):
    #print '***cmd*** %s' % cmd
    sub = subprocess.Popen(args=cmd, stdout=subprocess.PIPE, shell=True)
    res = sub.stdout.read()
    #print '***res*** %s' % res
    return res

def ping2(ip):
    conn = UssConn(ip, 9501)
    resource = UssResource(conn, '/')
    d = resource.get()
    if d['status'] < 300:
        print '==> active manager: %s' % d['data']['node']
    else:
        #print d
        print '==> manager [%s] not active!' % ip

if __name__ == '__main__':
    managers = cclib:get_managers()
    for x in managers:
        ping2(x)

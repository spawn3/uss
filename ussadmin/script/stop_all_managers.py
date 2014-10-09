#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import os
import sys
import subprocess

current_dir = os.path.dirname(os.path.realpath(__file__))

def run_cmd(cmd):
    print '***cmd*** %s' % cmd
    sub = subprocess.Popen(args=cmd, stdout=subprocess.PIPE, shell=True)
    res = sub.stdout.read()
    print '***res*** %s' % res
    return res

def get_masters():
    cmd = 'escript %s/get_masters.erl' % current_dir
    res = run_cmd(cmd)
    return res.split()

def stop_manager(x):
    ip = x.split('@')[1]
    cmd = 'ssh root@%s %s/../bin/appctl manager stop %s' % (ip, current_dir, ip)
    run_cmd(cmd)

if __name__ == '__main__':
    managers = get_masters()
    seq = xrange(1, len(managers)+1)
    l = zip(managers, seq)
    for x, n in l:
        stop_manager(x)

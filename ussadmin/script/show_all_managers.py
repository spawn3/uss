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

def start_master(x, n):
    ip = x.split('@')[1]
    cmd = 'curl http://%s:9601' % ip
    run_cmd(cmd)

if __name__ == '__main__':
    masters = get_masters()
    seq = xrange(1, len(masters)+1)
    l = zip(masters, seq)
    for x, n in l:
        start_master(x, n)

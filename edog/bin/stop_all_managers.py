#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import os
import sys
import subprocess
import cclib

current_dir = os.path.dirname(os.path.realpath(__file__))

def run_cmd(cmd):
    #print '***cmd*** %s' % cmd
    sub = subprocess.Popen(args=cmd, stdout=subprocess.PIPE, shell=True)
    res = sub.stdout.read()
    #print '***res*** %s' % res
    return res

def stop_manager(ip):
    cmd = 'ssh root@%s %s/../bin/appctl manager stop %s' % (ip, current_dir, ip)
    run_cmd(cmd)

if __name__ == '__main__':
    managers = cclib.get_managers()
    seq = xrange(1, len(managers)+1)
    l = zip(managers, seq)
    for x, n in l:
        stop_manager(x)

#!/usr/bin/env python

import os
import sys
import shlex
import subprocess
import commands
import xml.etree.ElementTree as ElementTree

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

def ping(x):
    ip = x.split('@')[1]
    port = 9601
    cmd = 'curl http://%s:%d/uss/uss_web/info' % (ip, port)
    d = {}
    try:
        res = run_cmd(cmd)
        tree = ElementTree.fromstring(res)
        for item in tree.findall('item'):
            d[item.get('key')] = item.get('value')
        return d['node']
    except Exception, e:
        return None

def get_primary():
    l = [ping(x) for x in get_masters()]
    return [x for x in l if x]

def get_primary_ip():
    l = [x.split('@')[1] for x in get_primary() if x]
    return l[0]

if __name__ == '__main__':
    primary = get_primary()
    print get_primary_ip()
    print '--------------------------------------------------------------'
    print 'current manager:', primary
    print '--------------------------------------------------------------'

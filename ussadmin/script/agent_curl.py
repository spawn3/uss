#!/usr/bin/env python

import os, sys
import subprocess
import shlex
import simplejson
import json

def run_cmd(cmd):
    print '***cmd*** %s' % cmd
    args = shlex.split(cmd)
    res = subprocess.call(args)
    print '\n--------------------------------------------------------------'
    return res

def url(f):
    return "http://%s:%d/uss/sql/%s" % ("192.168.1.201", 9601, f)

def get_cmd(data, f):
    return 'curl -X POST -d \"%s\" %s' % (json.dumps(data).replace('"', '\\\"'), url(f))

def test_moni():
    data = {"id":1}
    run_cmd(get_cmd(data, 'moni'))

def test_syst_test():
    data = {"action":"test"}
    run_cmd(get_cmd(data, 'syst'))

def test_syst_deploy():
    data = {"action":"deploy"}
    run_cmd(get_cmd(data, 'syst'))

if __name__ == '__main__':
    test_moni()
    test_syst_test()
    test_syst_deploy()

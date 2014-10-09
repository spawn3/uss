#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import os
import sys
import subprocess

def run_cmd(cmd):
    sub = subprocess.Popen(args=cmd, stdout=subprocess.PIPE, shell=True)
    res = sub.stdout.read()
    print '==> %s -> %s' % (cmd, res)
    return res

def print_module_info(x):
    try:
        print '==> %s -> %s' % (x, x.VERSION)
    except Exception, e:
        print '==> %s -> %s' % (x, x.__version__)

if __name__ == '__main__':
    print 'MODULES------------------------------------------------------------------------'
    try:
        import django
        import web
        import jinja2
        import Image

        print_module_info(django)
        print_module_info(web)
        print_module_info(jinja2)
        print_module_info(Image)
    except Exception, e:
        print e

    print 'COMMANDS------------------------------------------------------------------------'
    run_cmd('which gcc')
    run_cmd('which ssh')
    run_cmd('which rsync')
    run_cmd('which expect')

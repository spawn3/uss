#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import os
import sys
import subprocess

def run_cmd(cmd):
    print '***cmd*** %s' % cmd
    sub = subprocess.Popen(args=cmd, stdout=subprocess.PIPE, shell=True)
    res = sub.stdout.read()
    print '***res*** %s' % res
    return res

def print_module_info(x):
    print 'module:',  x
    try:
        print 'version: ', x.VERSION
    except Exception, e:
        print 'version:', x.__version__
    print

if __name__ == '__main__':
    print 'MODULES------------------------------------------------------------------------'
    try:
        import django
        import web
        import jinja2

        print_module_info(django)
        print_module_info(web)
        print_module_info(jinja2)
    except Exception, e:
        print e

    print 'COMMANDS------------------------------------------------------------------------'
    run_cmd('which ssh')
    run_cmd('which expect')
    run_cmd('which gcc')
    run_cmd('which rsync')

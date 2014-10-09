#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101101
Author: zkz
"""
import sys

from common.rrdtool.server import ThreadRrdtool

def usage():
    print "usage: %s start/stop/restart"%(sys.argv[0])

if __name__ == "__main__":
    thread_rrd = ThreadRrdtoolDaemon()
    if len(sys.argv) != 2: 
        usage()
        sys.exit()
    if sys.argv[1] == "start":
        thread_rrd.start()
    elif sys.argv[1] == "stop":
        thread_rrd.stop()
    elif sys.argv[1] == "restart":
        thread_rrd.restart()
    else:
        usage()

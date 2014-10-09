#!/usr/bin/python
#-*- coding: utf-8 -*-

from ua.common.rrdtool import updater, grapher, fetcher
from ua.common.utils import current_dir, get_log
from ua.common.daemon import Daemon
from ua.common.ua_conf import ua_conf, ua_path

import os
import time
import threading

imgs_dir = os.path.join(ua_path, ua_conf.rrd_imgs_dir)
rrdtool_log = get_log('rrdtool', ua_conf.rrd_log)

def work():
    while True:
        updater.update(rrds_dir=ua_conf.rrd_rrds_dir, imgs_dir=imgs_dir)
        time.sleep(ua_conf.moni_interval)

class ThreadRrdtool(threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)
    def run(self):
        work()

class ThreadRrdtoolDaemon(Daemon):
    def __init__(self):
        rundir = os.path.dirname(ua_conf.rrd_pid_file)
        if not os.path.exists(rundir):
            os.makedirs(rundir)
        Daemon.__init__(self, ua_conf.rrd_pid_file)
    def run(self):
        work()

#---------------------------------------------------
def grapher_work():
    while True:
        try:
            grapher.graph(ua_conf.rrd_rrds_dir, imgs_dir)
        except:
            rrdtool_log.error('grapher_work error: rrd %s img %s' % (ua_conf.rrd_rrds_dir, imgs_dir))
        rrdtool_log.info('grapher_work finished')
        time.sleep(ua_conf.grapher_moni_interval)

class ThreadRrdtoolGrapher(threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)
    def run(self):
        grapher_work()

class ThreadRrdtoolGrapherDaemon(Daemon):
    def __init__(self):
        rundir = os.path.dirname(ua_conf.grapher_pid_file)
        if not os.path.exists(rundir):
            os.makedirs(rundir)
        Daemon.__init__(self, ua_conf.grapher_pid_file)
    def run(self):
        grapher_work()


def updater_work():
    while True:
        try:
            nodes_moni, racks_moni, glob_moni = updater.prepare_data()
            rrdtool_log.info('nodes_moni -------%s'%str(nodes_moni))
            rrdtool_log.info('racks_moni -------%s'%str(racks_moni))
            rrdtool_log.info('glob_moni  -------%s'%str(glob_moni))
            updater.update_pure(rrds_dir=ua_conf.rrd_rrds_dir, imgs_dir=imgs_dir)
        except:
            rrdtool_log.error('updater_work error: rrd %s img %s' % (ua_conf.rrd_rrds_dir, imgs_dir))
        rrdtool_log.info('updater_work finished')
        time.sleep(ua_conf.updater_moni_interval)

class ThreadRrdtoolUpdater(threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)
    def run(self):
        updater_work()

class ThreadRrdtoolUpdaterDaemon(Daemon):
    def __init__(self):
        rundir = os.path.dirname(ua_conf.updater_pid_file)
        if not os.path.exists(rundir):
            os.makedirs(rundir)
        Daemon.__init__(self, ua_conf.updater_pid_file)
    def run(self):
        updater_work()

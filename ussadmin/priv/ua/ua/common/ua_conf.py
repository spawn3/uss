#!/usr/bin/python
#-*- coding: utf-8 -*-

import os
import ConfigParser

def ensure_dir(dir):
    """如果没有创建目录"""
    if not os.path.isdir(dir):
        print "*** make dir: %s" % dir
        os.makedirs(dir)

ua_data_prefix='/sysy/yfs/ussadmin/data'

ua_conf_file     = os.path.join(ua_data_prefix, 'conf/ua.conf')
ua_password_file = os.path.join(ua_data_prefix, 'conf/.password')

cp = ConfigParser.ConfigParser()
cp.read(ua_conf_file)

edog_host        = cp.get('edog_info', 'host')
edog_port        = cp.get('edog_info', 'port')
sql_url          = cp.get('edog_info', 'sql_url')
misc_url         = cp.get('edog_info', 'misc_url')

ua_path          = cp.get('ua_main', 'ua_path')
ua_data_path     = cp.get('ua_main', 'ua_data_path')
ua_main_log_file = cp.get('ua_main', 'main_log')
ua_main_pid_file = cp.get('ua_main', 'pid_file')
ear_log_path     = cp.get('ua_main', 'ear_log')
conf_request_log = cp.get('ua_main', 'request_log')

db_dir           = cp.get('db_info', 'db_dir')
sql_dir          = cp.get('db_info', 'sql_dir')

class ua_conf:
    rrd_rrds_dir          = cp.get('rrd_info', 'rrds_dir')
    rrd_imgs_dir          = cp.get('rrd_info', 'imgs_dir')
    rrd_pid_file          = cp.get('rrd_info', 'pid_file')
    grapher_pid_file      = cp.get('rrd_info', 'grapher_pid_file')
    updater_pid_file      = cp.get('rrd_info', 'updater_pid_file')
    rrd_log               = cp.get('rrd_info', 'rrdtool_log')
    moni_interval         = int(cp.get('rrd_info', 'moni_interval'))
    grapher_moni_interval = int(cp.get('rrd_info', 'grapher_moni_interval'))
    updater_moni_interval = int(cp.get('rrd_info', 'updater_moni_interval'))
    rrd_step              = int(cp.get('rrd_info', 'rrd_step'))
    rrd_heartbeat         = int(cp.get('rrd_info', 'rrd_heartbeat'))

########################################################
ensure_dir(ua_data_path)
ensure_dir(os.path.join(ua_data_path, 'conf'))
ensure_dir(os.path.join(ua_data_path, 'store'))
ensure_dir(ua_conf.rrd_rrds_dir)
ensure_dir(os.path.dirname(conf_request_log))
ensure_dir(os.path.dirname(ear_log_path))
ensure_dir(db_dir)

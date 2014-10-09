#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20110123
Author: zkz
"""
from ua.common.utils import mkdirs, local_exec
from ua.common.ua_conf import ua_conf

import time
import os
import re

def create(rrd_path, start='now-10s', step=ua_conf.rrd_step,
        heartbeat=ua_conf.rrd_heartbeat):
    """
    :param rrd_path:path of rrd file,likes:/root/store/rrds/load.rrd
    """
    def local_create(rrd_path, cmd):
        if not rrd_path.endswith('.rrd'):
            raise Exception('Error: wrong rrd file path.')

        rrd_dir = '/'.join(rrd_path.split('/')[0:-1])
        try: mkdirs(rrd_dir)
        except: raise

        if not os.path.exists(rrd_path):
            try: local_exec(cmd)
            except: raise

#    START = time.time()-10

    cmd = ' '.join([
            'rrdtool create %s' % rrd_path,
            '--start %s' % start,                   #--start|-b start time (default: now - 10s)
            '--step %s' % step,                     #--step|-s step (default: 300 seconds)
#            '--no-overwrite',                       #Do not clobber an existing file of the same name.
            'DS:load1:GAUGE:%d:U:U' % heartbeat,   #U:U expresses no limit
            'DS:load5:GAUGE:%d:U:U' % heartbeat,
            'DS:load15:GAUGE:%d:U:U' % heartbeat,
            'RRA:AVERAGE:0.5:1:600',
            'RRA:AVERAGE:0.5:4:600',
            'RRA:AVERAGE:0.5:24:600',
            'RRA:AVERAGE:0.5:288:730',
            'RRA:MIN:0.5:1:600',
            'RRA:MIN:0.5:4:600',
            'RRA:MIN:0.5:24:600',
            'RRA:MIN:0.5:288:730',
            'RRA:MAX:0.5:1:600',
            'RRA:MAX:0.5:4:600',
            'RRA:MAX:0.5:24:600',
            'RRA:MAX:0.5:288:730',
            'RRA:LAST:0.5:1:600',
            'RRA:LAST:0.5:4:600',
            'RRA:LAST:0.5:24:600',
            'RRA:LAST:0.5:288:730'
            ])

    try:
        local_create(rrd_path, cmd)
    except:
        raise




def update(rrd_path, data):
    """
    :param rrd_path: path of rrd file,likes:/root/store/rrds/load.rrd
    :param data: data would be updated into rrd, likes: [load1, load5, load15]
    """
    if not os.path.exists(rrd_path):
        try:
            create(rrd_path)
        except:
            raise

    cmd = 'rrdtool update %s %d:%f:%f:%f' % (
            rrd_path,
            time.time(),
            data[0],
            data[1],
            data[2]
            )
    try:
        out, err = local_exec(cmd)
        if err:
            raise Exception(err)
    except:
        raise

def graph(rrd_path, img_path, start='end-360m', end='now', width=200, height=45):
    if not os.path.exists(rrd_path):
        raise Exception("Error:file %s doesn't exist" % rrd_path)

    cmd = ' '.join([
            'rrdtool graph %s' % img_path,
            '--end %s' % end,
            '--start %s' % start,
            '--width %s' % width,
            '--height %s' % height,
            '--title "load average"',
            '--vertical-label "load"',
            '--lower-limit 0',
            '-Y -X 0',
            '--rigid',
            'DEF:avg1=%s:load1:AVERAGE' % rrd_path,
            'DEF:avg5=%s:load5:AVERAGE' % rrd_path,
            'DEF:avg15=%s:load15:AVERAGE' % rrd_path,
            'AREA:avg1#E7CD00:"avg1"',
            'AREA:avg5#DE9300:"avg5"',
            'AREA:avg15#F20E01:"avg15"'
            ])

    try:
        local_exec(cmd)
    except:
        raise

def fetch(rrd_path, CF='AVERAGE', resolution=None, start='now', end='now'):
    if not os.path.exists(rrd_path):
        raise Exception("Error:file %s doesn't exist" % rrd_path)

    cmd = ' '.join(['rrdtool fetch %s' % rrd_path,
                    CF,
                    '--resolution %s' % resolution if resolution else '',
                    '--start %s' % start,
                    '--end %s' % end
                    ])

    try:
        out, err = local_exec(cmd)
        return out, err
    except:
        raise

def fetch_last(rrd_path):
    try:
        out, err = fetch(rrd_path)
        if err: raise Exception(load_err)

        data = re.findall('(\d+):\s(\S+)\s(\S+)\s(\S+)', out)

        time = int(data[0][0])
        load1 = None if '-nan' == data[0][1] else int(data[0][1])
        load5 = None if '-nan' == data[0][1] else int(data[0][1])
        load15 = None if '-nan' == data[0][1] else int(data[0][1])

        return time, load1, load5, load15
    except:
        raise

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
            'DS:total:GAUGE:%d:U:U' %  heartbeat, #U:U expresses no limit
            'DS:free:GAUGE:%d:U:U' % heartbeat,
            'DS:cached:GAUGE:%d:U:U' % heartbeat,
            'DS:buffers:GAUGE:%d:U:U' % heartbeat,
            'RRA:AVERAGE:0.5:1:600',
            'RRA:AVERAGE:0.5:4:600',
            'RRA:AVERAGE:0.5:24:600',
            'RRA:AVERAGE:0.5:288:730'
            ])

    try:
        local_create(rrd_path, cmd)
    except:
        raise




def update(rrd_path, data):
    """
    :param rrd_path: path of rrd file,likes:/root/store/rrds/mem.rrd
    :param data: data would be updated into rrd, likes: [mem_total, mem_free, mem_cached, mem_buffers]
    """
    if not os.path.exists(rrd_path):
        try:
            create(rrd_path)
        except:
            raise
    cmd = 'rrdtool update %s %d:%f:%f:%f:%f' % (
            rrd_path,
            time.time(),
            data[0],
            data[1],
            data[2],
            data[3]
            )
    try:
        local_exec(cmd)
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
            '--lower-limit 0',
            '--rigid',
            '--title "memory"',
            '--vertical-label "GB"',
            'DEF:total=%s:total:AVERAGE' % rrd_path,
            'DEF:free=%s:free:AVERAGE' % rrd_path,
            'DEF:cached=%s:cached:AVERAGE' % rrd_path,
            'DEF:buffers=%s:buffers:AVERAGE' % rrd_path,
            'CDEF:a1=total,',
            'CDEF:a2=a1,1024,/,',
            'CDEF:a3=a2,1024,/,',
            'CDEF:a4=a3,1024,/,',
            'CDEF:b1=free,',
            'CDEF:b2=b1,1024,/,',
            'CDEF:b3=b2,1024,/,',
            'CDEF:b4=b3,1024,/,',
            'CDEF:c1=cached,',
            'CDEF:c2=c1,1024,/,',
            'CDEF:c3=c2,1024,/,',
            'CDEF:c4=c3,1024,/,',
            'CDEF:d1=buffers,',
            'CDEF:d2=d1,1024,/,',
            'CDEF:d3=d2,1024,/,',
            'CDEF:d4=d3,1024,/,',
            'AREA:a4#FFFF00:"total"',
            'AREA:b4#00ff00:"free"',
            'AREA:c4#0000ff:"cached"',
            'AREA:d4#FF6633:"buffers"'
            ])

    try:
        local_exec(cmd)
    except:
        raise

def fetch():
    pass

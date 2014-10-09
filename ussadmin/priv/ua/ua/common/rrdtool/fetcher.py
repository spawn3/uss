#!/usr/bin/python
#-*- coding: utf-8 -*-

from ua.common.rrdtool.utils import mkdirs, local_exec
from ua.common.rrdtool.tools import load, mem, swap, nw, iops

import os
import time
import re


def fetch(rrds_dir, node):
    """
    :param node: dict of the node, like: {'id':1, 'rack':1}
    """
    rack_dir = os.path.join(rrds_dir, str(node['rack']))
    try: mkdirs(rack_dir)
    except: raise

    node_dir = os.path.join(rack_dir, str(node['id']))
    try: mkdirs(node_dir)
    except: raise

    load_rrd_path = os.path.join(node_dir, 'load.rrd')
    mem_rrd_path = os.path.join(node_dir, 'mem.rrd')
    swap_rrd_path = os.path.join(node_dir, 'swap.rrd')
    nw_rrd_path = os.path.join(node_dir, 'nw.rrd')
    iops_rrd_path = os.path.join(node_dir, 'iops.rrd')

    try:
#        load_out, load_err = load.fetch(load_rrd_path)
#        if load_err: raise Exception(load_err)

#        mem_out, mem_err = mem.fetch(mem_rrd_path)
#        if mem_err: raise Exception(mem_err)
#
#        swap_out, swap_err = swap.fetch(swap_rrd_path)
#        if swap_err: raise Exception(swap_err)
#
#        nw_out, nw_err = nw.fetch(nw_rrd_path)
#        if nw_err: raise Exception(nw_err)
#
#        iops_out, iops_err = iops.fetch(iops_rrd_path)
#        if iops_err: raise Exception(iops_err)

#        if load_out: print load_out

        load_time, load1, load5, load15 = load.fetch_last(load_rrd_path)

        print 'load_time: '+ str(load_time)
        print 'load1: '+ str(load1)
        print 'load5: '+ str(load5)
        print 'load15: '+ str(load15)

        return {
#                'load':load_out
#                'mem':mem_out,
#                'swap':swap_out,
#                'nw':nw_out,
#                'iops':iops_out
                }
    except:
        raise


if __name__ == '__main__':
    rrds_dir = '/root/rrds/'
    imgs_dir = '/root/rrds/imgs/'

    nodes = [{'id':1, 'rack':1},
             {'id':2, 'rack':1},
             {'id':3, 'rack':2}]

#    create(nodes, rrds_dir)

    node = {'id':1, 'rack':1}
    data = {'load':[0.54, 0.45, 0.48],
            'mem':[12340, 5678, 9101, 2354],
            'swap':[12340, 5678],
            'nw':[12340, 5678],
            'iops':[12340, 5678]}

    for i in range(100):
        update(rrds_dir, node, data)

    graph(rrds_dir, imgs_dir, node)

    fetch(rrds_dir, node)

#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20110215
Author: zkz
"""

from ua.common.bridge import moni as bridge_moni
from ua.common.utils import mkdirs, get_log
from ua.common.rrdtool.tools import load, mem, swap, nw, iops, disk
from ua.common.rrdtool import grapher

import os
import time

def prepare_data():
    try:
        rslt = bridge_moni.node()
    except:
        raise Exception('Bridge_moni get message error')

    nodes_moni = []
    racks_moni = {}
    glob_moni = {'load':[0, 0, 0],
                 'mem':[0, 0, 0, 0],
                 'swap':[0, 0],
                 'nw':[0, 0],
                 'iops':[0, 0],
                 'disk':[0, 0]}
    for n in rslt:
        node = {}
        rack_moni = {}

        if n['rack']:
            node['rack'] = n['rack']
        else:
            continue

        if n['id']:
            node['id'] = n['id']
        else:
            continue

        ###################################################################
        if str(n['rack']) not in racks_moni.keys():
            racks_moni[str(n['rack'])] = {'load':[0, 0, 0],
                                          'mem':[0, 0, 0, 0],
                                          'swap':[0, 0],
                                          'nw':[0, 0],
                                          'iops':[0, 0],
                                          'disk':[0, 0],
                                          'node_count':0}

        racks_moni[str(n['rack'])]['node_count'] += 1
        ###################################################################

        if n['load']['avg1'] and n['load']['avg5'] and n['load']['avg15']:
            load_avg1 = n['load']['avg1']
            load_avg5 = n['load']['avg5']
            load_avg15 = n['load']['avg15']
            node['load'] = [load_avg1, load_avg5, load_avg15]

            rack_load = [racks_moni[str(n['rack'])]['load'][0],
                         racks_moni[str(n['rack'])]['load'][1],
                         racks_moni[str(n['rack'])]['load'][2]]

            rack_load = [rack_load[0]+load_avg1,
                         rack_load[1]+load_avg5,
                         rack_load[2]+load_avg15]
            racks_moni[str(n['rack'])]['load'] = rack_load

            glob_moni['load'] = [glob_moni['load'][0]+load_avg1,
                                 glob_moni['load'][1]+load_avg5,
                                 glob_moni['load'][2]+load_avg15]
        else:
            node['load'] = [0, 0, 0]
        ###################################################################

        if n['mem']['total'] and n['mem']['free'] and n['mem']['cached']and n['mem']['buffers']:
            mem_total = n['mem']['total']
            mem_free = n['mem']['free']
            mem_cached = n['mem']['cached']
            mem_buffers = n['mem']['buffers']
            node['mem'] = [mem_total, mem_free, mem_cached, mem_buffers]

            rack_mem = [racks_moni[str(n['rack'])]['mem'][0],
                        racks_moni[str(n['rack'])]['mem'][1],
                        racks_moni[str(n['rack'])]['mem'][2],
                        racks_moni[str(n['rack'])]['mem'][3]]

            racks_mem = [rack_mem[0]+mem_total,
                         rack_mem[1]+mem_free,
                         rack_mem[2]+mem_cached,
                         rack_mem[3]+mem_buffers]
            racks_moni[str(n['rack'])]['mem'] = rack_mem

            glob_moni['mem'] = [glob_moni['mem'][0]+mem_total,
                                glob_moni['mem'][1]+mem_free,
                                glob_moni['mem'][2]+mem_cached,
                                glob_moni['mem'][3]+mem_buffers]
        else:
            node['mem'] = [0, 0, 0, 0]

        if n['swap']['total'] and n['swap']['free']:
            swap_total = n['swap']['total']
            swap_free = n['swap']['free']
            node['swap'] = [swap_total, swap_free]

            rack_swap = [racks_moni[str(n['rack'])]['swap'][0],
                         racks_moni[str(n['rack'])]['swap'][1]]

            rack_swap = [rack_swap[0]+swap_total,
                         rack_swap[1]+swap_free]
            racks_moni[str(n['rack'])]['swap'] = rack_mem

            glob_moni['swap'] = [glob_moni['swap'][0]+swap_total,
                                 glob_moni['swap'][1]+swap_free]
        else:
            node['swap'] = [0, 0]

        if n['nw']['in'] and n['nw']['out']:
            nw_in = n['nw']['in']
            nw_out = n['nw']['out']
            node['nw'] = [nw_in, nw_out]

            rack_nw = [racks_moni[str(n['rack'])]['nw'][0],
                       racks_moni[str(n['rack'])]['nw'][1]]

            rack_nw = [rack_nw[0]+nw_in,
                       rack_nw[1]+nw_out]
            racks_moni[str(n['rack'])]['nw'] = rack_nw

            glob_moni['nw'] = [glob_moni['nw'][0]+nw_in,
                               glob_moni['nw'][1]+nw_out]
        else:
            node['nw'] = [0, 0]

        if n['iops']['read'] and n['iops']['write']:
            iops_read = n['iops']['read']
            iops_write = n['iops']['write']
            node['iops'] = [iops_read, iops_write]

            rack_iops = [racks_moni[str(n['rack'])]['iops'][0],
                         racks_moni[str(n['rack'])]['iops'][1]]

            rack_iops = [rack_iops[0]+iops_read,
                         rack_iops[1]+iops_write]
            racks_moni[str(n['rack'])]['iops'] = rack_iops

            glob_moni['iops'] = [glob_moni['iops'][0]+iops_read,
                               glob_moni['iops'][1]+iops_write]
        else:
            node['iops'] = [0, 0]

        if n['disk']['total'] and n['disk']['used']:
            disk_total = n['disk']['total']
            disk_used = n['disk']['used']
            node['disk'] = [disk_total, disk_used]

            rack_disk = [racks_moni[str(n['rack'])]['disk'][0],
                         racks_moni[str(n['rack'])]['disk'][1]]

            rack_disk = [rack_disk[0]+disk_total,
                         rack_disk[1]+disk_used]
            racks_moni[str(n['rack'])]['disk'] = rack_mem

            glob_moni['disk'] = [glob_moni['disk'][0]+disk_total,
                                 glob_moni['disk'][1]+disk_used]
        else:
            node['disk'] = [0, 0]

        nodes_moni.append(node)

    if len(rslt) > 0:
        glob_moni['load'] = [i/len(rslt) for i in glob_moni['load']]


    for rack_id in racks_moni.keys():
        node_count = racks_moni[rack_id]['node_count']
        if node_count > 0:
            racks_moni[rack_id]['load'] = [i/node_count for i in glob_moni['load']]

    return nodes_moni, racks_moni, glob_moni


def update_glob(rrds_dir, glob_moni):
    glob_rrds_dir = os.path.join(rrds_dir, 'glob')
    try: mkdirs(glob_rrds_dir)
    except: raise

    load_rrd_path = os.path.join(glob_rrds_dir, 'load.rrd')
    mem_rrd_path = os.path.join(glob_rrds_dir, 'mem.rrd')
    swap_rrd_path = os.path.join(glob_rrds_dir, 'swap.rrd')
    nw_rrd_path = os.path.join(glob_rrds_dir, 'nw.rrd')
    iops_rrd_path = os.path.join(glob_rrds_dir, 'iops.rrd')
    disk_rrd_path = os.path.join(glob_rrds_dir, 'disk.rrd')



    try:
        if 'load' in glob_moni.keys() and glob_moni['load']: load.update(load_rrd_path, glob_moni['load'])
        if 'mem' in glob_moni.keys() and glob_moni['mem']: mem.update(mem_rrd_path, glob_moni['mem'])
        if 'swap' in glob_moni.keys() and glob_moni['swap']: swap.update(swap_rrd_path, glob_moni['swap'])
        if 'nw' in glob_moni.keys() and glob_moni['nw']: nw.update(nw_rrd_path, glob_moni['nw'])
        if 'iops' in glob_moni.keys() and glob_moni['iops']: iops.update(iops_rrd_path, glob_moni['iops'])
        if 'disk' in glob_moni.keys() and glob_moni['disk']: disk.update(disk_rrd_path, glob_moni['disk'])
    except:
        raise


def update_racks(rrds_dir, racks_moni):

    for rack_id in racks_moni.keys():
        rack_rrds_dir = os.path.join(rrds_dir, str(rack_id), 'rack')
        try: mkdirs(rack_rrds_dir)
        except: raise

        load_rrd_path = os.path.join(rack_rrds_dir, 'load.rrd')
        mem_rrd_path = os.path.join(rack_rrds_dir, 'mem.rrd')
        swap_rrd_path = os.path.join(rack_rrds_dir, 'swap.rrd')
        nw_rrd_path = os.path.join(rack_rrds_dir, 'nw.rrd')
        iops_rrd_path = os.path.join(rack_rrds_dir, 'iops.rrd')
        disk_rrd_path = os.path.join(rack_rrds_dir, 'disk.rrd')

        try:
            if 'load' in racks_moni[rack_id].keys() and racks_moni[rack_id]['load']:
                load.update(load_rrd_path, racks_moni[rack_id]['load'])
            if 'mem' in racks_moni[rack_id].keys() and racks_moni[rack_id]['mem']:
                mem.update(mem_rrd_path, racks_moni[rack_id]['mem'])
            if 'swap' in racks_moni[rack_id].keys() and racks_moni[rack_id]['swap']:
                swap.update(swap_rrd_path, racks_moni[rack_id]['swap'])
            if 'nw' in racks_moni[rack_id].keys() and racks_moni[rack_id]['nw']:
                nw.update(nw_rrd_path, racks_moni[rack_id]['nw'])
            if 'iops' in racks_moni[rack_id].keys() and racks_moni[rack_id]['iops']:
                iops.update(iops_rrd_path, racks_moni[rack_id]['iops'])
            if 'disk' in racks_moni[rack_id].keys() and racks_moni[rack_id]['disk']:
                disk.update(disk_rrd_path, racks_moni[rack_id]['disk'])
        except:
            raise


def update_nodes(rrds_dir, nodes_moni):
    """
    :param node: dict of the node, like: {'id':1, 'rack':1},
    :param node: dict of the node would be updated into rrd, likes:
                    {'load':[],
                     'mem':[],
                     'swap':[],
                     'nw':[],
                     'iops':[]}
    """
    for node in nodes_moni:
        node_dir = os.path.join(rrds_dir, str(node['rack']), str(node['id']))
        try: mkdirs(node_dir)
        except: raise

        load_rrd_path = os.path.join(node_dir, 'load.rrd')
        mem_rrd_path = os.path.join(node_dir, 'mem.rrd')
        swap_rrd_path = os.path.join(node_dir, 'swap.rrd')
        nw_rrd_path = os.path.join(node_dir, 'nw.rrd')
        iops_rrd_path = os.path.join(node_dir, 'iops.rrd')
        disk_rrd_path = os.path.join(node_dir, 'disk.rrd')

        try:
            if 'load' in node.keys() and node['load']: load.update(load_rrd_path, node['load'])
            if 'mem' in node.keys() and node['mem']: mem.update(mem_rrd_path, node['mem'])
            if 'swap' in node.keys() and node['swap']: swap.update(swap_rrd_path, node['swap'])
            if 'nw' in node.keys() and node['nw']: nw.update(nw_rrd_path, node['nw'])
            if 'iops' in node.keys() and node['iops']: iops.update(iops_rrd_path, node['iops'])
            if 'disk' in node.keys() and node['disk']: disk.update(disk_rrd_path, node['disk'])
        except:
            raise

def update(rrds_dir, imgs_dir):
    """
    :param node: dict of the node, like: {'id':1, 'rack':1},
    :param node: dict of the node would be updated into rrd, likes:
                    {'load':[],
                     'mem':[],
                     'swap':[],
                     'nw':[],
                     'iops':[]}
    """


    try:
        nodes_moni, racks_moni, glob_moni = prepare_data()
        update_nodes(rrds_dir, nodes_moni)
        update_racks(rrds_dir, racks_moni)
        update_glob(rrds_dir, glob_moni)
        grapher.graph(rrds_dir, imgs_dir)
        #print prepare_data()
    except:
        raise

def update_pure(rrds_dir, imgs_dir):
    #pure表示不涉及到grapher
    """
    :param node: dict of the node, like: {'id':1, 'rack':1},
    :param node: dict of the node would be updated into rrd, likes:
                    {'load':[],
                     'mem':[],
                     'swap':[],
                     'nw':[],
                     'iops':[]}
    """


    try:
        nodes_moni, racks_moni, glob_moni = prepare_data()
        update_nodes(rrds_dir, nodes_moni)
        update_racks(rrds_dir, racks_moni)
        update_glob(rrds_dir, glob_moni)
    except:
        raise

if __name__ == '__main__':
    nodes_moni, racks_moni, glob_moni = prepare_data()
    print nodes_moni

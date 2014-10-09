#!/usr/bin/python
#-*- coding: utf-8 -*-

from ua.common.bridge import node as bridge_node
from ua.common.utils import mkdirs, local_exec
from ua.common.rrdtool.tools import load, mem, swap, nw, iops, disk

import os
import time
import re


def graph_glob(rrds_dir, imgs_dir):
    glob_rrds_dir = os.path.join(rrds_dir, 'glob')
    try: mkdirs(glob_rrds_dir)
    except: raise

    load_rrd_path = os.path.join(glob_rrds_dir, 'load.rrd')
    mem_rrd_path = os.path.join(glob_rrds_dir, 'mem.rrd')
    swap_rrd_path = os.path.join(glob_rrds_dir, 'swap.rrd')
    nw_rrd_path = os.path.join(glob_rrds_dir, 'nw.rrd')
    iops_rrd_path = os.path.join(glob_rrds_dir, 'iops.rrd')
    disk_rrd_path = os.path.join(glob_rrds_dir, 'disk.rrd')

    glob_imgs_dir = os.path.join(imgs_dir, 'glob')
    try: mkdirs(glob_imgs_dir)
    except: raise

    load_img_path = os.path.join(glob_imgs_dir, 'load.png')
    mem_img_path = os.path.join(glob_imgs_dir, 'mem.png')
    swap_img_path = os.path.join(glob_imgs_dir, 'swap.png')
    nw_img_path = os.path.join(glob_imgs_dir, 'nw.png')
    iops_img_path = os.path.join(glob_imgs_dir, 'iops.png')
    disk_img_path = os.path.join(glob_imgs_dir, 'disk.png')

    try:
        if os.path.exists(load_rrd_path): load.graph(load_rrd_path, load_img_path)
        if os.path.exists(mem_rrd_path): mem.graph(mem_rrd_path, mem_img_path)
        if os.path.exists(swap_rrd_path): swap.graph(swap_rrd_path, swap_img_path)
        if os.path.exists(nw_rrd_path): nw.graph(nw_rrd_path, nw_img_path)
        if os.path.exists(iops_rrd_path): iops.graph(iops_rrd_path, iops_img_path)
        if os.path.exists(disk_rrd_path): disk.graph(disk_rrd_path, disk_img_path)
    except:
        raise


def graph_racks(rrds_dir, imgs_dir, rack=None):
    if rack:
        racks = [rack]
    else:
#        ro = RackOpt()
#        racks = ro.select()
        try:
            racks = bridge_node.rack_list()
        except:
            raise

    for rack in racks:
#        print "rack:", rack
        rack_rrds_dir = os.path.join(rrds_dir, str(rack['id']), 'rack')
        try: mkdirs(rack_rrds_dir)
        except: raise

        load_rrd_path = os.path.join(rack_rrds_dir, 'load.rrd')
        mem_rrd_path = os.path.join(rack_rrds_dir, 'mem.rrd')
        swap_rrd_path = os.path.join(rack_rrds_dir, 'swap.rrd')
        nw_rrd_path = os.path.join(rack_rrds_dir, 'nw.rrd')
        iops_rrd_path = os.path.join(rack_rrds_dir, 'iops.rrd')
        disk_rrd_path = os.path.join(rack_rrds_dir, 'disk.rrd')

        rack_imgs_dir = os.path.join(imgs_dir, str(rack['id']), 'rack')
        try: mkdirs(rack_imgs_dir)
        except: raise

        load_img_path = os.path.join(rack_imgs_dir, 'load.png')
        mem_img_path = os.path.join(rack_imgs_dir, 'mem.png')
        swap_img_path = os.path.join(rack_imgs_dir, 'swap.png')
        nw_img_path = os.path.join(rack_imgs_dir, 'nw.png')
        iops_img_path = os.path.join(rack_imgs_dir, 'iops.png')
        disk_img_path = os.path.join(rack_imgs_dir, 'disk.png')

        try:
            if os.path.exists(load_rrd_path): load.graph(load_rrd_path, load_img_path)
            if os.path.exists(mem_rrd_path): mem.graph(mem_rrd_path, mem_img_path)
            if os.path.exists(swap_rrd_path): swap.graph(swap_rrd_path, swap_img_path)
            if os.path.exists(nw_rrd_path): nw.graph(nw_rrd_path, nw_img_path)
            if os.path.exists(iops_rrd_path): iops.graph(iops_rrd_path, iops_img_path)
            if os.path.exists(disk_rrd_path): disk.graph(disk_rrd_path, disk_img_path)
        except:
            raise


def graph_nodes(rrds_dir, imgs_dir, node=None):
    """
    :param node: dict of the node, like: {'id':1, 'rack':1}
    """
    if node:
        nodes = [node]
    else:
        try:
            nodes = bridge_node.node_list()
        except:
            raise

    for n in nodes:
        node_dir = os.path.join(rrds_dir, str(n['rack']), str(n['id']))
        try: mkdirs(node_dir)
        except: raise

        load_rrd_path = os.path.join(node_dir, 'load.rrd')
        mem_rrd_path = os.path.join(node_dir, 'mem.rrd')
        swap_rrd_path = os.path.join(node_dir, 'swap.rrd')
        nw_rrd_path = os.path.join(node_dir, 'nw.rrd')
        iops_rrd_path = os.path.join(node_dir, 'iops.rrd')
        disk_rrd_path = os.path.join(node_dir, 'disk.rrd')

        imgs_node_dir = os.path.join(imgs_dir, str(n['rack']), str(n['id']))
        try: mkdirs(imgs_node_dir)
        except: raise

        load_img_path = os.path.join(imgs_node_dir, 'load.png')
        mem_img_path = os.path.join(imgs_node_dir, 'mem.png')
        swap_img_path = os.path.join(imgs_node_dir, 'swap.png')
        nw_img_path = os.path.join(imgs_node_dir, 'nw.png')
        iops_img_path = os.path.join(imgs_node_dir, 'iops.png')
        disk_img_path = os.path.join(imgs_node_dir, 'disk.png')

        try:
            if os.path.exists(load_rrd_path): load.graph(load_rrd_path, load_img_path)
            if os.path.exists(mem_rrd_path): mem.graph(mem_rrd_path, mem_img_path)
            if os.path.exists(swap_rrd_path): swap.graph(swap_rrd_path, swap_img_path)
            if os.path.exists(nw_rrd_path): nw.graph(nw_rrd_path, nw_img_path)
            if os.path.exists(iops_rrd_path): iops.graph(iops_rrd_path, iops_img_path)
            if os.path.exists(disk_rrd_path): disk.graph(disk_rrd_path, disk_img_path)
        except:
            raise


def graph(rrds_dir, imgs_dir):
    try:
        graph_glob(rrds_dir, imgs_dir)
        graph_racks(rrds_dir, imgs_dir)
        #print 'graph'
        graph_nodes(rrds_dir, imgs_dir)
    except:
        raise

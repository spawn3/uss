#!/usr/bin/python
#-*- coding: utf-8 -*-

import os
import shlex
import subprocess

base_dir = os.path.dirname(__file__)

import settings

from oss.models import *
import xml.etree.ElementTree as ET
import threading
import time
from erlapi import *

def run_cmd(cmd):
    # print '***cmd*** %s' % cmd
    args = shlex.split(cmd)
    res = subprocess.call(args, stdout=subprocess.PIPE)
    return res

class ThreadMonitor(threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)
        self.status = False

    def run(self):
        self.status = True
        interval = settings.INTERVAL_TIME * 2
        while True:
            try:
                self._run()
            except Exception as error:
                pass
            time.sleep(interval)

    def _run(self):
        if True:
            # info = api_get_request('/uss/edog_web/slaves_dumpxml')
            info = api_get_request('/agents')
            try:
                xmlTree = ET.fromstring(info)
                # print("%s:This is a very very good xml doc" % __file__)
                for node in xmlTree.findall('node'):
                    monitor = Monitor()
                    pm_id = node.attrib['name']
                    monitor.pm_id = pm_id.split('@')[1]
                    monitor.time = node.attrib['time']
                    avg1 = node.find('avg1')
                    monitor.cpuAvg1 = avg1.attrib['value']
                    avg5 = node.find('avg5')
                    monitor.cpuAvg5 = avg5.attrib['value']
                    avg15 = node.find('avg15')
                    monitor.cpuAvg15 = avg15.attrib['value']
                    total = node.find('total')
                    monitor.memTotal = total.attrib['value']
                    free = node.find('free')
                    monitor.memFree = free.attrib['value']
                    cached = node.find('cached')
                    monitor.memCached = cached.attrib['value']
                    buffered = node.find('buffered')
                    monitor.memBuffered = buffered.attrib['value']
                    totalswap = node.find('totalswap')
                    monitor.swapTotal = totalswap.attrib['value']
                    freeswap = node.find('freeswap')
                    monitor.swapFree = freeswap.attrib['value']

                    netflow = node.find('netflow')
                    monitor.netRecv = netflow.attrib['total_rx']
                    monitor.netSend = netflow.attrib['total_tx']

                    #monitor.save()
                    self.rrd_factory(monitor)
                    self.rrd_updater(monitor)
                    self.rrd_grapher(monitor)
            except Exception, e:
                print("%s: This may be a bad doc" % __file__)
                print("Error:",e)

    def __build_rrd_path(self, hostid):
        rrdsdir = settings.MONITOR_PATH+'/rrds'
        hostid = str(hostid)

        load_rrd = os.path.join(rrdsdir, hostid, 'load.rrd')
        mem_rrd  = os.path.join(rrdsdir, hostid, 'mem.rrd')
        swap_rrd = os.path.join(rrdsdir, hostid, 'swap.rrd')
        net_rrd  = os.path.join(rrdsdir, hostid, 'net.rrd')

        return (load_rrd, mem_rrd, swap_rrd, net_rrd)

    def rrd_factory(self, monitor):
        rrdsdir = settings.MONITOR_PATH+'/rrds'
        hostid = monitor.pm_id

        if not os.path.exists(os.path.join(rrdsdir, str(hostid))):
            os.makedirs(os.path.join(rrdsdir, str(hostid)))

        (load_rrd, mem_rrd, swap_rrd, net_rrd) = self.__build_rrd_path(hostid)

        load_cmd = """rrdtool create %s \
                --start %d --step %d \
                DS:load_one:GAUGE:20:U:U \
                DS:load_five:GAUGE:20:U:U \
                DS:load_fifteen:GAUGE:20:U:U \
                RRA:AVERAGE:0.5:1:720 \
                RRA:AVERAGE:0.5:6:720 \
                RRA:AVERAGE:0.5:60:720 \
                RRA:AVERAGE:0.5:288:720 """ % (
                        load_rrd,
                        int(time.time()),
                        settings.INTERVAL_TIME * 2
                        )

        mem_cmd = """rrdtool create %s \
                --start %d --step %d \
                DS:mem_total:GAUGE:20:U:U \
                DS:mem_used:GAUGE:20:U:U \
                DS:mem_cached:GAUGE:20:U:U \
                DS:mem_buffered:GAUGE:20:U:U \
                RRA:AVERAGE:0.5:1:720 \
                RRA:AVERAGE:0.5:6:720 \
                RRA:AVERAGE:0.5:60:720 \
                RRA:AVERAGE:0.5:288:720 """ % (
                        mem_rrd,
                        int(time.time()),
                        settings.INTERVAL_TIME * 2
                        )

        swap_cmd = """rrdtool create %s \
                --start %d --step %d \
                DS:swap_total:GAUGE:20:U:U \
                DS:swap_used:GAUGE:20:U:U \
                RRA:AVERAGE:0.5:1:720 \
                RRA:AVERAGE:0.5:6:720 \
                RRA:AVERAGE:0.5:60:720 \
                RRA:AVERAGE:0.5:288:720 """ % (
                        swap_rrd,
                        int(time.time()),
                        settings.INTERVAL_TIME * 2
                        )

        net_cmd = """rrdtool create %s \
                --start %d --step %d \
                DS:send:COUNTER:20:U:U \
                DS:recv:COUNTER:20:U:U \
                RRA:AVERAGE:0.5:1:720 \
                RRA:AVERAGE:0.5:6:720 \
                RRA:AVERAGE:0.5:60:720 \
                RRA:AVERAGE:0.5:288:720 """ % (
                        net_rrd,
                        int(time.time()),
                        settings.INTERVAL_TIME * 2
                        )

        if not os.path.exists(load_rrd):
            run_cmd(load_cmd)
        if not os.path.exists(mem_rrd):
            run_cmd(mem_cmd)
        if not os.path.exists(swap_rrd):
            run_cmd(swap_cmd)
        if not os.path.exists(net_rrd):
            run_cmd(net_cmd)

        return True

    def rrd_updater(self, monitor):
        (load_rrd, mem_rrd, swap_rrd, net_rrd) = self.__build_rrd_path(monitor.pm_id)

        memtotal = int(monitor.memTotal)
        memused = memtotal - int(monitor.memFree)

        load_one = float(monitor.cpuAvg1)
        load_five = float(monitor.cpuAvg5)
        load_fifteen = float(monitor.cpuAvg15)
        swaptotal = int(monitor.swapTotal)
        swapused = swaptotal - int(monitor.swapFree)

        memcached = int(monitor.memCached)
        membuffered = int(monitor.memBuffered)
        netRecv = int(monitor.netRecv)
        netSend = int(monitor.netSend)

        now = int(time.time())

        # print load_one, load_five, load_fifteen
        load_cmd = 'rrdtool update %s %d:%f:%f:%f' % (
                load_rrd,
                now,
                load_one,
                load_five,
                load_fifteen
                )

        mem_cmd = 'rrdtool update %s %d:%f:%f:%f:%f' % (
                mem_rrd,
                now,
                memtotal,
                memused,
                memcached,
                membuffered
                )
        swap_cmd = 'rrdtool update %s %d:%d:%d' % (
                swap_rrd,
                now,
                swaptotal,
                swapused
                )

        net_cmd = 'rrdtool update %s %d:%d:%d' % (
                net_rrd,
                now,
                netSend,
                netRecv
                )

        run_cmd(load_cmd)
        run_cmd(mem_cmd)
        run_cmd(swap_cmd)
        run_cmd(net_cmd)

        return True

    def rrd_grapher(self, monitor):
        end = 'now'
        start = 'end-30m'

        rrdsdir = settings.MONITOR_PATH+'/rrds'
        hostid = monitor.pm_id

        load_rrd = os.path.join(rrdsdir, hostid, 'load.rrd')
        mem_rrd  = os.path.join(rrdsdir, hostid, 'mem.rrd')
        swap_rrd = os.path.join(rrdsdir, hostid, 'swap.rrd')
        net_rrd  = os.path.join(rrdsdir, hostid, 'net.rrd')

        load_png = os.path.join(rrdsdir, hostid, 'load.png')
        mem_png  = os.path.join(rrdsdir, hostid, 'mem.png')
        swap_png = os.path.join(rrdsdir, hostid, 'swap.png')
        net_png  = os.path.join(rrdsdir, hostid, 'net.png')

        load_cmd = """rrdtool graph %s \
                --end=%s --start=%s \
                -w 300 \
                -h 80 \
                -t "loadAvg" -v "size" \
                --lower-limit 0 --rigid \
                DEF:avg1=%s:load_one:AVERAGE \
                DEF:avg5=%s:load_five:AVERAGE \
                DEF:avg15=%s:load_fifteen:AVERAGE \
                LINE2:avg1#ffff00:"avg1" \
                LINE2:avg5#00ff00:"avg5" \
                LINE2:avg15#0000ff:"avg15"
                """ % (
                        load_png,
                        end,
                        start,
                        load_rrd,
                        load_rrd,
                        load_rrd
                        )

        mem_cmd = """rrdtool graph %s \
                --end=%s --start=%s \
                -w 300 \
                -h 80 \
                -t "Memory" -v "GB" \
                --lower-limit 0 \
                --rigid \
                DEF:memtotal=%s:mem_total:AVERAGE \
                DEF:memused=%s:mem_used:AVERAGE \
                DEF:memcached=%s:mem_cached:AVERAGE \
                DEF:membuffered=%s:mem_buffered:AVERAGE \
                CDEF:a1=memtotal, \
                CDEF:a2=a1,1024,/,\
                CDEF:a3=a2,1024,/,\
                CDEF:a4=a3,1024,/,\
                CDEF:b1=memused, \
                CDEF:b2=b1,1024,/,\
                CDEF:b3=b2,1024,/,\
                CDEF:b4=b3,1024,/,\
                CDEF:c1=memcached, \
                CDEF:c2=c1,1024,/,\
                CDEF:c3=c2,1024,/,\
                CDEF:c4=c3,1024,/,\
                CDEF:d1=membuffered, \
                CDEF:d2=d1,1024,/,\
                CDEF:d3=d2,1024,/,\
                CDEF:d4=d3,1024,/,\
                AREA:a4#ffff00:"total" \
                AREA:b4#00ff82:"used" \
                AREA:c4#00b35b:"cached" \
                AREA:d4#ff8000:"buffered"
                """ % (
                        mem_png,
                        end,
                        start,
                        mem_rrd,
                        mem_rrd,
                        mem_rrd,
                        mem_rrd,
                        )

        swap_cmd = """rrdtool graph %s \
                --end=%s --start=%s \
                -w 300 \
                -h 80 \
                -t "Swap" -v "size" \
                --lower-limit 0 --rigid \
                DEF:swaptotal=%s:swap_total:AVERAGE \
                DEF:swapused=%s:swap_used:AVERAGE \
                LINE2:swaptotal#00ff00:"total" \
                LINE2:swapused#ff0000:"used"
                """ % (
                        swap_png,
                        end,
                        start,
                        swap_rrd,
                        swap_rrd
                        )

        net_cmd = """rrdtool graph %s  \
                --end=%s --start=%s \
                -w 300 \
                -h 80 \
                -t "network" -v "bytes" \
                --lower-limit 0 --rigid \
                DEF:send=%s:send:AVERAGE \
                DEF:recv=%s:recv:AVERAGE \
                AREA:send#ffff00:"send" \
                AREA:recv#00ff00:"recv"
                """ % (
                        net_png,
                        end,
                        start,
                        net_rrd,
                        net_rrd
                        )

        run_cmd(load_cmd)
        run_cmd(mem_cmd)
        run_cmd(swap_cmd)
        run_cmd(net_cmd)
        return True

# global
the_monitor = ThreadMonitor()

def start_monitor():
    if not the_monitor.status:
        the_monitor.start()
        return "monitor start ok"
    else:
        return "monitor is running"

start_monitor()

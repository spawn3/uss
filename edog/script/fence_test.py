#!/usr/bin/env python

import commands
import fcntl
import os
import re
import sys
import string
import struct
import socket
import time
import thread
from threading import Thread

NUM = 0

def get_nic_ip(ifname):
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    localIP = socket.inet_ntoa(fcntl.ioctl(s.fileno(), 0x8915, struct.pack('256s' ,ifname[:15]))[20:24])
    return localIP

cmd = """cat /sysy/yfs/etc/yfs.conf  |grep '\<network\>' |awk '{print $2}' |awk -F';' '{print $1}' """
(status,output) = commands.getstatusoutput(cmd)
LOCAL_NETWORK= output

cmd = "netstat -nr |grep " + "'" + output  + " '" + " |awk '{print $NF}'"
(status,output) = commands.getstatusoutput(cmd)
LOCAL_NIC = output

try:
    LOCAL_IP = get_nic_ip(output)
except:
    LOCAL_IP = 'error'

class testping(Thread):
   def __init__ (self,ip):
      Thread.__init__(self)
      self.ip = ip
      self.status = -1

   def run(self):
        global NUM

        cmd = 'ping -c 2 -i 0.1 -w 1 ' + self.ip
        (status, output) = commands.getstatusoutput(cmd)
        if status == 0:
                NUM += 1

def log_append(message):
    f = file('/var/log/uss/fence_test_edog.log', 'a')
    f.write('[%s %s] %s\n' % (time.time(), time.strftime('%x %X'), message))
    f.close()

class arping(Thread):
    def __init__ (self,ip, total):
        Thread.__init__(self)
        self.ip = ip
        self.total = total
        self.deadline = '10'

    def run(self):
        global NUM
        global LOCAL_NIC
        global LOCAL_IP

        if self.ip == LOCAL_IP:
            # log_append('=> %s ok' % (self.ip))
            NUM += 1
        else:
            cmd = 'ping -c 2 -i 0.1 -w 1 ' + self.ip + ' > /dev/null'
            os.system(cmd)
            cmd = 'arping -c 10 ' + self.ip + ' -f ' + ' -I ' + LOCAL_NIC + ' -w ' + self.deadline
            (status, output) = commands.getstatusoutput(cmd)
            if status == 0:
                # log_append('=> %s ok' % (self.ip))
                NUM += 1
            else:
                log_append('!! %s failed' % (cmd))
                pass


if __name__ == '__main__':
    iplist = []
    if len(sys.argv) == 1:
        self_pid = os.getpid()
        #os.mkdir('/dev/shm/sysy')
        fence_list = '/dev/shm/sysy/fence_%s.lst' % self_pid

        cmd = """cat /sysy/yfs/etc/cluster.conf  | awk '{print $1}' > %s""" % fence_list
        os.system(cmd)

        fd = open(fence_list, 'r')
        for line in fd.readlines():
            line = ''.join(line.split())
            iplist.append(line)
        fd.close()
        os.remove(fence_list)
    else:
        iplist = sys.argv[1:]
    limit = len(iplist) / 2 + 1
    log_append('==> %s, limit %d' % (iplist, limit))

    print iplist
    pinglist = []
    for ip in iplist:
        current = arping(ip, len(iplist))
        pinglist.append(current)
        current.start()

    for pingle in pinglist:
        pingle.join()

    log_append('<== passed/total: %d/%d' % (NUM, len(iplist)))
    if NUM >= limit or limit == 1:
        sys.exit (0)
    else:
        sys.exit(64)

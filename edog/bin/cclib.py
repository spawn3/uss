#!/usr/bin/env python
# -*- coding: utf-8 -*-

#################################################################################
# cluster/system
#
# racks
#   GET
#   POST
#   DELETE
# nodes
#   GET
#   POST
#   DELETE
# services
#   GET
#   POST
#   DELETE
# actions
#   POST
#################################################################################

import os
import sys
import httplib
import json
import shlex
import subprocess
import commands
import re
import time
from xml.etree.ElementTree import ElementTree
from threading import Thread

current_dir = os.path.dirname(os.path.realpath(__file__))

########################################################################
##
########################################################################
def _build_response(status, reason, data):
    return {'status': status, 'reason':reason, 'data':data}

class UssConn:
    def __init__(self, host, port, timeout=3):
        self.host = host
        self.port = port
        self.conn = httplib.HTTPConnection(host, port, timeout=timeout)
        #self.conn.set_debuglevel(1)
    def __del__(self):
        self.conn.close()
    def connect(self, host, port, timeout=3):
        self.host = host
        self.port = port
        self.conn = httplib.HTTPConnection(host, port, timeout=timeout)
    def close(self):
        self.conn.close()
    def request(self, method, url, params='', headers={}):
        try:
            headers["Content-Type"] = "application/json"
            if params:
                body = json.dumps(params)
            else:
                body = ''
            self.conn.request(method=method, url=url, headers=headers, body=body)
            resp = self.conn.getresponse()
            data = resp.read()
            json_rep = json.loads(data)
            return _build_response(resp.status, resp.reason, json_rep)
        except ValueError, e:
            return _build_response(resp.status, resp.reason, data)
        except httplib.BadStatusLine, e:
            return _build_response(httplib.INTERNAL_SERVER_ERROR, e, '')
        except httplib.HTTPException, e:
            return _build_response(httplib.INTERNAL_SERVER_ERROR, e, '')
        except Exception, e:
            return _build_response(httplib.INTERNAL_SERVER_ERROR, e, '')

    def get(self, url):
        return self.request('GET', url)
    def post(self, url, params, headers):
        return self.request(method='POST', url=url, params=params, headers=headers)
    def delete(self, url):
        return self.request('DELETE', url)

########################################################################
##
########################################################################
class UssResource:
    def __init__(self, conn, base_url):
        self.conn = conn
        self.base_url = base_url
    def get(self):
        return self.conn.get(self.base_url)
    def post(self, params, headers={}):
        return self.conn.post(self.base_url, params, headers)
    def delete(self):
        return self.conn.delete(self.base_url)

def http_get(host, port, url, timeout=6):
    conn = UssConn(host, port, timeout)
    resource = UssResource(conn, url)
    res = resource.get()
    return res
def http_post(host, port, url, params, headers={}, timeout=6):
    conn = UssConn(host, port, timeout)
    resource = UssResource(conn, url)
    res = resource.post(params)
    return res
def http_delete(host, port, url, timeout=6):
    conn = UssConn(host, port, timeout)
    resource = UssResource(conn, url)
    res = resource.delete()
    return res

########################################################################
##
########################################################################
def print_rack(rack):
    #print rack
    for k, v in rack.iteritems():
        print '*** %20s : %-20s' % (k, v)

def print_items():
    l = res['data']
    for i in l:
        print '*** %s' % i
        #print_rack(i)

########################################################################
##
########################################################################
def run_cmd(cmd, verbose=False):
    print_verbose('=> cmd: [ %s ]' % cmd, verbose)
    args = shlex.split(cmd)
    res = subprocess.call(args)
    return res

def node_to_ip(node):
    return str(node.split('@')[1])

def local_ips():
    ips = commands.getoutput("/sbin/ifconfig | grep -i inet | grep -iv inet6 | awk {'print $2'} | sed -ne 's/addr\:/ /p'")
    return ips

def sort_ips(ips):
    ips = list(set(ips))
    ips.sort()
    return ips

def format_info(d):
    for k, v in d.iteritems():
        print '%20s: %s' % (k, v)

def sleep(seconds, verbose=True):
    for x in range(seconds):
        if verbose:
            print '...'
        time.sleep(1)

def print_verbose(info, verbose=False):
    if verbose:
        print info

def handle_res(res, verbose=False):
    if verbose:
        print res
    elif isinstance(res, dict):
        if res.has_key('status') and res['status'] > 300:
            print res
        elif res.has_key('data') and isinstance(res['data'], dict) and res['data'].has_key('error'):
            print res

def is_nodeup(ip):
    cmd = 'ssh root@%s echo' % ip
    (status,output) = commands.getstatusoutput(cmd)
    if status != 0:
        print status, output
        return False
    return True

def ask_yn(prompt, complaint='Yes or no, please!'):
    while True:
        yn = raw_input(prompt)
        if yn in ('y', 'ye', 'yes'):
            return True
        elif yn in ('n', 'no'):
            return False
        return complaint

## -----------------------------------------------------------------------
## MANAGER
## -----------------------------------------------------------------------
def _get_managers():
    cmd = 'escript %s/get_masters.erl' % current_dir
    status, output = commands.getstatusoutput(cmd)
    if status == 0:
        return output.split()
    else:
        print (status, output)
        return []

def get_managers():
    l = [node_to_ip(x) for x in _get_managers()]
    return sort_ips(l)

def ping_manager(ip, port, verbose=False):
    res = http_get(ip, port, '/', 3)
    if res['status'] < 300:
        if verbose:
            format_info(res['data'])
        if res['data'].has_key('node'):
            node = res['data']['node']
            return node_to_ip(node)
    return ''

def get_active_manager(managers, port, verbose=False):
    for ip in managers:
        if ping_manager(ip, port, verbose):
            return ip;
    return ''

#!/usr/bin/python
#-*- coding: utf-8 -*-

from ua.common.secty.json_api import moni as rest_moni
from ua.common.ua_conf import *

def node(host=edog_host, port=edog_port, id=None):
    try:
        if id:
            rslt = rest_moni(host=host, port=port, id=id)
        else:
            rslt = rest_moni(host=host, port=port)
    except:
        raise Exception('rest/moni get message error')
    return rslt

if __name__ == "__main__":
    print node("192.168.1.21", port=edog_port, id=None)


"""
[
    {
        'status': u'running',
        'load': {'avg5': 0.01, 'cpu_used': 0.018032786885245903, 'avg15': 0.050000000000000003, 'avg1': 0.0},
        'uptime': {u'user': 5, u'time': u'8:36'},
        'disk': {'df': [{u'avail': 6087544, u'used': 13505604, u'capacity': 20641788, u'util': 0.68999999999999995, u'device': u'/dev/sda10', u'mountpoint': u'/media/sda10', u'smart': u'undefined'}, {u'avail': 38085844, u'used': 180236, u'capacity': 40313964, u'util': 0.01, u'device': u'/dev/sda5', u'mountpoint': u'/media/sda5', u'smart': u'undefined'}, {u'avail': 13106044, u'used': 24520264, u'capacity': 39674192, u'util': 0.66000000000000003, u'device': u'/dev/sda7', u'mountpoint': u'/home', u'smart': u'undefined'}, {u'avail': 82192, u'used': 102211, u'capacity': 194442, u'util': 0.56000000000000005, u'device': u'/dev/sda6', u'mountpoint': u'/boot', u'smart': u'undefined'}, {u'avail': 1024884, u'used': 2016, u'capacity': 1026900, u'util': 0.01, u'device': u'tmpfs', u'mountpoint': u'/dev/shm', u'smart': u'undefined'}, {u'avail': 1026748, u'used': 152, u'capacity': 1026900, u'util': 0.01, u'device': u'udev', u'mountpoint': u'/dev', u'smart': u'undefined'}, {u'avail': 1026900, u'used': 0, u'capacity': 1026900, u'util': 0.0, u'device': u'varlock', u'mountpoint': u'/var/lock', u'smart': u'undefined'}, {u'avail': 1026764, u'used': 136, u'capacity': 1026900, u'util': 0.01, u'device': u'varrun', u'mountpoint': u'/var/run', u'smart': u'undefined'}, {u'avail': 1026900, u'used': 0, u'capacity': 1026900, u'util': 0.0, u'device': u'tmpfs', u'mountpoint': u'/lib/init/rw', u'smart': u'undefined'}, {u'avail': 1761204, u'used': 7807216, u'capacity': 10080488, u'util': 0.81999999999999995, u'device': u'/dev/sda8', u'mountpoint': u'/', u'smart': u'undefined'}], 'total': 116039374, 'free': 64255024, 'used': 46117835},
        'mem': {'cached': 810065920, 'total': 2103095296, 'buffers': 208306176, 'free': 366403584},
        'ip': u'192.168.1.201',
        'hostname': u'undefined',
        'rack': 1,
        'cluster': 0,
        'iops': {'read': None, 'write': None},
        'swap': {'total': 2048057344, 'free': 2048057344},
        'cpu': [{u'count': 2, u'physical_id': u'0', u'model_name': u'Genuine Intel(R) CPU 2140 @ 1.60GHz'}],
        'id': 1,
        'nw': {'dtls': [{u'info': u'undefined', u'rx_bytes': 266, u'ip': u'192.168.1.201', u'mask': u'255.255.255.0', u'mac': u'00:e0:a0:1a:b8:ee', u'tx_bytes': 922, u'ifname': u'eth4', u'speed': u'100Mb/s', u'bcast': u'192.168.1.255'}], 'out': 922, 'in': 266}}
]
"""

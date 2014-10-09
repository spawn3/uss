#!/usr/bin/env python
# -*- coding: utf-8 -*-

import json
import httplib
import time
from utils import request


"""
http://localhost:9601/uss/misc/syst
Request:
{
    "action":"start"
}

http://localhost:9601/uss/misc/syst
Request:
{
    "action":"stop"
}


http://localhost:9600/ear/srvc/stat
随时POST返回service信息:
[node_id, service_type, service_id, prev_status, status, info]
"""

class RestCtrl(object):
    def __init__(self, host, port=80, url='/uss/misc'):
        self.host = host
        self.port = port
        self.url = url

    def deploy(self, headers={}):
        return self.fetch(headers={}, opt='deploy')

    def test(self, headers={}):
        return self.fetch(headers=headers, opt='test')

    def start(self, headers={}, ip=None, service=None, serviceId=None):
        return self.fetch(headers=headers, opt='start', ip=ip, service=service, serviceId=serviceId)

    def stop(self, headers={}, ip=None, service=None, serviceId=None):
        return self.fetch(headers={}, opt='stop', ip=ip, service=service, serviceId=serviceId)

    def fetch(self, headers={}, opt='test', ip=None, service=None, serviceId=None):
        d = {}
        d['action'] = opt
        if ip:
            d['ip'] = ip
        if service:
            d['type'] = service
        if serviceId:
            d['n'] = serviceId
        url = self.url + '/syst'
        return self._fetch(d=d, url=url)

    def _fetch(self, headers={}, d={}, method='POST', url=None):
        params = json.dumps(d)
        headers["Content-Type"] = "application/json"
        host = self.host
        port = self.port
        method = method
        try:
            resp = request(host=host, method=method, url=url,
                                 headers=headers, params=params, port=port)
            print d, headers, host, method, url, port, resp
            return json.loads(resp)
        except:
            raise Exception('Bad response from %s' %(host))

    def node_join(self, id, headers={}):
        d = {}
        d['action'] = 'node_join'
        d['id'] = id
        url = self.url + '/syst'
        return self._fetch(d=d, url=url)

    def node_exit(self, id, headers={}):
        d = {}
        d['action'] = 'node_exit'
        d['id'] = id
        url = self.url + '/syst'
        return self._fetch(d=d, url=url)

    def set_mds_c60(self, mds_list, c60_list, headers={}):
        d = {}
        d['action'] = 'set_mds_c60'
        d['mds'] = mds_list
        d['c60'] = c60_list
        url = self.url + '/syst'
        return self._fetch(d=d, url=url)

    def set_ylvm(self, action, name = None, size = None):
        """
            action: create, resize, list
            transId: 是每一个事件的id，用来从监听的事件中，查找是否成功
        """
        stand_action = ["lvm_create", "lvm_resize", "lvm_list"]
        action = "lvm_" + action
        d = {}
        d['action'] = action
        d['name'] = name
        d['size'] = size 
        url = self.url + "/syst"
        return self._fetch(d=d, url=url)

    def get_log(self, ip, stype, n, level = None, lines = None):
        """
            action:
            stype: mds, cds, c60, nfs, cifs, ftp, http, proxy ....
            n: 在某节点上stype的id值
            transId: 是每一个事件的id，用来从监听的事件中，查找是否成功
        """
        d = {}
        d['action'] = "get_log"
        d['ip'] = ip
        d['type'] = stype
        d['n'] = n
        d['level'] = level
        d['lines'] = lines
        url = self.url + "/syst"
        return self._fetch(d=d, url=url)

    def clean_log_all(self):
        """
        """
        d = {}
        d['action'] = "clean_log"
        url = self.url + "/syst"
        return self._fetch(d=d, url=url)

    def dump_log_all(self):
        """
        """
        d = {}
        d['action'] = "dump_log"
        url = self.url + "/syst"
        return self._fetch(d=d, url=url)

    def get_conf(self):
        """
            action:
            stype: mds, cds, c60, nfs, cifs, ftp, http, proxy ....
            n: 在某节点上stype的id值
            transId: 是每一个事件的id，用来从监听的事件中，查找是否成功
        """

        d = {}
        d['action'] = "get_conf"
        url = self.url + "/syst"
        return self._fetch(d=d, url=url)

        
"""
http://localhost:9601/uss/misc/moni
Request:
{
    id:1,
}
Response:
[
   {
      "atime":1294992918,
      "ip":"192.168.1.201",
      "hostname":"node1",
      "rack":"1",
      "user":"root",
      "time":1295416771,
      "password":"root",
      "id":1,
      "status":"running",
      "info":{
         "hostname":"thefive.192.168.1.1",
         "netflow":"undefined",
         "uptime":{
            "user":9,
            "time":"4:22"
         },
         "cpu":{
            "load":{
               "avg5":0.66000000000000003,
               "avg15":0.29999999999999999,
               "avg1":1.5600000000000001
            },
            "used":45.833333333333329,
            "spec":[
               {
                  "count":2,
                  "physical_id":"0",
                  "model_name":"Genuine Intel(R) CPU 2140 @ 1.60GHz"
               }
            ]
         },
         "mem":{
            "mem_cached":732172288,
            "swap_free":2048053248,
            "mem_free":238755840,
            "mem_buffers":257409024,
            "swap_total":2048053248,
            "mem_total":2083622912
         },
         "network":[
            {
               "info":"undefined",
               "rx_bytes":31430187,
               "ip":"192.168.1.201",
               "mask":"255.255.255.0",
               "mac":"00:e0:a0:1a:b8:ee",
               "tx_bytes":4744186,
               "ifname":"eth4",
               "speed":"100Mb/s",
               "bcast":"192.168.1.255"
            }
         ],
         "disk":[
            {
               "avail":"1884960",
               "used":"7683460",
               "capacity":"10080488",
               "util":"81",
               "device":"/dev/sda8",
               "mountpoint":"/",
               "smart":"undefined"
            },
            {
               "avail":"1017392",
               "used":"0",
               "capacity":"1017392",
               "util":"0",
               "device":"tmpfs",
               "mountpoint":"/lib/init/rw",
               "smart":"undefined"
            }
         ]
      }
   },
   {
      "status":"shutoff",
      "info":[

      ],
      "atime":1294996057,
      "ip":"192.168.1.202",
      "hostname":"node2",
      "rack":"1",
      "user":"root",
      "time":0,
      "password":"root",
      "id":2
   }
]
"""


def moni(host, port=80, url='/uss/misc/moni', id=None, limit={}, headers={}):
    """
    Modify self.moni_dict and return it as this function's return value.

    :param id: node id
    :param headers: HTTP Request headers

    :returns: [{
                    'cpu':[{  'physical_id': '0',
                              'count': 2,
                              'model_name': 'Genuine Intel(R) CPU 2140 @ 1.60GHz'
                              }]

                    'load': {
                                'cpu_used':0.21,
                                'avg1':1.56,
                                'avg5':0.66,
                                'avg15':0.30
                                }
                    'mem': {
                                "total":2083622912,
                                "free":238755840,
                                "cached":732172288,
                                "buffers":257409024

                               }
                    'swap': {
                                "total":2048053248,
                                "free":2048053248
                               }
                    'nw': [{
                               'info':'undefined',
                               'rx_bytes':31430187,
                               'ip':'192.168.1.201',
                               'mask':'255.255.255.0',
                               'mac':'00:e0:a0:1a:b8:ee',
                               'tx_bytes':4744186,
                               'ifname':'eth4',
                               'speed':'100Mb/s',
                               'bcast':'192.168.1.255'
                            }]
                    'drv':[{
                               'avail':'1884960',
                               'used':'7683460',
                               'capacity':'10080488',
                               'util':'81',
                               'device':'/dev/sda8',
                               'mountpoint':'/',
                               'smart':'undefined'
                                }]
                }]
    """
    d = {}
    if id:
        d['id'] = id
    elif limit:
        d['limit'] = limit

    try:
        params = json.dumps(d)
        headers["Content-Type"] = "application/json"

        moni_json = request(host=host, method='POST', url=url,
                                 headers=headers, params=params, port=port)
        moni_dict = json.loads(moni_json)
    except:
        raise
    #############################################################
    if moni_dict == {'result': 'empty'}:
        return {}
    elif moni_dict == {"error": ""}:
        raise Exception('%s response error'%(host))
    else:
        pass
    #############################################################

    moni_rslt = []
    for n in moni_dict:
        node = {}

        status = ""
        try:
            status = n['status']
            node['status'] = status
        except:
            node['status'] = None

        try: node['id'] = n['id']
        except: node['id'] = None

        try: node['ip'] = n['ip']
        except: node['ip'] = None

        try: node['hostname'] = n['hostname']
        except: node['hostname'] = None

        try: node['rack'] = n['rack']
        except: node['rack'] = None

        try: node['cluster'] = n['cluster']
        except: node['cluster'] = None

        if status != 'unavailable':
            try:
                node['uptime'] = n['info']['uptime']
            except:
                node['uptime'] = None
        else:
            node['uptime'] = None

        if status != 'unavailable':
            try:
                node['cpu'] = n['info']['cpu']['spec']
            except:
                node['cpu'] = None
        else:
            node['cpu'] = None

        if status != 'unavailable':
            try:
                node['load'] = {'cpu_used':float(n['info']['cpu']['used']),
                                'avg1'    :float(n['info']['cpu']['load']['avg1']),
                                'avg5'    :float(n['info']['cpu']['load']['avg5']),
                                'avg15'   :float(n['info']['cpu']['load']['avg15'])}
            except:
                node['load'] = {'cpu_used':None,
                                'avg1'    :None,
                                'avg5'    :None,
                                'avg15'   :None}
        else:
            node['load'] = {'cpu_used':None,
                            'avg1'    :None,
                            'avg5'    :None,
                            'avg15'   :None}

        if status != 'unavailable':
            try:
                node['mem'] = {'total'  :n['info']['mem']['mem_total'],
                               'free'   :n['info']['mem']['mem_free'],
                               'cached' :n['info']['mem']['mem_cached'],
                               'buffers':n['info']['mem']['mem_buffers']}
            except:
                node['mem'] = {'total':None,
                               'free':None,
                               'cached':None,
                               'buffers':None}
        else:
            node['mem'] = {'total':None,
                           'free':None,
                           'cached':None,
                           'buffers':None}

        if status != 'unavailable':
            try:
                node['swap'] = {'total':n['info']['mem']['swap_total'],
                                'free' :n['info']['mem']['swap_free']}
            except:
                node['swap'] = {'total':None,
                                'free' :None}
        else:
            node['swap'] = {'total':None,
                            'free' :None}

        if status != 'unavailable':
            node['nw'] = {}
            try:
                node['nw']['in'] = 0
                node['nw']['out'] = 0
                for nw in n['info']['network']:
                    node['nw']['in'] = node['nw']['in'] + nw['rx_bytes']
                    node['nw']['out'] = node['nw']['out'] + nw['tx_bytes']
                node['nw']['dtls'] = n['info']['network']
            except:
                node['nw'] = {'in':None,
                              'out':None,
                              'dtls':None}
        else:
            node['nw'] = {'in':None,
                          'out':None,
                          'dtls':None}

        if status != 'unavailable':
            try:
                node['iops'] = {'read':n['info']['iops']['read'],
                                'write' :n['info']['iops']['write']}
            except:
                node['iops'] = {'read':None,
                                'write' :None}
        else:
            node['iops'] = {'read':None,
                            'write' :None}

        node['disk'] = {}
        if status != 'unavailable':
            try:
                total = 0
                used = 0
                free = 0
                for d in n['info']['disk']:
                    total += d['capacity']
                    used += d['used']
                    free += d['avail']



                node['disk']['df'] = n['info']['disk']
                node['disk']['total'] = total
                node['disk']['used'] = used
                node['disk']['free'] = free
            except:
                node['disk']['df'] = None
                node['disk']['total'] = None
                node['disk']['used'] = None
                node['disk']['free'] = None
        else:
            node['disk']['df'] = None
            node['disk']['total'] = None
            node['disk']['used'] = None
            node['disk']['free'] = None

        moni_rslt.append(node)

    return moni_rslt

######################################################################################
class RestSql(object):
    def __init__(self, host, who, port=80, url='/uss/sql'):
        """
        :param host: HTTP Request host
        :param port: HTTP Request port
        :param url: HTTP Request url
        :param who: SQL option on which table
        """
        self.host = host
        self.port = port
        self.url = url
        self.who = who

    def _fetch(self, headers={}, d={}, method='POST', url=None):
        params = json.dumps(d)
        headers["Content-Type"] = "application/json"
        host = self.host
        port = self.port
        method = method
        try:
            resp = request(host=host, method=method, url=url,
                                 headers=headers, params=params, port=port)
            print d, headers, host, method, url, port, resp
            return json.loads(resp)
        except:
            raise Exception('Bad response from %s' %(host))

    def insert(self, what, headers={}):
        """
        :param what: { "ip":"192.168.1.1", "hostname":"node1", "user":"root", "passwd":"root", "rack":"1" }
        :param headers: HTTP Request headers
        :returns: { id:0 }, id of the node inserted.
        """
        d = {}
        d['who'] = self.who
        d['what'] = what
        url = self.url + '/insert'
        return self._fetch(d=d, url=url)

    def update(self, what, where, headers={}):
        """
        :param what: {"hostname":"node2", "password":"mdsmds"},
        :param where: {"ip":"192.168.1.1"}
        :returns: { id:0 }, id of the node updated.
        """
        d = {}
        d['who'] = self.who
        d['what'] = what
        d['where'] = where
        url = self.url + '/update'
        return self._fetch(d=d, url=url)

    def select(self, what='*', where=None, limit=None, orderby=None, headers={}):
        """
        :param what: ["id", "ip", "name", "atime"],
        :param where: {"name":"node1"},
        :param limit: {"offset":"10","count":"20"},
        :param orderby: {"column":"id", "order":"desc"}
        :returns: [
                    {id:0, ip:192.168.1.1, name:node1, atime:foo},
                    {id:2, ip:192.168.1.1, name:node1, atime:foo}
                    ]
        """

        d = {}
        d['who'] = self.who
        d['what'] = what
        if where:
            d['where'] = where
        if limit:
            d['limit'] = limit
        if orderby:
            d['orderby'] = orderby
        url = self.url + '/select'
        return self._fetch(d=d, url=url)

    def delete(self, where, headers={}):
        """
        :param where: {"id":0}
        :returns: { id:0 }
        """
        d = {}
        d['who'] = self.who
        d['where'] = where
        url = self.url + '/delete'
        return self._fetch(d=d, url=url)

################################################################################
################################################################################
if __name__ == "__main__":
    restctrl = RestCtrl("192.168.1.201", port="9601", url="/uss/misc" )
#    print restctrl.set_ylvm("create", "test", 10)
#    print restctrl.set_ylvm("resize", "test", 10)
#    rest = restctrl.set_ylvm("list")
#    print rest
#    print type(rest)
#    print "---------------"

    #rest = restctrl.deploy()

#    rest = restctrl.set_mds_c60([], [])
    #rest = restctrl.node_exit(1)
#    rest = restctrl.node_join(2)
#    rest = restctrl.test()
#    print rest

#    print restctrl.get_log("192.168.1.201", "mds", 1, level = None, lines = None)
#    print restctrl.get_conf()

#    d = {}
#    d['action'] = "action"
#    d['name'] = "name"
#    d['size'] = "size"
#    d['time'] = time.time()
#
#    print d
#    params = json.dumps(d)
#    headers = {}
#    headers["Content-Type"] = "application/json"
#
#    resp = request(host="192.168.1.189", method='POST', url='/ear/srvc/stat',
#            headers=headers, params=params, port="9600")
#    print resp

    print restctrl.dump_log_all()
    print restctrl.clean_log_all()

    d = {}
    d["action"] = "lvm_list"
    d["name"] = ""
    d["size"] = ""
    params = json.dumps(d)
    headers = {}
    headers["Content-Type"] = "application/json"
    rslt = request("192.168.1.201", "POST", "/uss/misc/moni", headers, params, port=9601)
  #  print json.dumps(rslt)
    rslt = moni("192.168.1.201", 9601)
    print rslt


    rs = RestSql(host='192.168.1.15', port=9601, who='rack')
    print rs.select()



"""
    rs = RestSql(host='192.168.1.201', port=9601, who='service')
    print rs.select()
    结果：
[{u'status': u'shutoff', u'info': u'undefined', u'ip': u'192.168.1.201', u'pid': -1, u'analysis': [], u'n': 0, u'type': u'c60'}, {u'status': u'shutoff', u'info': u'undefined', u'ip': u'192.168.1.201', u'pid': -1, u'analysis': [], u'n': 1, u'type': u'cds'}, {u'status': u'shutoff', u'info': u'undefined', u'ip': u'192.168.1.201', u'pid': -1, u'analysis': [], u'n': 2, u'type': u'cds'}, {u'status': u'shutoff', u'info': u'undefined', u'ip': u'192.168.1.201', u'pid': -1, u'analysis': [], u'n': 3, u'type': u'cds'}, {u'status': u'shutoff', u'info': u'undefined', u'ip': u'192.168.1.201', u'pid': -1, u'analysis': [], u'n': 4, u'type': u'cds'}, {u'status': u'shutoff', u'info': u'undefined', u'ip': u'192.168.1.201', u'pid': -1, u'analysis': [], u'n': 1, u'type': u'mds'}, {u'status': u'shutoff', u'info': u'undefined', u'ip': u'192.168.1.201', u'pid': -1, u'analysis': [], u'n': 0, u'type': u'nfs'}, {u'status': u'shutoff', u'info': u'undefined', u'ip': u'192.168.1.201', u'pid': -1, u'analysis': [], u'n': 0, u'type': u'proxy'}]
"""

"""
    rs = RestSql(host='192.168.1.201', port=9601, who='node')
    print rs.select()
secty :utils: resp: <httplib.HTTPResponse instance at 0x7fd7b15cbcf8>
[{u'status': u'running', u'atime': 1301476524, u'ip': u'192.168.1.201', u'hostname': u'undefined', u'rack': 1, u'cluster': 0, u'user': u'undefined', u'password': u'undefined', u'id': 1}]
"""

"""
    rs = RestSql(host='192.168.1.201', port=9601, who='rack')
    print rs.select()
secty :utils: resp: <httplib.HTTPResponse instance at 0x7f9776ccecf8>
[{u'atime': 1301476524, u'id': 1, u'name': u'rack1'}, {u'atime': 1301476524, u'id': 2, u'name': u'rack2'}]
"""

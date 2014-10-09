#!/usr/bin/python
#-*- coding: utf-8 -*-

from ua.common.secty.json_api import RestCtrl
from ua.common.secty.json_api import RestSql
from ua.common.secty.json_api import moni as rest_moni
from ua.common.ua_conf import *

import json

class SystCtrl(RestCtrl):
    def __init__(self, host=edog_host, port=edog_port, misc_url=misc_url):
        RestCtrl.__init__(self, host=host, port=port, url=misc_url)

############################################################################################
"""[{"id":7,"name":"rack7","atime":1294904753},
    {"id":6,"name":"rack6","atime":1294903938},
    {"id":5,"name":"rack5","atime":1294903522}]
"""

class ClusterOpt():
    def __init__(self, host=edog_host, port=edog_port, sql_url=sql_url):
        self.cs = RestSql(host=host, who='cluster', port=port, url=sql_url)

    def __call__(self):
        return self

    def select(self, what='*', where=None, limit=None, orderby=None, headers={}):
        print what, where, limit, orderby, headers
        print 'rslt = self.cs.select(what=what, where=where, limit=limit, orderby=orderby, headers=headers)'
        rslt = self.cs.select(what=what, where=where, limit=limit, orderby=orderby, headers=headers)
        if rslt == {'result':'empty'} or not rslt:
            return []
        return rslt


class RackOpt():
    def __init__(self, host=edog_host, port=edog_port, sql_url=sql_url):
        self.rs = RestSql(host=host, who='rack', port=port, url=sql_url)

    def __call__(self):
        return self

    def select(self, what='*', where=None, limit=None, orderby=None, headers={}):
        rslt = self.rs.select(what=what, where=where, limit=limit, orderby=orderby, headers=headers)
        if rslt == {'result':'empty'} or not rslt:
            return []
        return rslt

    def insert(self, name, headers={}):
        """
        :param what: {"name":"rack1" }
        """
        what = {'name':name}
        return self.rs.insert(what=what, headers=headers)

    def update(self, what, where, headers={}):
        """
        :param what: {"name":"rack2"},
        :param where: {"id":"1"}

        :returns: { id:0 }, id of the node updated.
        """
        return self.rs.update(what=what, where=where, headers=headers)

    def delete(self, where, headers={}):
        """
        :param where: {"id":0}

        :returns: { id:0 }
        """
        return self.rs.delete(where=where, headers=headers)


"""
[{u'atime': 1294912705, u'ip': u'192.168.1.4', u'hostname': u'node4', u'rack': u'1', u'user': u'root', u'password': u'root', u'id': 4},
{u'atime': 1294983713, u'ip': u'192.168.1.201', u'hostname': u'node1', u'rack': u'1', u'user': u'root', u'password': u'root', u'id': 5}]
"""

class NodeOpt():
    def __init__(self, host=edog_host, port=edog_port, sql_url=sql_url, misc_url=misc_url):
        self.rs = RestSql(host=host, who='node', port=port, url=sql_url)

    def __call__(self):
        return self

    def select(self, what='*', where=None, limit=None, orderby=None, headers={}):
        rslt = self.rs.select(what=what, where=where, limit=limit, orderby=orderby, headers=headers)
        if rslt == {'result':'empty'} or not rslt:
            return []
        return rslt

    def insert(self, what, headers={}):
        """
        :param what: { "ip":"192.168.1.1", "hostname":"node1", "user":"root", "passwd":"root", "rack":"1" }
        """
        return self.rs.insert(what=what, headers=headers)

    def update(self, what, where, headers={}):
        """
        :param what: {"hostname":"node2", "password":"mdsmds"},
        :param where: {"ip":"192.168.1.1"}

        :returns: { id:0 }, id of the node updated.
        """
        return self.rs.update(what=what, where=where, headers=headers)

    def delete(self, where, headers={}):
        """
        :param where: {"id":0}

        :returns: { id:0 }
        """
        return self.rs.delete(where=where, headers=headers)

    def moni(self, id=None, limit={}, headers={}):
        """
        Modify self.moni_dict and return it as this function's return value.

        :param id: node id
        :param headers: HTTP Request headers

        :returns: {
                        'cpu':{
                                'Intel i3, 2.4Ghz':8,
                                'Intel i7, 2.2Ghz':8
                                },
                        'cpu_used':0.21,
                        'load':[load1, load5, load15],
                        'mem':[total, used, buffers, cached],
                        'swap':[total, used],
                        'nw':[in, out],
                        'drv':[('Filesystem','1K-blocks','Use%','Mounted on'), ('/dev/sda3', 41943036, 0.69, '/media/wind')]
                    }
        """

        return rest_moni(host=edog_host, port=edog_port, id=id, limit=limit, headers=headers)

    def join(self, id, cluster_id=None):
        pass

    def exit(self, id):
        pass

#!/usr/bin/python
#-*- coding: utf-8 -*-

from ua.common.secty.json_api import RestCtrl
from ua.common.ua_conf import *

import json
import uuid
import time

NS = [{'id':1, 'name':'dir1', 'fileid':320, 'size':100 },
      {'id':2, 'name':'dir2', 'fileid':320, 'size':120 },
      {'id':3, 'name':'dir3', 'fileid':500, 'size':130 },
      {'id':4, 'name':'dir4', 'fileid':320, 'size':30 }]

class YlvmOpt():
    def __init__(self):
        self.rc = RestCtrl(host=edog_host, port=edog_port)

    def create(self, name, size):
        """
        <type 'dict'>
        {u'result': True}
        """
        ns = self.rc.set_ylvm("create", name, size)
        print "lvm create:", ns
        if not type(ns) == type({}):
            raise Exception("%s lvm create  error."%(edog_host))
        if "result" in ns.keys() and ns["result"] == True:
            pass
        else:
            raise Exception("%s lvm create  error."%(edog_host))

    def resize(self, name, size):
        """
        <type 'dict'>
        {u'result': True}
        """
        ns = self.rc.set_ylvm("resize", name, size)
#        print type(ns)
        print "lvm resize:", ns
        if not type(ns) == type({}):
            raise Exception("%s lvm resize  error."%(edog_host))
        if "result" in ns.keys() and ns["result"] == True:
            pass
        else:
            raise Exception("%s lvm resize  error."%(edog_host))

    def list(self):
        """
        """
        ns = self.rc.set_ylvm("list")
        l = 4
        ##下面对返回的结果进行约束检查是否满足
        if not type(ns) == type([]):
            raise Exception("%s lvm list返回数据格式操作，format error."%(edog_host))
        for i in ns:
            if not type(i) == type({}):
                raise Exception("%s lvm list返回数据格式操作，format error."%(edog_host))
            if not len(i) == 4:
                raise Exception("%s lvm list返回数据格式操作，format error."%(edog_host))
            keys = i.keys()
            if "id" not in keys or "name" not in keys \
                    or "fileid" not in keys or "size" not in keys:
                raise Exception("%s lvm list返回数据格式操作，format error."%(edog_host))
        return ns

if __name__ == "__main__":
    ylvmopt = YlvmOpt()
    ylvmopt.create("createname", "size")
    ylvmopt.resize("createename", "size")
    ylvmopt.list()

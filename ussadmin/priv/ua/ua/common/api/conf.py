#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101220
Author: zkz
"""

from ua.common.secty.json_api import RestCtrl
from ua.common.ua_conf import *

import json
import uuid
import time

class ConfOpt():
    def __init__(self):
        self.rc = RestCtrl(host=edog_host, port=edog_port)

    def get_conf(self):
        """
        """
        ns = self.rc.get_conf()
        ##下面对返回的结果进行约束检查是否满足
        l = 6
        wantkeys = ["doc", "value", "catagory", "mode", "key", "type"]
        if not type(ns) == type([]):
            raise Exception("edog get_conf返回数据格式操作，format error.")
        for i in ns:
            if not type(i) == type({}):
                raise Exception("edog get_conf 返回数据格式操作，format error.")
            if not len(i) == 6:
                raise Exception("edog get_conf 返回数据格式操作，format error.")
            keys = i.keys()
            for key in keys:
                if key not in wantkeys:
                    raise Exception("edog get_conf 返回数据格式操作，format error.")
        return ns

if __name__ == "__main__":
    confOpt = ConfOpt()
    print confOpt.get_conf()


    """
    [{u'doc': u'subnet ip', u'value': u'192.168.1.2', u'catagory': u'yfs', u'mode': u'undefined', u'key': u'network', u'type': u'undefined'}, {u'doc': u'subnet mask', u'value': u'255.255.255.0', u'catagory': u'yfs', u'mode': u'undefined', u'key': u'netmask', u'type': u'undefined'}, {u'doc': u'mds count', u'value': 1, u'catagory': u'yfs', u'mode': u'undefined', u'key': u'mds_count', u'type': u'undefined'}, {u'doc': u'cds count', u'value': 2, u'catagory': u'yfs', u'mode': u'undefined', u'key': u'cds_count', u'type': u'undefined'}, {u'doc': u'client count', u'value': 1, u'catagory': u'yfs', u'mode': u'undefined', u'key': u'client_count', u'type': u'undefined'}]
    """

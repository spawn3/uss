#!/usr/bin/python
#-*- coding: utf-8 -*-

from ua.common.secty.json_api import RestCtrl
from ua.common.ua_conf import *

import json
import uuid
import time

class YlogOpt():
    def __init__(self):
        self.rc = RestCtrl(host=edog_host, port=edog_port)

    def get_log(self, ip, stype, n, level = None, lines = None):
        """
        """

        rest = self.rc.get_log(ip, stype, n, level, lines)
        if not type(rest) == type({}):
            raise Exception("%s response error.get log"%(ip))
        if "result" in rest.keys() and len(rest) == 1:
            pass
        else:
            raise Exception("%s response error.get log"%(ip))
        return rest["result"].split("\n")

    def clean_log_all(self):
        """
        """
        rest = self.rc.clean_log_all()
        if not type(rest) == type({}):
            raise Exception("%s response error.get log"%(ip))
        if "result" in rest.keys() and len(rest) == 1:
            pass
        else:
            raise Exception("%s response error.get log"%(ip))
        return rest
        pass

    def dump_log_all(self):
        """
        """
        rest = self.rc.dump_log_all()
        if not type(rest) == type({}):
            raise Exception("%s response error.get log"%(ip))
        if "result" in rest.keys() and len(rest) == 1:
            pass
        else:
            raise Exception("%s response error.get log"%(ip))
        return rest

    def get_log_all(self, ip, stype, n, level = None, lines = None):
        """
        """
        rest = self.rc.get_log("192.168.1.201", "mds", 1, level,  lines)
        return "log_all"


if __name__ == "__main__":
    ylogOpt = YlogOpt()
    rest = ylogOpt.get_log("192.168.1.201", "mds", 1, level = None, lines = None)
    print type(rest)
    for i in rest:
        print i
    print len(rest)
    pass

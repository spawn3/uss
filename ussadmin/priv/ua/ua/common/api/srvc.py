#!/usr/bin/python
#-*- coding: utf-8 -*-

from ua.common.secty.json_api import RestCtrl
from ua.common.secty.json_api import RestSql
from ua.common.ua_conf import *

import json

class SrvcOpt():
    def __init__(self, host=edog_host, port=edog_port, sql_url=sql_url, misc_url=misc_url):
        self.rs = RestSql(host=host, who='service', port=port, url=sql_url)
        self.rc = RestCtrl(host=host, port=port, url=misc_url)

    def __call__(self):
        return self

    def select(self, what='*', where=None, limit=None, orderby=None, headers={}):
        rslt = self.rs.select(what=what, where=where, limit=limit, orderby=orderby, headers=headers)
        if rslt == {'result':'empty'} or not rslt:
            return []
        return rslt

    def start(self, ip=None, service=None, serviceId=None):
        ''' '''
        return self.rc.start(ip=ip, service=service, serviceId=serviceId)

    def stop(self, ip=None, service=None, serviceId=None):
        ''' '''
        return self.rc.stop(ip=ip, service=servcie, serviceId=serviceId)

#    def insert(self, name, headers={}):
#        """
#        :param what: {"name":"rack1" }
#        """
#        what = {'name':name}
#        return self.rs.insert(what=what, headers=headers)
#
#    def update(self, what, where, headers={}):
#        """
#        :param what: {"name":"rack2"},
#        :param where: {"id":"1"}
#
#        :returns: { id:0 }, id of the node updated.
#        """
#        return self.rs.update(what=what, where=where, headers=headers)
#
#    def delete(self, where, headers={}):
#        """
#        :param where: {"id":0}
#
#        :returns: { id:0 }
#        """
#        return self.rs.delete(where=where, headers=headers)


if __name__ == "__main__":
    pass

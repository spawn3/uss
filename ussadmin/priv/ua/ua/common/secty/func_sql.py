#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101220
Author: zkz
"""

from utils import request

import json
import httplib

#def request(host, method, url, headers, params, port=80):
#    try:
#        conn = httplib.HTTPConnection(host, port)
#        conn.request(method=method, url=url, body=params, headers=headers)
#        resp = conn.getresponse()
#        data = resp.read()
#        conn.close()
#        return data
#    except:
#        raise


def insert(who, what, host, port=80, url='/uss/sql/insert', headers={}):
    """
    :param host: HTTP Request host
    :param port: HTTP Request port
    :param url: HTTP Request url
    :param who: SQL option on which table
    :param what: { "ip":"192.168.1.1", "hostname":"node1", "user":"root", "passwd":"root", "rack":"1" }
    :param headers: HTTP Request headers

    :returns: { id:0 }, id of the node inserted.
    """
    d = {}
    d['who'] = who
    d['what'] = what
    try:
        params = json.dumps(d)
        headers["Content-Type"] = "application/json"

        rslt = request(host=host, method='POST', url=url,
                       headers=headers, params=params, port=port)
        return json.loads(rslt)
    except:
        raise

def update(who, what, where, host, port=80, url='/uss/sql/update', headers={}):
    """
    :param host: HTTP Request host
    :param port: HTTP Request port
    :param url: HTTP Request url
    :param who: SQL option on which table
    :param what: {"hostname":"node2", "password":"mdsmds"},
    :param where: {"ip":"192.168.1.1"}

    :returns: { id:0 }, id of the node updated.
    """

    d = {}
    d['who'] = who
    d['what'] = what
    d['where'] = where

    try:
        params = json.dumps(d)
        headers["Content-Type"] = "application/json"

        rslt = request(host=host, method='POST', url=url,
                       headers=headers, params=params, port=port)
        return json.loads(rslt)
    except:
        raise

def select(who, host, port=80, url='/uss/sql/select', what='*', where=None, limit=None, orderby=None, headers={}):
    """
    :param host: HTTP Request host
    :param port: HTTP Request port
    :param url: HTTP Request url
    :param who: SQL option on which table
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
    d['who'] = who
    d['what'] = what
    if where:
        d['where'] = where
    if limit:
        d['limit'] = limit
    if orderby:
        d['orderby'] = orderby

    try:
        params = json.dumps(d)
        headers["Content-Type"] = "application/json"

        rslt = request(host=host, method='POST', url=url,
                       headers=headers, params=params, port=port)
        return json.loads(rslt)
    except:
        raise

def delete(who, where, host, port=80, url='/uss/sql/delete', headers={}):
    """
    :param host: HTTP Request host
    :param port: HTTP Request port
    :param url: HTTP Request url
    :param who: SQL option on which table
    :param where: {"id":0}

    :returns: { id:0 }
    """
    d = {}
    d['who'] = who
    d['where'] = where

    try:
        params = json.dumps(d)
        headers["Content-Type"] = "application/json"

        rslt = request(host=host, method='POST', url=url,
                       headers=headers, params=params, port=port)
        return json.loads(rslt)
    except:
        raise

if __name__ == '__main__':
    print select(host='192.168.1.201', port=9601, who='service', where={'ip':'192.168.1.201'})

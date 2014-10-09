#!/usr/bin/python
#-*- coding: utf-8 -*-

import httplib

from ua.common.utils import get_log
from ua.common.ua_conf import *

request_log = get_log('request', conf_request_log)

def request(host, method, url, headers, params, port=80):
    msg = ""
    for i in [host, method, url, headers, params, port]:
        msg = msg + str(i) + ";"
    try:
        conn = httplib.HTTPConnection(host, port)
        conn.request(method=method, url=url, body=params, headers=headers)
        resp = conn.getresponse()
        data = resp.read()
        conn.close()
        msg = msg + ";" + "finished"
    except:
        msg = msg + ";" + "Bad response from :%s"%(host)
        raise Exception('Bad response from : %s'% host)
    request_log.info(msg)
    return data

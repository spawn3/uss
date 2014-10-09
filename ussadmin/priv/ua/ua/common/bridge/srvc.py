#!/usr/bin/python
#-*- coding: utf-8 -*-

from ua.common.api.srvc import SrvcOpt
from ua.common.ua_conf import *
from ua.common.yfs import get_service_list

def srvc_list_all():
    try:
        so = SrvcOpt()
        ss = so.select()
    except:
        raise
    if ss == {'result':'empty'} or not ss:
        return []
    else:
        return ss

def srvc_list(type=None):
    try:
        so = SrvcOpt()
        if not type:
            ss = so.select()
        else:
            ss = so.select(where={'type':type})
    except:
        raise

    if ss == {'result':'empty'} or not ss:
        ss = []
    elif ss == {'error': ''}:
        raise Exception("get error response for %s,  srvc_list"%(edog_host))

    service_list = get_service_list()
    service_dict = dict()
    for i in service_list:
        service_dict[i] = []

    #print ss
    for s in ss:
        type   = s['type']
        if type in service_list:
            service_dict[type].append(s)

    return service_dict

def srvc_list_in_status(status):
    try:
        result = srvc_list()
        service_list = get_service_list()
        d = dict()
        for i in service_list:
            d[i] = []

        for k, v in result.iteritems():
            d[k] = [x for x in v if x['status'] == status]
        return d
    except:
        raise

def srvc_running_list():
    return srvc_list_in_status('running')

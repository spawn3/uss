#!/usr/bin/python
#-*- coding: utf-8 -*-

from ua.common.secty.json_api import moni as rest_moni
from ua.common.api.node import RackOpt, NodeOpt
from ua.common.bridge import srvc as bridge_srvc
from ua.common.ua_conf import edog_host, edog_port

def rack_list(id=None):
    ro = RackOpt()
    try:
        if id:
            rslt = ro.select(where={'id':id})
        else:
            rslt = ro.select()
    except:
        raise
    print rslt
    if rslt == {'result':'empty'} or not rslt:
        return []
    elif rslt == {'error': ''}:
        raise Exception('%s response error, rack_list'%(edog_host))
    else:
        return rslt

def node_list(id=None, ip=None):
    no = NodeOpt()
    try:
        where = {}
        if id:
            where['id'] = id
        if ip:
            where['ip'] = ip
        if where:
            rslt = no.select(where=where)
        else:
            rslt = no.select()
    except:
        raise
    if rslt == {'result':'empty'} or not rslt:
        return []
    elif rslt == {'error': ''}:
        raise Exception('get error response from %s, bridge/node node_list'%(edog_host))
    else:
        return rslt

def node_newadded_list(id=None, ip=None):
    rslt = node_list(id=id, ip=ip)
    node_newadded = []
    for n in rslt:
        if n['cluster'] == -1:
            node_newadded.append(n)
    return node_newadded


def node_stat_list():
    try:
        rslt = rest_moni(host=edog_host, port=edog_port)
    except:
        raise
    if rslt == {'result':'empty'} or not rslt:
        return []
    running_list = []
    unavailable_list = []
    starting_list = []
    stopping_list = []
    sleeping_list = []
    for n in rslt:
    #######################################################
#        if not n['cluster'] > -1:
#            continue
    #######################################################
        if n['status'] == 'running':
            running_list.append(n)
        if n['status'] == 'unavailable':
            unavailable_list.append(n)
        if n['status'] == 'starting':
            starting_list.append(n)
        if n['status'] == 'stopping':
            stopping_list.append(n)
        if n['status'] == 'sleeping':
            sleeping_list.append(n)
        if n['status'] == 'shutoff':
            sleeping_list.append(n)
    return running_list, unavailable_list, starting_list, \
            stopping_list, sleeping_list

def _node_srvc_list():
    #这个函数需要重构， zhangjunfeng ,这个函数重构完成，命名为node_srvc_list，
    highlight = '<span class="highlight">%s</span>'
    healthy = '<span class="healthy">%s</span>'

    try:
        services = bridge_srvc.srvc_list()
        nodes = rest_moni(host=edog_host, port=edog_port)
    except:
        raise

    if nodes == {'result':'empty'}:
        return None
    elif not nodes:
        return None

    print 'node_srvc_list, service:%s'%service
    c60_list   = services['c60']
    mds_list   = services['mds']
    rjnld_list = services['rjnld']
    cds_list   = services['cds']
    nfs_list   = services['nfs']
    proxy_list = services['proxy']
    http_list  = services['http']
    cifs_list  = services['cifs']
    ftp_list   = services['ftp']

    for n in nodes:
    #######################################################
#        if not n['cluster'] > -1:
#            continue
    #######################################################
        n_mds_list = []
        n_cds_list = []
        n_c60_list = []
        n_rjnld_list = []
        n_nfs_list = []
        n_cifs_list = []
        n_ftp_list = []
        n_http_list = []
        n_proxy_list = []

        for mds in mds_list:
            if n['ip'] == mds['ip']:
                n_mds_list.append(mds)
        for cds in cds_list:
            if n['ip'] == cds['ip']:
                n_cds_list.append(cds)
        for c60 in c60_list:
            if n['ip'] == c60['ip']:
                n_c60_list.append(c60)
        for rjnld in rjnld_list:
            if n['ip'] == rjnld['ip']:
                n_rjnld_list.append(rjnld)
        for nfs in nfs_list:
            if n['ip'] == nfs['ip']:
                n_nfs_list.append(nfs)
        for cifs in cifs_list:
            if n['ip'] == cifs['ip']:
                n_cifs_list.append(cifs)
        for ftp in ftp_list:
            if n['ip'] == ftp['ip']:
                n_ftp_list.append(ftp)
        for http in http_list:
            if n['ip'] == http['ip']:
                n_http_list.append(http)
        for proxy in proxy_list:
            if n['ip'] == proxy['ip']:
                n_proxy_list.append(proxy)

        mds_running_list = []
        for mds in n_mds_list:
            if mds['status'] == 'running':
                mds_running_list.append(mds)
        len_mds_running = len(mds_running_list)
        len_mds = len(n_mds_list)

        cds_running_list = []
        for cds in n_cds_list:
            if cds['status'] == 'running':
                cds_running_list.append(cds)
        len_cds_running = len(cds_running_list)
        len_cds = len(n_cds_list)

        c60_running_list = []
        for c60 in n_c60_list:
            if c60['status'] == 'running':
                c60_running_list.append(c60)
        len_c60_running = len(c60_running_list)
        len_c60 = len(n_c60_list)

        rjnld_running_list = []
        for rjnld in n_rjnld_list:
            if rjnld['status'] == 'running':
                rjnld_running_list.append(rjnld)
        len_rjnld_running = len(rjnld_running_list)
        len_rjnld = len(n_rjnld_list)

        nfs_running_list = []
        for nfs in n_nfs_list:
            if nfs['status'] == 'running':
                nfs_running_list.append(nfs)
        len_nfs_running = len(nfs_running_list)
        len_nfs = len(n_nfs_list)

        cifs_running_list = []
        for cifs in n_cifs_list:
            if cifs['status'] == 'running':
                cifs_running_list.append(cifs)
        len_cifs_running = len(cifs_running_list)
        len_cifs = len(n_cifs_list)

        ftp_running_list = []
        for ftp in n_ftp_list:
            if ftp['status'] == 'running':
                ftp_running_list.append(ftp)
        len_ftp_running = len(ftp_running_list)
        len_ftp = len(n_ftp_list)

        http_running_list = []
        for http in n_http_list:
            if http['status'] == 'running':
                http_running_list.append(http)
        len_http_running = len(http_running_list)
        len_http = len(n_http_list)

        proxy_running_list = []
        for proxy in n_proxy_list:
            if proxy['status'] == 'running':
                proxy_running_list.append(proxy)
        len_proxy_running = len(proxy_running_list)
        len_proxy = len(n_proxy_list)

        ###################################################################################
        mds_part = ''
        if len_mds>0:
            if len_mds_running != len_mds:
                mds_part = highlight % ('M[%d/%d]' % (len_mds_running, len_mds))
            else:
                mds_part = healthy % ('M[%d/%d]' % (len_mds_running, len_mds))
        ###################################################################################
        c60_part = ''
        if len_c60>0:
            if len_c60_running != len_c60:
                c60_part = highlight % ('L[%d/%d]' % (len_c60_running, len_c60))
            else:
                c60_part = healthy % ('L[%d/%d]' % (len_c60_running, len_c60))
        ###################################################################################
        rjnld_part = ''
        if len_rjnld>0:
            if len_rjnld_running != len_rjnld:
                rjnld_part = highlight % ('R[%d/%d]' % (len_rjnld_running, len_rjnld))
            else:
                rjnld_part = healthy % ('R[%d/%d]' % (len_rjnld_running, len_rjnld))
        ###################################################################################
        cds_part = ''
        if len_cds>0:
            if len_cds_running != len_cds:
                cds_part = highlight % ('C[%d/%d]' % (len_cds_running, len_cds))
            else:
                cds_part = healthy % ('C[%d/%d]' % (len_cds_running, len_cds))
        ###################################################################################
        proxy_part = ''
        if len_proxy>0:
            if len_proxy_running != len_proxy:
                proxy_part = highlight % ('P[%d/%d]' % (len_proxy_running, len_proxy))
            else:
                proxy_part = healthy % ('P[%d/%d]' % (len_proxy_running, len_proxy))
        ###################################################################################
        nfs_part = ''
        if len_nfs>0:
            if len_nfs_running != len_nfs:
                nfs_part = highlight % ('N[%d/%d]' % (len_nfs_running, len_nfs))
            else:
                nfs_part = healthy % ('N[%d/%d]' % (len_nfs_running, len_nfs))
        ###################################################################################
        cifs_part = ''
        if len_cifs>0:
            if len_cifs_running != len_cifs:
                cifs_part = highlight % ('S[%d/%d]' % (len_cifs_running, len_cifs))
            else:
                cifs_part = healthy % ('S[%d/%d]' % (len_cifs_running, len_cifs))
        ###################################################################################
        ftp_part = ''
        if len_ftp>0:
            if len_ftp_running != len_ftp:
                ftp_part = highlight % ('F[%d/%d]' % (len_ftp_running, len_ftp))
            else:
                ftp_part = healthy % ('F[%d/%d]' % (len_ftp_running, len_ftp))
        ###################################################################################
        http_part = ''
        if len_http>0:
            if len_http_running != len_http:
                http_part = highlight % ('H[%d/%d]' % (len_http_running, len_http))
            else:
                http_part = healthy % ('H[%d/%d]' % (len_http_running, len_http))
        ###################################################################################
        n['srvc'] = '&nbsp;'.join([
                mds_part, c60_part, cds_part, rjnld_part,
                '<br/>',
                proxy_part, nfs_part, cifs_part, ftp_part, http_part])
    return node

def node_srvc_list():
    """给node这个数据结构添加一个srvc的项 """
    #这个函数上上面函数的重构，测试通过后删除上面的函数， zhangjunfeng
    def highlight(string):
        return '<span class="highlight">%s</span>'%str(string)
    def healthy(string):
        return '<span class="healthy">%s</span>'%str(string)

    try:
        services = bridge_srvc.srvc_list()
        nodes = rest_moni(host=edog_host, port=edog_port)
    except:
        raise
    if nodes == {'result':'empty'}:
        return None
    elif not nodes:
        return None
    #service 和nodes 的格式参考 
    #print 'node_srvc_list, service:%s'%str(services)
    #print 'node_srvc_list, nodes:%s'%str(nodes)

    def get_service_part(node, services, serviceName, serviceShort):
        service_list = services[serviceName]

        n_service_list = []
        for service in service_list:
            if node['ip'] == service['ip']:
                n_service_list.append(service)

        service_running_list = []
        for service in n_service_list:
            if service['status'] == 'running':
                service_running_list.append(service)
        len_service_running = len(service_running_list)
        len_service = len(n_service_list)

        service_part = ''
        if len_service>0:
            if len_service_running != len_service:
                service_part = highlight('%s[%d/%d]'%
                        (serviceShort, len_service_running, len_service))
            else:
                service_part = healthy('%s[%d/%d]'%
                        (serviceShort, len_service_running, len_service))
        return service_part

    service_dict = {'mds':'M', 'c60':'L', 'cds':'C', 'rjnld':'R', 'proxy':'P',
            'nfs':'N', 'cifs':'S', 'ftp':'F', 'http':'H'}
    for n in nodes:
        #######################################################
    #        if not n['cluster'] > -1: continue
        #######################################################
        def l(serviceName):
            return get_service_part(n, services, serviceName, service_dict[serviceName])
        n['srvc'] = '&nbsp;'.join([
                    l('mds'), l('c60'), l('cds'), l('rjnld'),
                    '<br/>',
                    l('proxy'), l('nfs'), l('cifs'), l('ftp'), l('http')])
    return nodes

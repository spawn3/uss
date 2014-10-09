#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101205
Author: zkz
"""
from ua.common.bridge import node as bridge_node
from ua.common.bridge import srvc as bridge_srvc

import web
from web.contrib.template import render_jinja

import json

urls = (
        '/tab_proxy', 'Tab_proxy',
        '/tab_nfs', 'Tab_nfs',
        '/tab_cifs', 'Tab_cifs',
        '/tab_ftp', 'Tab_ftp',
        '/tab_http', 'Tab_http',

        '/proxy_rsrc', 'Proxy_rsrc',
        '/nfs_rsrc', 'Nfs_rsrc',
        '/cifs_rsrc', 'Cifs_rsrc',
        '/ftp_rsrc', 'Ftp_rsrc',
        '/http_rsrc', 'Http_rsrc',

        '/page/(.*)', 'Page',
        )

render = render_jinja('webapps/fsys/templates/conn', encoding='utf-8',)
app = web.application(urls, locals())


def notfound():
    return web.notfound(render.notfound())
app.notfound = notfound

def login_required(func):
    def new_func(*args, **argvkw):
        session = web.ctx.session
        if session.user == 'anonymous' or session.user == '':
#            return render.notlogin()
             raise web.seeother('/login?tip=%s' % u'请登录!')
        else:
            return func(*args, **argvkw)
    return new_func

class Page:
    @login_required
    def GET(self, index=0):
        return render.page(index=index)

class Tab_proxy:
    @login_required
    def GET(self):
        return render.tab_proxy()

class Tab_nfs:
    @login_required
    def GET(self):
        return render.tab_nfs()

class Tab_cifs:
    @login_required
    def GET(self):
        return render.tab_cifs()

class Tab_ftp:
    @login_required
    def GET(self):
        return render.tab_ftp()

class Tab_http:
    @login_required
    def GET(self):
        return render.tab_http()

class Proxy_rsrc:
    @login_required
    def GET(self):
        input = web.input()

        mytype='proxy'
        services = bridge_srvc.srvc_list(type=mytype)
        proxy_list = services[mytype]

        count = 0
        if proxy_list:
            count = len(proxy_list)

        limit = int(input.rows) #每页显示行。
        sord = input.sord #排序关键字
        sidx = input.sidx #升序还是降序
        page = input.page #申请的第几页

        total_pages = count/limit
        if count%limit != 0:
            total_pages += 1

        start = (int(page) - 1) * limit
        end = int(page) * limit
        print start, end
        if end > count: end = count

        rows = []
        if proxy_list:
            for proxy in proxy_list:
                id = proxy['n']
                ip = proxy['ip']
                nodes = bridge_node.node_list(ip=ip)

                node = nodes[0]
                server = node['hostname']
                rows.append({'id':id,
                             'cell':[id, ip, server, '']})

        results = {
            'total'   : str(total_pages),
            'page'    : str(page),
            'records' : str(count),
            'rows'    : rows
            }
        web.header("Content-Type", "application/json")
        return json.dumps(results)

class Nfs_rsrc:
    @login_required
    def GET(self):
        input = web.input()

        mytype='nfs'
        services = bridge_srvc.srvc_list(type=mytype)
        nfs_list = services[mytype]

        count = 0
        if nfs_list:
            count = len(nfs_list)

        limit = int(input.rows) #每页显示行。
        sord = input.sord #排序关键字
        sidx = input.sidx #升序还是降序
        page = input.page #申请的第几页

        total_pages = count/limit
        if count%limit != 0:
            total_pages += 1

        start = (int(page) - 1) * limit
        end = int(page) * limit
        print start, end
        if end > count: end = count

        rows = []
        if nfs_list:
            for nfs in nfs_list:
                id = nfs['n']
                ip = nfs['ip']
                node = bridge_node.node_list(ip=ip)[0]
                server = node['hostname']
                rows.append({'id':id,
                             'cell':[id, ip, server, '']})

        results = {
            'total'   : str(total_pages),
            'page'    : str(page),
            'records' : str(count),
            'rows'    : rows
            }
        web.header("Content-Type", "application/json")
        return json.dumps(results)


class Cifs_rsrc:
    @login_required
    def GET(self):
        input = web.input()

        mytype='cifs'
        services = bridge_srvc.srvc_list(type=mytype)
        cifs_list = services[mytype]

        count = 0
        if cifs_list:
            count = len(cifs_list)

        limit = int(input.rows) #每页显示行。
        sord = input.sord #排序关键字
        sidx = input.sidx #升序还是降序
        page = input.page #申请的第几页

        total_pages = count/limit
        if count%limit != 0:
            total_pages += 1

        start = (int(page) - 1) * limit
        end = int(page) * limit
        print start, end
        if end > count: end = count

        rows = []
        if cifs_list:
            for cifs in cifs_list:
                id = cifs['n']
                ip = cifs['ip']
                node = bridge_node.node_list(ip=ip)[0]
                server = node['hostname']
                rows.append({'id':id,
                             'cell':[id, ip, server, '']})

        results = {
            'total'   : str(total_pages),
            'page'    : str(page),
            'records' : str(count),
            'rows'    : rows
            }
        web.header("Content-Type", "application/json")
        return json.dumps(results)

class Ftp_rsrc:
    @login_required
    def GET(self):
        input = web.input()

        mytype='ftp'
        services = bridge_srvc.srvc_list(type='ftp')
        ftp_list = services['ftp']

        count = 0
        if ftp_list:
            count = len(ftp_list)

        limit = int(input.rows) #每页显示行。
        sord = input.sord #排序关键字
        sidx = input.sidx #升序还是降序
        page = input.page #申请的第几页

        total_pages = count/limit
        if count%limit != 0:
            total_pages += 1

        start = (int(page) - 1) * limit
        end = int(page) * limit
        print start, end
        if end > count: end = count

        rows = []
        if ftp_list:
            for ftp in ftp_list:
                id = ftp['n']
                ip = ftp['ip']
                node = bridge_node.node_list(ip=ip)[0]
                server = node['hostname']
                rows.append({'id':id,
                             'cell':[id, ip, server, '']})

        results = {
            'total'   : str(total_pages),
            'page'    : str(page),
            'records' : str(count),
            'rows'    : rows
            }
        web.header("Content-Type", "application/json")
        return json.dumps(results)

class Http_rsrc:
    @login_required
    def GET(self):
        input = web.input()

        mytype='http'
        services = bridge_srvc.srvc_list(type=mytype)
        http_list = services[mytype]

        count = 0
        if http_list:
            count = len(http_list)

        limit = int(input.rows) #每页显示行。
        sord = input.sord #排序关键字
        sidx = input.sidx #升序还是降序
        page = input.page #申请的第几页

        total_pages = count/limit
        if count%limit != 0:
            total_pages += 1

        start = (int(page) - 1) * limit
        end = int(page) * limit
        print start, end
        if end > count: end = count

        rows = []
        if http_list:
            for http in http_list:
                id = http['n']
                ip = http['ip']
                node = bridge_node.node_list(ip=ip)[0]
                server = node['hostname']
                rows.append({'id':id, 'cell':[id, ip, server, '']})

        results = {
            'total'   : str(total_pages),
            'page'    : str(page),
            'records' : str(count),
            'rows'    : rows
            }
        web.header("Content-Type", "application/json")
        return json.dumps(results)

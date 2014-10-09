#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101205
Author: zkz
"""
import web
from web.contrib.template import render_jinja
from ua.common.api.conf import ConfOpt
from ua.common import utils

import json


urls = (
        '/rsrc', 'Rsrc',
        '/page', 'Page',
        '/hello', 'Hello',
        )

render = render_jinja('webapps/fsys/templates/conf', encoding='utf-8',)
app = web.application(urls, locals())
confOpt = ConfOpt()


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
    def GET(self):
        try:
            cf = confOpt.get_conf()
        except:
            print "------------ get conf error,"
            raise web.internalerror(message = "get conf error,")
        return render.page(cf = cf)
    @login_required
    def POST(self):
        x = web.input()
        y = {}
        for key in x.keys():
            y[key] = x[key]
        print y
        print type(y)
        return "配置已写入!"

class Rsrc:
    @login_required
    def GET(self):
        x = web.input()
        print x
        print 'in rsrc get,conf'
        try:
            cf = confOpt.get_conf()
        except:
            print "------------ get conf error,"
            raise web.internalerror(message = "get conf error,")
        limit = int(x.rows) #每页显示行。
        sord = x.sord #排序关键字
        sidx = x.sidx #升序还是降序
        page = x.page #申请的第几页
        cf = utils.sort_dict_list(cf, sord, sidx)
        count = len(cf)
        total_pages = count/limit
        if count%limit != 0: total_pages += 1
        start = (int(page)-1) * limit
        end = int(page) * limit
        if end > count: end = count
        rows = []
        if count == 0:
            rows={'id':'', 'cell':['', '', '', '']}
        else:
            for i in range(start, end):
                option = cf[i]
                rows.append({'id':option['key'],
                             'cell':[
                                 option['catagory'], option['key'], 
                                 option['value'], option['doc']]})
        results = {
            'total'   : str(total_pages),
            'page'    : str(page),
            'records' : str(count),
            'rows'    : rows
            }
        web.header("Content-Type", "application/json")
        return json.dumps(results)

    @login_required
    def POST(self):
        print 'in rsrc post,conf'
        x = web.input()
        print x
        oper = x.oper
        key = x.id
        value = x.value
        print oper, key, value
     
class Hello:
    @login_required
    def GET(self):
        x = web.input()
        print x
        print 'in hello get,conf'

    @login_required
    def POST(self):
        x = web.input()
        print x
        print 'in  hello post,conf'

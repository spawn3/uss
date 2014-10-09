#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101205
Author: zkz
"""
import web
from web.contrib.template import render_jinja
import re

import json

from  ua.common.api.ylvm import YlvmOpt
from  ua.common.utils import sort_dict_list

urls = (
        '/page', 'Page',
        '/rsrc', 'Rsrc',
        )

ylvmOpt = YlvmOpt()
render = render_jinja('webapps/fsys/templates/flvm', encoding='utf-8',)
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
    def GET(self):
        return render.page()

class Rsrc:
    @login_required
    def GET(self):
        input = web.input()
        ylvmopt = YlvmOpt()
        try:
            lvm = ylvmopt.list()
        except:
            print "-----------  get message error,"
            raise web.internalerror(message = "get message error,")
        limit = int(input.rows) #每页显示行。
        sord = input.sord #排序关键字
        sidx = input.sidx #升序还是降序
        page = input.page #申请的第几页
        count = len(lvm)
        if count == 0:
            rows={'id':'', 'cell':['', '', '']}
        else:
            for i in lvm:
                i['id'] = int(i['id'])
            NS = sort_dict_list(lvm, sord, sidx)
            count = len(NS)
            total_pages = count/limit
            if count%limit != 0:
                total_pages += 1
            start = (int(page)-1) * limit
            end = int(page) * limit
            if end > count: end = count
            rows = []
            for i in range(start, end):
               n = NS[i]
               size = n['size']
               n['size'] = ((int(size)/1024)/1024)/1024
               rows.append({'id':n['name'],
                             'cell':[n['id'], n['name'], n['size']]})
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
        x = web.input()
        print x
        global NS
        if 'oper' in x.keys() and x['oper'] == "add":
            name = x["name"]
            m = re.match("^[0-9a-zA-Z_]*$", name)
            if not m:
                raise web.internalerror(message = "名字格式不对")
            try:
                size = int(x['size'])
            except ValueError:
                raise web.internalerror(message = "名字格式不对")

            ylvmOpt.create(name, size)
        if 'oper' in x.keys() and x['oper'] == "del":
            pass
        if 'oper' in x.keys() and x['oper'] == "edit":
            name = x['name']
            m = re.match("^[0-9a-zA-Z_]*$", name)
            if not m:
                raise web.internalerror(message = "名字格式不对")
            try:
                size = int(x['size'])
            except ValueError:
                raise web.internalerror(message = "名字格式不对")

            ylvmOpt.resize(name, size)
        return x

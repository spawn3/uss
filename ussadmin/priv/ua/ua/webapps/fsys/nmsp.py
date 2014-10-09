#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101205
Author: zkz
"""
import web
from web.contrib.template import render_jinja

import json


urls = (
        '/rsrc', 'Rsrc',
        
        '/page', 'Page',
        )

render = render_jinja('webapps/fsys/templates/nmsp', encoding='utf-8',)
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
        
        count = 7
        
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
        
        ns = [{'id':1, 'namespace':'ns1', 'capacity':500, 'used':250, 'percent':0.50, 'auth':'root'},
              {'id':2, 'namespace':'ns2', 'capacity':500, 'used':100, 'percent':0.20, 'auth':'root'},
              {'id':3, 'namespace':'ns3', 'capacity':500, 'used':120, 'percent':0.24, 'auth':'anyone'},
              {'id':4, 'namespace':'ns4', 'capacity':500, 'used':130, 'percent':0.26, 'auth':'anyone'},]
        
        
        rows = []
        for n in ns:
            rows.append({'id':n['id'], 
                         'cell':[n['id'], n['namespace'], n['capacity'], n['used'], n['percent'], n['auth']]})

        results = {
            'total'   : str(total_pages),
            'page'    : str(page),
            'records' : str(count),
            'rows'    : rows
            }
        web.header("Content-Type", "application/json")
        return json.dumps(results)

#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101205
Author: zkz
"""
import web
from web.contrib.template import render_jinja
from ua.common.ua_conf import db_dir

import json
import datetime
import ConfigParser
import sqlite3
import os


urls = (
        '/rsrc', 'Rsrc',

        '/page', 'Page',
        )

render = render_jinja('webapps/syst/templates/alert', encoding='utf-8',)
app = web.application(urls, locals())


def notfound():
    return web.notfound(render.notfound())
app.notfound = notfound

db = os.path.join(db_dir, "alert.db")

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
        conn = sqlite3.connect(db)
        conn.text_factory = str
        c = conn.cursor()
        result = c.execute("select count(*) from alert_info")
        count = 0
        for i in result:
            (count, ) = i


        limit = int(input.rows) #每页显示行。
        sord = input.sord #排序关键字
        sidx = input.sidx #升序还是降序
        page = input.page #申请的第几页

        total_pages = count/limit
        if count%limit != 0:
            total_pages += 1

        start = (int(page) - 1) * limit

#        end = int(page) * limit
#        print start, end
#        if end > count: end = count

#        start = 1
#        limit = 1
        print input
        result = c.execute("select * from alert_info order by %s %s limit %d,%d "%(sidx, sord, start, limit))
        ns = []
        for i in result:
            message = {}
            message['id'] = i[0]
            message['level'] = i[1]
            message['message'] = i[2]
            message['datetime'] = i[3]
            ns.append(message)
        conn.close()

#        now = datetime.datetime.now()
#        ns = [{'id':0, 'datetime':str(now), 'level':'<span style="color:green;">Info</span>', 'message':'Node1 online', 'quiet':'yes'},
#              {'id':1, 'datetime':str(now), 'level':'<span style="color:green;">Info</span>', 'message':'Node2 online', 'quiet':'yes'},
#              {'id':2, 'datetime':str(now), 'level':'<span style="color:green;">Info</span>', 'message':'Node3 online', 'quiet':'yes'},
#              {'id':3, 'datetime':str(now), 'level':'<span style="color:red;">Crit</span>', 'message':'Node3 has a hardware monitoring failure', 'quiet':'no'},
#              {'id':4, 'datetime':str(now), 'level':'<span style="color:green;">Info</span>', 'message':'Node4 online', 'quiet':'yes'},
#              {'id':5, 'datetime':str(now), 'level':'<span style="color:orange;">Warn</span>', 'message':'Node2 something wrong with the drives', 'quiet':'no'}]
#

        rows = []
        for n in ns:
            level =  n['level']
            if level == "error":
                n['level'] = '<span style="color:red">%s</span>'%(level)
            elif level == "info":
                n['level'] = '<span style="color:green">%s</span>'%(level)
            else:
                n['level'] = '<span style="color:orange">%s</span>'%(level)
                pass
            rows.append({'id':n['id'],
                         'cell':[n['id'], n['datetime'], n['level'], n['message']]})

        results = {
            'total'   : str(total_pages),
            'page'    : str(page),
            'records' : str(count),
            'rows'    : rows
            }
        web.header("Content-Type", "application/json")
        return json.dumps(results)

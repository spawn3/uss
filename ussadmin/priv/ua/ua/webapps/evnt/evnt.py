#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101015
Author: zkz
"""
from ua.common.sqlitedb import LocalSqliteDB
from ua.common.utils import current_dir
from ua.common.ua_conf import *

import web
from web.contrib.template import render_jinja

import os

urls = (
        '/tree', 'Tree',
        '/page', 'Page',
        '/.*', 'Page'
        )

app = web.application(urls, locals())
render = render_jinja('webapps/evnt/templates/', encoding='utf-8',)

#########################################################
#oops_db_dir = os.path.join(os.path.dirname(current_dir()), db_dir)
#oops_sql_dir = os.path.join(os.path.dirname(current_dir()), sql_dir)

#user_db = LocalSqliteDB('user', oops_db_dir, oops_sql_dir)
#db = os.path.join(os.path.dirname(current_dir()), db_dir, 'user.db')
#########################################################

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

class Events:
    @login_required
    def GET(self):
        return render.events()

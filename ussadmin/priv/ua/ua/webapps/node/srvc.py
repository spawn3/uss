#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101205
Author: zkz
"""
from ua.common.sqlitedb import LocalSqliteDB
from ua.common.utils import current_dir
from ua.common.ua_conf import *

import web
from web.contrib.template import render_jinja

import os

urls = (
        '/tab_mds', 'Tab_mds',
        '/tab_cds', 'Tab_cds',
        '/tab_c60', 'Tab_c60',

        '/page', 'Page',
        )

render = render_jinja('webapps/node/templates/srvc', encoding='utf-8',)
app = web.application(urls, locals())

#########################################################
oops_db_dir = os.path.join(os.path.dirname(current_dir()), db_dir)
oops_sql_dir = os.path.join(os.path.dirname(current_dir()), sql_dir)

node_db = LocalSqliteDB('node', oops_db_dir, oops_sql_dir)
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

class Tab_c60:
    @login_required
    def GET(self):
        return render.tab_c60()

class Tab_mds:
    @login_required
    def GET(self):
        return render.tab_mds()

class Tab_cds:
    @login_required
    def GET(self):
        return render.tab_cds()

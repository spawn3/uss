#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101205
Author: zkz
"""
from ua.common.api.node import SystCtrl

import web
from web.contrib.template import render_jinja

urls = (
        '/redeploy', 'Redeploy',
        '/page', 'Page',
        )

render = render_jinja('webapps/stin/templates/update', encoding='utf-8',)
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

class Redeploy:
    @login_required
    def GET(self):
        try:
            sc = SystCtrl()
            rslt = sc.deploy()

            if rslt['result'] == 'ok':
                return 'SUCC'
            else:
                return rslt
        except Exception, e:
            return e

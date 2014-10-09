#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101205
Author: zkz
"""
from ua.common.bridge import moni as bridge_moni

import web
from web.contrib.template import render_jinja

urls = (
        '/page', 'Page',
        )

render = render_jinja('webapps/syst/templates/moni', encoding='utf-8',)
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

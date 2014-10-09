#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101205
Author: zkz
"""
from ua.common.api.node import SystCtrl

import web
from web.contrib.template import render_jinja

import json
import time


urls = (
        '/syst_deploy', 'Syst_deploy',
        '/syst_test', 'Syst_test',
        '/syst_start', 'Syst_start',
        '/syst_stop', 'Syst_stop',
        '/page', 'Page',
        )

render = render_jinja('webapps/syst/templates/ctrl', encoding='utf-8',)
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

class Syst_deploy:
    @login_required
    def GET(self):
        try:
            sc = SystCtrl()
            result = sc.deploy()
            print 'deploy: %s' % result
            """
            {'result': 'ok'}
            """
            if "result" in result.keys():
                return 'SUCC'
            else:
                return result['error']
        except Exception, e:
            print " sc.deploy executete error, "
            return '命令执行错误'

class Syst_test:
    @login_required
    def GET(self):
        rslt = ""
        try:
            sc = SystCtrl()
            rslt = sc.test()
            """
            {'error':{'unavailable-c60', 'no-c60'}}
            {'result':True}
            """
#            return rslt
#            print rslt
#            rslt['error'] = 'error'
#            rslt['result'] = 'error'
            if "result" in rslt.keys():
                return 'SUCC'
            else:
                return rslt['error']
        except Exception, e:
            print " sc.test execute error, "
            return '测试命令执行错误'

class Syst_start:
    @login_required
    def GET(self):
        try:
            sc = SystCtrl()
            rslt = sc.start()
            """
            {'error':{'unavailable-c60', 'no-c60'}}
            {'result':True}
            """
#            return rslt
#            print rslt
#            rslt['error'] = 'error'
            if "result" in rslt.keys():
                return 'SUCC'
            else:
                return rslt['error']
        except Exception, e:
            print " sc.test execute error, "
            return '测试命令执行错误'


class Syst_stop:
    @login_required
    def GET(self):
        try:
            sc = SystCtrl()
            rslt = sc.stop()
            """
            {'error':{'unavailable-c60', 'no-c60'}}
            {'result':True}
            """
#            return rslt
#            print rslt
#            rslt['error'] = 'error'
            if "result" in rslt.keys():
                return 'SUCC'
            else:
                return rslt['error']
        except Exception, e:
            print " sc.test execute error, "
            return '测试命令执行错误'

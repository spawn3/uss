#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101202
Author: zkz
"""
import alert
import moni
import ctrl

import web
from web.contrib.template import render_jinja

urls = (
        '/alert', alert.app,
        '/moni', moni.app,
        '/ctrl', ctrl.app,
        )

render = render_jinja('webapps/syst/templates', encoding='utf-8',)
app = web.application(urls, locals())

def notfound():
    return web.notfound(render.notfound())
app.notfound = notfound

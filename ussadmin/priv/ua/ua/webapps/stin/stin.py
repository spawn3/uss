#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101202
Author: zkz
"""
import update

import web
from web.contrib.template import render_jinja

urls = (
        '/update', update.app
        )

render = render_jinja('webapps/stin/templates', encoding='utf-8',)
app = web.application(urls, locals())

def notfound():
    return web.notfound(render.notfound())
app.notfound = notfound
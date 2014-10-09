#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101205
Author: zkz
"""
import srvc

import web
from web.contrib.template import render_jinja

urls = (
        '/srvc', srvc.app,
        )

render = render_jinja('webapps/ear/templates', encoding='utf-8',)
app = web.application(urls, locals())


def notfound():
    return web.notfound(render.notfound())
app.notfound = notfound
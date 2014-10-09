#!/usr/bin/python
#-*- coding: utf-8 -*-

import conn
import nmsp
import fshr
import flvm
import auth
import conf
import flog

print dir(flog)
print flog.__file__

import web
from web.contrib.template import render_jinja

urls = (
        '/conn', conn.app,
        '/nmsp', nmsp.app,
        '/fshr', fshr.app,
        '/auth', auth.app,
        '/flvm', flvm.app,
        '/flog', flog.app,
        '/conf', conf.app,
        )

render = render_jinja('webapps/fsys/templates', encoding='utf-8',)
app = web.application(urls, locals())

def notfound():
    return web.notfound(render.notfound())
app.notfound = notfound

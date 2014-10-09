#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101101
"""

import os
import sys
import hashlib
import time

realpath = os.path.dirname(os.path.realpath( __file__))
working_directory = os.path.join(realpath, '..')
sys.path.insert(0, working_directory)
os.chdir(working_directory)
print '************************************************'
print sys.path
print 'working directory: %s' % working_directory
print '************************************************'

from ua.common.rrdtool.server import ThreadRrdtool
from ua.common.rrdtool.server import ThreadRrdtoolGrapher
from ua.common.rrdtool.server import ThreadRrdtoolUpdater
from ua.common.sqlitedb import LocalSqliteDB
from ua.common.utils import current_dir, mkdirs
from ua.webapps.ear import ear
from ua.webapps.user import user
#from webapps.auth import auth
from ua.webapps.syst import syst
from ua.webapps.node import node
from ua.webapps.fsys import fsys
from ua.webapps.stin import stin
#from webapps.conf import conf
from ua.webapps.div import div
from ua.common.ua_conf import *

import web
from web.contrib.template import render_jinja
from common.wsgilog import WsgiLog

os.chdir(ua_path)
urls = (
        '/div', div.app,
        '/ear', ear.app,
        '/syst', syst.app,
        '/node', node.app,
        '/fsys', fsys.app,
        '/stin', stin.app,
#        '/conf', conf.app,
#        '/auth', auth.app,
        '/user', user.app,

        '/login', 'Login',
        '/logout', 'Logout',
        '/home', 'Home',
        '/', 'Home'
        )

app = web.application(urls, locals())
web.config.debug = False
render = render_jinja('webapps/glob/templates/', encoding='utf-8',)

user_db = LocalSqliteDB('user', db_dir, sql_dir)
session_db = LocalSqliteDB('session', db_dir, sql_dir)
if web.config.get('_session') is None:
    session = web.session.Session(app, web.session.DBStore(session_db, 'session'), initializer={'user':''})
    web.config._session = session
else:
    session = web.config._session

def session_hook():
    web.ctx.session = session

app.add_processor(web.loadhook(session_hook))

def login_required(func):
    def new_func(*args, **argvkw):
        if session.user == 'anonymous' or session.user == '':
             raise web.seeother('/login?tip=%s' % u'请登录!')
        else:
            return func(*args, **argvkw)
    return new_func

class Home:
    @login_required
    def GET(self):
        session = web.ctx.session
        return render.home(user=session.user)

class Login:
    def GET(self):
        input = web.input(tip='')
        return render.login(tip=input.tip)

    def POST(self):
        input = web.input()
        pwhash = hashlib.md5(input.password).hexdigest()
        passwd_hash = ""
        with open(ua_password_file) as f:
            passwd_hash = f.read()
        if pwhash == passwd_hash:
            session.user = "root"
            web.seeother('/home')
        else:
            return web.seeother('/login?tip=%s' % u'密码错误，请重试!')

class Logout:
    @login_required
    def GET(self):
        session.kill()
        raise web.seeother('/login?tip=%s' % u'已登出!')

class Log(WsgiLog):
    '''对wsgi进行了定制，把TimeRotatingFileHandler 换成了RotatingFileHandler '''
    def __init__(self, application):
        WsgiLog.__init__(
                self,
                application,
                logformat = '%(asctime)s-%(message)s',
                tofile = True,
                toprint = True,
                file = ua_main_log_file,
                )


if __name__ == "__main__":
    mkdirs(os.path.dirname(ua_main_log_file))
    with open(ua_main_log_file, 'a') as f:
        print "ua_path", ua_path
        print os.getcwd()
        if not ua_path == os.getcwd():
            f.write("your pwd is not %s, please cd %s \n"%(ua_path, ua_path))
            sys.exit()
        print 'edog_host:%s'%(edog_host)
        print 'edog_port:%s'%(edog_port)
        f.write('edog_host:%s \n'%(edog_host))
        f.write('edog_port:%s \n'%(edog_port))
        if not os.path.isdir(os.path.dirname(ua_main_pid_file)):
            os.makedirs(os.path.dirname(ua_main_pid_file))
        if os.path.isfile(ua_main_pid_file):
            with open(ua_main_pid_file, "r") as f:
                old_pid = f.read().strip()
                if old_pid:
                    cmdline_path = os.path.join("/proc/", old_pid, "cmdline")
                    if os.path.isfile(cmdline_path):
                        with open(cmdline_path, 'r') as cmdline_f:
                            cmdline_list = (cmdline_f.read().strip()).split("\x00")
                            if "python" in cmdline_list and "ua_main.py" in cmdline_list:
                                print "ua is running, please stopua it first"
                                f.write("ua is running, please stopua it first \n")
                                sys.exit(1)
                else:
                    pass
        pid = str(os.getpid())
        with open(ua_main_pid_file, "w") as ff:
            ff.write(pid)
    updater_thread = ThreadRrdtoolUpdater()
    updater_thread.start()
    grapher_thread = ThreadRrdtoolGrapher()
    grapher_thread.start()
    app.run(Log)

#!/usr/bin/python
#-*- coding:utf-8 -*-
"""
Created on 20101205
Author: zkz
"""
from ua.common.utils import mkdirs, current_dir

import web
from web.contrib.template import render_jinja
from ua.common.utils import get_log
from ua.common.sqlitedb import LocalSqliteDB
from ua.common.ua_conf import *

import os
import hashlib
import json
import time
import sqlite3
import commands
import logging
import logging.handlers

urls = (
        '/login', 'Login',
        '/stat', 'Stat',
        '/async_events', 'Async_events',
        )

render = render_jinja('webapps/ear/templates', encoding='utf-8',)
app = web.application(urls, locals())

if not os.path.isdir(os.path.dirname(ear_log_path)):
    os.makedirs(os.path.dirname(ear_log_path))
ear_log = get_log("ear", ear_log_path)

alert_db = LocalSqliteDB('alert', db_dir, sql_dir)
alert_db = os.path.join(db_dir, 'alert.db')

ASYNC_EVENTS_LOG_PATH = os.path.join(ua_data_path, 'store/log/async_events.log')
async_events_log_dir = os.path.dirname(ASYNC_EVENTS_LOG_PATH)

if not os.path.exists(async_events_log_dir):
    mkdirs(async_events_log_dir)

def get_async_log():
    my_logger = logging.getLogger("async_log")
    my_logger.setLevel(logging.DEBUG)
    i = 10 #单位是一兆。
    handler = logging.handlers.RotatingFileHandler(
            ASYNC_EVENTS_LOG_PATH, maxBytes = 1024000*i, backupCount = 5
        )
    handler.setLevel(logging.DEBUG)
    formatter = logging.Formatter("%(message)s")
    handler.setFormatter(formatter)
    my_logger.addHandler(handler)
    return my_logger
async_log = get_async_log()


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

class Login:
    def GET(self):
        return ''

class Stat:
    def GET(self):
        print 'xxxxxx'

    def POST(self):
        """
        {"ip":"192.168.2.16","type":"mds","n":1,
        "event":"need_stop","prev_status":"running",
        "status":"running","info":21034}
        """
        input = web.data()
        x = web.input()

        highlight = '<div class = "div_right_highlight div_right div_right_alert"><span>%s</span></div>'
        healthy   = '<div class = "div_right_healthy div_right div_right_alert"><span>%s</span></div>'
        lowlight  = '<div class = "div_right_lowlight div_right div_right_alert"><span>%s</span></div>'

        ISOTIMEFORMAT='%Y-%m-%d %X'
        t = time.strftime( ISOTIMEFORMAT, time.localtime() )

        try:
            event_dict = json.loads(input)
        except Exception:
            ear_log.error("接收数据格式错误，不是预期的json")
            raise web.internalerror(message =u"传输数据格式错，不是预期的json")
        ear_log.info(str(event_dict))

        catagory = ''
        try:
            catagory = event_dict['catagory']
        except:
            msg_log = u"post error, 提交了不可预期的数据, 没有catagory这个关键字"
            ear_log.error(msg_log)

        def insert_br(info, n = 20):
            """info 是一个字符串，每n个字符串插入一个</br> """
            count = 0
            info = str(info)
            info_br = ""
            for i in info:
                count += 1
                info_br += i
                if count == n:
                    info_br += "<br/>"
                    count = 0
            return info_br

        catagory_list = ['alert', 'user']
        if catagory in catagory_list:
            info     = event_dict['info']
            event    = event_dict['event']
            level    = event_dict['level']
            etime    = event_dict['time']

        msg_log = []
        if catagory == "alert":
            #写入到数据库
            conn = sqlite3.connect(alert_db)
            conn.text_factory = str
            c = conn.cursor()
            c.execute("insert into \
                    alert_info(level, info, etime, event, catagory) \
                    values (?, ?, ?, ?, ?)", \
                    (level, info , str(etime), event, catagory))
            conn.commit()
            conn.close()

        if catagory in catagory_list:
            # write into log file
            msg_log = u'<br/>事件:%s <br/> 级别:%s<br/>内容:%s' % (event, level, insert_br(info))
            if level.strip() == "info":
                msg_log = healthy % (t + msg_log)
            elif level.strip() == "error":
                msg_log = highlight % (t + msg_log)
            else:
                msg_log = lowlight % (t + msg_log)
        else:
            msg_log = highlight%(t + u"<br/>程序暂没有对catagory的value值进行处理, 请联系开发人员.")

        async_log.info("%s" % msg_log.encode('utf-8'))
        return 'Thanks for post,'

class Async_events:
    @login_required
    def GET(self):
        x = web.input()
        tmp_file = ASYNC_EVENTS_LOG_PATH
        lines_num = 6

        cmd = "tail -n %d %s" % (lines_num, tmp_file)
        status, output = commands.getstatusoutput(cmd)
        if status == 0:
            reverse_lines = output.split(os.linesep)
            reverse_lines.reverse()
        else:
            reverse_lines = []
        msg = ''
        for x in reverse_lines:
            msg += x
        return msg

    @login_required
    def POST(self):
        x = web.input()
        if 'hide' in x.keys():
            self.flag_show = False
        if 'show' in x.keys():
            self.flag_show = True




"""
        返回数据样式，做参考
        新:
        1,

        {u'info': u'192.168.1.201 leaved', u'time': u'1302860129',
        u'catagory': u'alert', u'event': u'agent_leave', u'level': u'warn'}
        旧:
        2,
        {u'status': u'stopped', u'info': [], u'prev_status':
        u'running', u'ip': u'192.168.1.201', u'n': 0,
        u'cluster': u'undefined', u'et': u'service',
        u'type': u'c60', u'event': u'yfs_stop'}
"""






"""
        if et == 'service':
            try:
                ip = event_dict['ip']
                type = event_dict['type']
                event = event_dict['event']
                cluster = event_dict['cluster']
                prev_stat = event_dict['prev_status']
                stat = event_dict['status']
                info = event_dict['info']

                msg_log = t + 'Node:%s, Service:%s, Event:%s, Status:%s --> %s\n' % (ip, type, event, prev_stat, stat)

                if (prev_stat == stat) or \
                   (prev_stat == 'running' and stat != 'running') or \
                   (prev_stat == 'starting' and stat == 'shutoff'):
                    msg_tmp = highlight % (t + 'Node:%s, <br/>Service:%s, Event:%s, <br/>Status:%s --> %s' % (ip, type, event, prev_stat, stat))
                else:
                    msg_tmp = lowlight % (t + 'Node:%s, <br/>Service:%s, Event:%s, <br/>Status:%s --> %s' % (ip, type, event, prev_stat, stat))
            except:
                return 'FAIL'
        elif et == 'node':
            try:
                ip = event_dict['ip']
                op = event_dict['op']
                result = event_dict['result']
                info = event_dict['info']

                msg_log = t + 'Node:%s, Operation:%s, Result:%s, Info:%s\n' % (ip, op, result, info)

                if True:
                    msg_tmp = highlight % (t + '<br/>Node:%s, Operation:%s,<br/>Result:%s, Info:%s' % (ip, op, result, info))
                else:
                    msg_tmp = lowlight % (t + '<br/>Node:%s, Operation:%s,<br/>Result:%s, Info:%s' % (ip, op, result, info))
            except:
                return 'FAIL'
        elif et == 'cluster':
            pass

"""

#!/usr/bin/python
#-*- coding: utf-8 -*-

from ua.common.bridge import srvc as bridge_srvc
from ua.common.bridge import node as bridge_node
from ua.common.bridge import moni as bridge_moni
from ua.common.api.node import RackOpt, NodeOpt, SystCtrl
from ua.common.api.ylog import YlogOpt

from ua.common.sqlitedb import LocalSqliteDB
from ua.common.utils import current_dir
from ua.common.yfs import get_service_list
from ua.common.ua_conf import *

import web
from web.contrib.template import render_jinja

import os
import json
import time

rackOpt = RackOpt()
nodeOpt = NodeOpt()
systCtrl = SystCtrl()
ylogOpt = YlogOpt()

#for test
LOGINFO = """


    loginfo, loginfo, loginfo, loginfo, loginfo
    loginfo, loginfo, loginfo, loginfo, loginfo
    loginfo, loginfo, loginfo, loginfo, loginfo
    loginfo, loginfo, loginfo, loginfo, loginfo
    loginfo, loginfo, loginfo, loginfo, loginfo
    loginfo, loginfo, loginfo, loginfo, loginfo
    """
urls = (
        '/tab_ctrl_mng', 'Tab_ctrl_mng', #used 单个服务
        '/field_ctrl', 'Field_ctrl', #used 单个服务
        '/tab_rrd', 'Tab_rrd', #used 整个集群

        '/srvc/(.*)', 'Srvc', # used 单个服务

        '/page/(.*)', 'Page',
        '/logOption', 'LogOption', #used
        '/loginfo', 'LogInfo', #used
        '/srvc_dtls', 'Srvc_dtls',
        )

render = render_jinja('webapps/fsys/templates/flog', encoding='utf-8',)
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
    def GET(self, index=0):
        racks=node_db.select('rack')
        print ""
        return render.page(racks=racks, index=index)


class Tab_rrd:
    @login_required
    def GET(self):
        return render.tab_rrd(racks=rackOpt.select())

class LogInfo:
    @login_required
    def GET(self):
        global LOGINFO
        x = web.input()
#        print x,
        return "here is loginfo get, welcome"
    @login_required
    def POST(self):
        global LOGINFO
        global ylogOpt
        input = web.input()
        x = web.input()
#        print "in loginfo, post.", x
#        print x, x.attr
        if 'sid' in x.keys():
            level = x.attr
            ip = (x.sid).split(":")[0]
            stype = (x.sid).split(":")[1]
            n = (x.sid).split(":")[2]
            loginfo = ylogOpt.get_log(ip, stype, int(n), level, lines = 20)
            loginfo_html = "<br>"
            for line in loginfo:
                loginfo_html = loginfo_html + str(line) + "<br>"
            loginfo_html = "<p><font size=2.5>" + loginfo_html + "</font></p>"
            return loginfo_html
        return x.attr , LOGINFO, time.time()

class LogOption:
    @login_required
    def GET(self):
        return render.logOption()
    @login_required
    def POST(self):
        global ylogOpt
        x = web.input()
        if "option" in x.keys():
            try:
                if x["option"] == "clean_all":
                    rest = ylogOpt.clean_log_all()
                    option_rest = u"日志清除成功"
                elif x["option"] == "backup_all":
                    rest = ylogOpt.dump_log_all()
                    option_rest = u"日志备份成功"
                else:
                    return "error: " + u"暂不支持该操作"
            except Exception, e:
                return "error: %s"%str(e)
            if "result" in rest and rest["result"] == "ok":
                return option_rest
            else:
                return u"操作失败：" + str(rest)
        else:
            return u"没有收到预期的option关键字"


class Tab_ctrl_mng:
    @login_required
    def GET(self):
        return render.tab_ctrl_mng()

class Field_ctrl:
    @login_required
    def GET(self):
        return render.field_ctrl(racks=rackOpt.select(), services=get_service_list())

class Srvc:
    @login_required
    def GET(self, type="all"):
        input = web.input()

        limit = int(input.rows) #每页显示行。
        sord = input.sord #排序关键字
        sidx = input.sidx #升序还是降序
        page = input.page #申请的第几页

        try:
            services = bridge_srvc.srvc_list()
        except:
            raise

        ss = []
        if type == 'all':
            ss = services['c60'] + services['mds'] + services['cds']
        elif type in ['c60', 'mds', 'cds']:
            ss = services[type]
        else:
            ss = []

        if ss:
            count = len(ss)#记录总数

            total_pages = count/limit
            if count%limit != 0:
                total_pages += 1

            start = (int(page) - 1) * limit
            end = int(page) * limit
            if end > count: end = count

            #现在还不支持limit
            rows = []
            for i in range(start, end):
                s      = ss[i]
                id     = s['n']
                ip     = s['ip']
                type   = s['type']
                status = s['status']
                pid    = s['pid']
                rows.append({'id':ip, 'cell':[ip, type, id, status, pid]})

            results = {
                'total'   : str(total_pages),
                'page'    : str(page),
                'records' : str(count),
                'rows'    : rows
                }
        else:
             results = {
                'total'   : 0,
                'page'    : str(page),
                'records' : 0,
                'rows'    : []
                }
        web.header("Content-Type", "application/json")
        return json.dumps(results)

class Srvc_dtls:
    @login_required
    def GET(self):
        x = web.input()
#        print x
        return render.srvc_dtls(sid="sid") #service id = sid
    @login_required
    def POST(self):
        x = web.input()
        return render.srvc_dtls(sid=x['sid']) #service id = sid

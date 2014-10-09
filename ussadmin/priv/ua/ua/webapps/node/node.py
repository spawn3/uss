#!/usr/bin/python
#-*- coding: utf-8 -*-

from ua.common.bridge import srvc as bridge_srvc
from ua.common.bridge import node as bridge_node
from ua.common.bridge import moni as bridge_moni
from ua.common.api.node import RackOpt, NodeOpt, SystCtrl
from ua.common.api.srvc import SrvcOpt

from ua.common.sqlitedb import LocalSqliteDB
from ua.common.utils import current_dir
from ua.common.utils import deployKey
from ua.common.utils import sort_dict_list
from ua.common.yfs import get_service_list
from ua.common.ua_conf import *

import os
import json
import time
import sys
import web
from web.contrib.template import render_jinja
reload(sys)
sys.setdefaultencoding('utf-8')

rackOpt = RackOpt()
nodeOpt = NodeOpt()
systCtrl = SystCtrl()
srvcOpt = SrvcOpt()

urls = (
        '/tab_ctrl_mng', 'Tab_ctrl_mng',
        '/field_ctrl', 'Field_ctrl',
        '/field_mng', 'Field_mng',
        '/tab_srvc', 'Tab_srvc',
        '/tab_rrd', 'Tab_rrd',
        '/tab_newnode', 'Tab_newnode',

        '/rsrc/(.*)', 'Rsrc',
        '/srvc/(.*)', 'Srvc',
        '/srvc_dtls', 'SrvcDtls',

        '/rack_add', 'Rack_add',
        '/rack_edit', 'Rack_edit',
        '/rack_del', 'Rack_del',

        '/node_add', 'Node_add',
        '/node_del', 'Node_del',

        '/node_join', 'Node_join',
        '/node_exit', 'Node_exit',

        '/set_mds_c60', 'Set_mds_c60',

        '/page/(.*)', 'Page',
        )

render = render_jinja('webapps/node/templates', encoding='utf-8',)
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
        d = web.input(type="all")
        return render.page(racks=racks, index=index, type=d.type)

class Tab_ctrl_mng:
    @login_required
    def GET(self):
        return render.tab_ctrl_mng()

class Field_ctrl:
    @login_required
    def GET(self):
        return render.field_ctrl(racks=rackOpt.select())

class Field_mng:
    @login_required
    def GET(self):
        return render.field_mng(racks=rackOpt.select())

class Tab_srvc:
    @login_required
    def GET(self):
        d = web.input(type="all")
        return render.tab_srvc(racks=rackOpt.select(), type=d.type, services=get_service_list())

class Tab_rrd:
    @login_required
    def GET(self):
        return render.tab_rrd(racks=rackOpt.select())

class Tab_newnode:
    @login_required
    def GET(self):
        return render.tab_newnode()

class Rsrc:
    @login_required
    def GET(self, status="joined"):
        input = web.input()

        nm = []
        try:
            nm = bridge_node.node_srvc_list()
        except Exception, err:
            print "----------- %s, get message error: %s"%(edog_host, str(err))
            raise web.internalerror(message = "%s, get message error"%(edog_host))
            pass

        count = 0
        if nm:
            count = len(nm)

        limit = int(input.rows) #每页显示行。
        sord = input.sord #升序还是降序
        sidx = input.sidx #排序关键字
        page = input.page #申请的第几页

        total_pages = count/limit
        if count%limit != 0:
            total_pages += 1

        start = (int(page) - 1) * limit
        end = int(page) * limit
        if end > count: end = count

        rows = []
        nm_new = []
        for n in nm:
            j = {}
            j["id"] = n["id"]
            j["ip"] = n["ip"]
            j["hostname"] = n["hostname"]
            j["status"] = n["status"]
            j["cpu"] = round(n['load']['cpu_used'], 2) if n['load']['cpu_used'] else ''
            j["load"] = round(n['load']['avg5'], 2) if n['load']['avg5'] else ''
            j["mem"] = round(float(n['mem']['total']-n['mem']['free'])/float(n['mem']['total']), 2) \
                                            if n['mem']['free'] and n['mem']['total'] \
                                            else ''
            j["swap"] = round(float(n['swap']['total']-n['swap']['free'])/float(n['swap']['total']), 2) \
                                            if n['swap']['free'] and n['swap']['total'] \
                                            else ''
            nw_in = 0
            nw_out = 0
            try:
                nw_in = round(float(n['nw']['in'])/(1024*1024), 2)
                nw_out = round(float(n['nw']['out'])/(1024*1024), 2)
            except:
                nw_in = ''
                nw_out = ''
            j["nw"] = str(nw_in) + ", " + str(nw_out)
            j["drvs"] = round(float(n['disk']['used'])/float(n['disk']['total']), 3) \
                                            if n['disk']['used'] and n['disk']['total'] \
                                            else ''
            j["srvc"] = n["srvc"]
            j["cluster"] = n["cluster"]
            nm_new.append(j)

        nm = sort_dict_list(nm_new, sord, sidx)
        if nm:
            for i in range(start, end):
                n = nm[i]
            #######################################################
                try:
                    if status == 'joined':
                        if n['cluster'] == -1 or n['cluster'] == None:
                            continue
                    elif status == 'exited':
                        if n['cluster'] > -1:
                            continue
                except:
                    continue
            #######################################################

                cpu_used = n["cpu"]
                load_avg5 = n["load"]
                mem_used_percent = n["mem"]
                swap_used_percent = n["swap"]
                disk_used_percent = n["drvs"]
                nw_in_out = n["nw"]

                ###########################################################################
                opt_edit = '<button onclick="node_edit(%d);">编辑</button>' % n['id']
                opt_del = '<button onclick="node_del(%d);">删除</button>' % n['id']

                opt_join = '<button onclick="node_join(%d);">加入</button>' % n['id']
                opt_exit = '<button onclick="node_exit(%d);">脱离</button>' % n['id']


                if status == 'joined':
                    operations = opt_exit
                elif status == 'exited':
                    operations = opt_del + opt_join
                else:
                    continue
                ###########################################################################

                rows.append({'id':n['id'],
                             'cell':[n['id'], n['ip'], n['hostname'],
                                     n['status'],
                                     cpu_used,
                                     load_avg5,
                                     mem_used_percent,
                                     swap_used_percent,
                                     nw_in_out,
                                     disk_used_percent,
                                     n['srvc'],
                                     operations
                                     ]})

        results = {
            'total'   : str(total_pages),
            'page'    : str(page),
            'records' : str(count),
            'rows'    : rows
            }
        web.header("Content-Type", "application/json")
        return json.dumps(results)

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
            print "--------  %s, get message error"%(edog_host)
            raise web.internalerror(message = "%s get message error,"%(edog_host))

        ss = []
        if type == 'all':
            for k, v in services.iteritems():
                ss += v
        elif type in get_service_list():
            ss = services[type]
        elif type == 'all_log':
            #因为在日志的操作中也调用的这个方法，
            #和node不同的地方是all，所以添加了这个else判断。
            for k, v in services.iteritems():
                ss += v
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
            ss = sort_dict_list(ss, sord, sidx)
            rows = []
            if ss:
                for i in range(start, end):
                    s = ss[i]
                    n = s['n']
                    ip = s['ip']
                    type = s['type']
                    status = s['status']
                    pid = s['pid']
                    id = ":".join([str(ip), type, str(n)])
                    argv = "'"+id+"'"
                    opt_start = '<button onclick="srvc_opt(%s, %s);">启动</button>'%(argv, "'"+'start'+"'") 
                    opt_stop = '<button onclick="srvc_opt(%s, %s);">关闭</button>'%(argv, "'"+'stop'+"'")
                    if status == 'running':
                        opt = opt_stop
                    else:
                        opt = opt_start
                    rows.append({'id':id,
                                 'cell':[ip, type, n, status, pid, opt]})

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

    @login_required
    def POST(self, argv):
        x = web.input()
        print 'srvc post,', x
        opt = x.opt
        ip = (x.id).split(':')[0]
        service = (x.id).split(':')[1]
        serviceId = (x.id).split(':')[2]
        print opt, ip, service, serviceId
        if opt == 'start':
            resp = srvcOpt.start(ip=ip, service=service, serviceId=serviceId)
        elif opt == 'stop':
            resp = srvcOpt.stop(ip=ip, service=service, serviceId=serviceId)
        else:
            return 'NOT SUPPORT'
        print resp
        if 'result' in resp.keys():
            return 'SUCC'
        else:
            return 'error:'+resp['error']

class Rack_add:
    @login_required
    def POST(self):
        input = web.input()
        name = input.name

        try:
            rack_id = rackOpt.insert(name)
        except:
            return 'Error: Add rack failed!'
        else:
            return 'SUCC'

class Rack_edit:
    @login_required
    def POST(self):
        input = web.input()

        what = {"name": input.name}
        where = {"id": int(input.id)}

        try:
            rack_id = rackOpt.update(what=what, where=where)
        except:
            return 'Error: Add rack failed!'
        else:
            return 'SUCC'

class Rack_del:
    @login_required
    def POST(self):
        input = web.input()
        id = int(input.id)

        where = {"id":id}

        try:
            rslt = rackOpt.delete(where=where)
            if rslt['id'] == id:
                return 'SUCC'
            else:
                return rslt
        except Exception, e:
            return 'Error: %s' % e


class Node_add:
    @login_required
    def POST(self):
        input = web.input()
        for i in (input.ip).split("."):
            if int(i) >= 255:
                e = u"IP 地址不能超过255"
                return 'Error: %s' % e
        what = {"ip":input.ip,
                "hostname":input.hostname,
                "user":input.user,
                "passwd":input.passwd,
                "rack":int(input.rack) }
#        ip = (input.ip).strip()
#        user = (input.user).strip()
#        passwd = (input.passwd).strip()
#        if not deployKey(ip, user, passwd, genkey="genkey.sh"):
#            return 'Error: deployKey error.'
        try:
            rsp = nodeOpt.insert(what=what)
            if 'error' in rsp.keys():
                return "error: " + str(rsp["error"])
            return 'SUCC'
        except Exception, e:
            return 'Error: %s' % e

class Node_del:
    @login_required
    def POST(self):
        input = web.input()
        id = int(input.id)

        where = {"id":id}

        #print '++++++++++++++++++++++++++++++++++++++++++++'
        #print where
        #print '++++++++++++++++++++++++++++++++++++++++++++'

        try:
            rslt = nodeOpt.delete(where=where)

            if rslt['id'] == id:
                return 'SUCC'
            else:
                return rslt
        except Exception, e:
            return 'Error: %s' % e

class Node_join:
    @login_required
    def POST(self):
        input = web.input()
        id = int(input.id)

        try:
            rslt = systCtrl.node_join(id=id)

            if rslt['id'] == id:
                return 'SUCC'
            else:
                return rslt
        except Exception, e:
            return 'Error: %s' % e

class Node_exit:
    @login_required
    def POST(self):
        input = web.input()
        id = int(input.id)

        try:
            rslt = systCtrl.node_exit(id=id)

            if rslt['id'] == id:
                return 'SUCC'
            else:
                return rslt
        except Exception, e:
            return 'Error: %s' % e

class Set_mds_c60:
    @login_required
    def POST(self):
        input = web.input()

        def str_to_list(str):
            return [int(i) for i in str.split(',') if i]

        #print input
        mds_list = str_to_list(input.mds_list)
        c60_list = str_to_list(input.c60_list)

        mds_list = sorted(set(mds_list),key=mds_list.index)
        c60_list = sorted(set(c60_list),key=c60_list.index)


        #print '***********************************************'
        #print mds_list
        #print c60_list
        #print '***********************************************'

        try:
            rslt = systCtrl.set_mds_c60(mds_list=mds_list, c60_list=c60_list)

            if 'result' in rslt.keys():
                return 'SUCC'
            else:
                return rslt['error']
        except Exception, e:
            print "设置c60, mds　错误"
            return 'Error: %s' % e

class SrvcDtls:
    @login_required
    def GET(self):
        x = web.input()
#        print x
        return render.srvc_dtls(sid="sid") #service id = sid
    @login_required
    def POST(self):
        x = web.input()
        return render.srvc_dtls(sid=x['sid']) #service id = sid

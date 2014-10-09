#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20110127
Author: zkz
"""
from ua.common.utils import mkdirs, current_dir

from ua.common.rrdtool import grapher

from ua.common.bridge import srvc as bridge_srvc
from ua.common.bridge import node as bridge_node
from ua.common.bridge import moni as bridge_moni

from ua.common.api.node import RackOpt, NodeOpt, ClusterOpt
from ua.common.ua_conf import ua_conf, edog_host

rackOpt = RackOpt()
nodeOpt = NodeOpt()
clusterOpt = ClusterOpt()

import os
import web
from web.contrib.template import render_jinja

urls = (
        '/login', 'Login',
        '/glob_stat', 'Glob_stat',
        '/glob_disk', 'Glob_disk',
        '/glob_rrd', 'Glob_rrd',

        '/rack_mng', 'Rack_mng',
        '/rack_list', 'Rack_list',

        '/node_role_set', 'Node_role_set',
        '/node_role_list', 'Node_role_list',
        '/node_dtls', 'Node_dtls',
        '/node_add', 'Node_add',
        '/node_rrd', 'Node_rrd',
        )

render = render_jinja('webapps/div/templates', encoding='utf-8',)
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

class Login:
    def GET(self):
        return ''

class Glob_stat:
    @login_required
    def GET(self):
        highlight = '<span class="highlight">%s</span>'
        healthy   = '<span class="healthy">%s</span>'
        lowlight  = '<span class="lowlight">%s</span>'

        def get_status(prefix, serv_list):
            running_list = [x for x in serv_list if x['status'] == 'running']
            stopped_list = [x for x in serv_list if x['status'] == 'shutoff']

            total   = len(serv_list)
            running = len(running_list)
            stopped = len(stopped_list)
            delta   = total - running - stopped

            result = '%s[%d/%d]' % (prefix, running, total)
            if total>0 and running == total:
                result = '<a type=%s>%s</a>' % (prefix.lower(), healthy % result)
            elif total>0 and running != total:
                if delta == 0:
                    result = '<a type=%s>%s</a>' % (prefix.lower(), highlight % result)
                else:
                    result = '<a type=%s>%s</a>' % (prefix.lower(), lowlight % result)
            else:
                result = ''
            return result

        def analyse_srvc():
            ########################################################################
            serv_status = ' '.join([
                    get_status('C60',   services['c60']),
                    get_status('MDS',   services['mds']),
                    get_status('RJNLD', services['rjnld']),
                    get_status('CDS',   services['cds'])
                ])
            fs_status = ' '.join([
                get_status('NFS',   services['nfs']),
                get_status('Proxy', services['proxy']),
                get_status('CIFS',  services['cifs']),
                get_status('FTP',   services['ftp']),
                get_status('HTTP',  services['http'])
                ])

            return serv_status, fs_status

        ########################################################################
        try:
            services = bridge_srvc.srvc_list()
            if services:
                serv_status, fs_status = analyse_srvc()
            else:
                serv_status = ''
                fs_status   = ''
        except Exception, e:
            print e
            print "--------- %s, get message error, srvc_list" % (edog_host)
            raise web.internalerror("%s, get message error" % (edog_host))

        try:
            rslt = bridge_node.node_stat_list()
            if rslt:
                t = bridge_node.node_stat_list()
                l_tmp = []
                for l in t:
                    l_new = []
                    for e in l:
                        if e['cluster']>-1:
                            l_new.append(e)
                    l_tmp.append(l_new)

                t = tuple(l_tmp)

                running_list, unknown_list, starting_list, \
                stopping_list, sleeping_list = t

                node_status = ' '.join([
                    '<a>%s</a>' % (healthy % (u'运行中[%d]' % len(running_list))) if len(running_list)>0 else '',
                    '<a>%s</a>' % (highlight % (u'无效[%d]' % len(unknown_list))) if len(unknown_list)>0 else '',
                    '<a>%s</a>' % (healthy % (u'启动中[%d]' % len(starting_list))) if len(starting_list)>0 else '',
                    '<a>%s</a>' % (healthy % (u'停止中[%d]' % len(stopping_list))) if len(stopping_list)>0 else '',
                    '<a>%s</a>' % (healthy % (u'休眠[%d]'   % len(sleeping_list))) if len(sleeping_list)>0 else ''
                                ])
            else:
                node_status = '<a>%s</a>' % (lowlight % '')
        except:
            pass

        cluster = clusterOpt.select()
        if cluster['status'] == 'off':
            cluster_status = highlight % (u'<span style="font-weight:bold;">停止</span>')
        elif cluster['status'] == 'on':
            cluster_status = healthy % (u'<span style="font-weight:bold;">运行</span>')
        else:
            cluster_status = u'<span style="font-weight:bold; color:#ff7f00;">%s</span>' % cluster['status']

        return render.glob_stat(cluster_status=cluster_status,
                                serv_status=serv_status,
                                fs_status = fs_status,
                                node_status=node_status)

class Glob_disk:
    @login_required
    def GET(self):
        try:
            rslt = bridge_moni.node()
        except:
            print "edog error, in div.py glob_disk get"
            return "Get message error, "

        disk_total = 0
        disk_used = 0
        disk_free = 0
        for n in rslt:
            if n['disk']['total'] and n['disk']['used'] and n['disk']['free']:
                disk_total += n['disk']['total']
                disk_used += n['disk']['used']
                disk_free += n['disk']['free']

        disk_total = disk_total/(1024*1024)
        disk_used = disk_used/(1024*1024)
        disk_free = disk_free/(1024*1024)

        #########TEMP######################################################
        disk_free = disk_total - disk_used
        ###############################################################

        return render.glob_disk(disk_total=disk_total, disk_used=disk_used, disk_free=disk_free)

class Glob_rrd:
    @login_required
    def GET(self):
        img_urls = []
        glob_imgs_dir = os.path.join(ua_conf.rrd_imgs_dir, 'glob')
        try: mkdirs(glob_imgs_dir)
        except: raise
        load_img_path = os.path.join(glob_imgs_dir, 'load.png')
        mem_img_path = os.path.join(glob_imgs_dir, 'mem.png')
        swap_img_path = os.path.join(glob_imgs_dir, 'swap.png')
        nw_img_path = os.path.join(glob_imgs_dir, 'nw.png')
        iops_img_path = os.path.join(glob_imgs_dir, 'iops.png')
        disk_img_path = os.path.join(glob_imgs_dir, 'disk.png')
        img_urls.append(load_img_path)
        img_urls.append(mem_img_path)
        img_urls.append(swap_img_path)
        img_urls.append(nw_img_path)
        img_urls.append(iops_img_path)
        img_urls.append(disk_img_path)
        #############################################################
        #下面这条语句加上后会造成刷新的时间短于预设的值。
#        web.header('Cache-Control', 'no-cache, must-revalidate')
        return render.glob_rrd(tip='', img_urls=img_urls)

class Rack_mng:
    @login_required
    def GET(self):
        return render.rack_mng()

class Rack_list:
    @login_required
    def GET(self):
        racks = rackOpt.select()
        nodes = nodeOpt.select()
        rack_node_count = {}
        for n in nodes:
            if str(n['rack']) in rack_node_count.keys():
                rack_node_count[str(n['rack'])] += 1
            else:
                rack_node_count[str(n['rack'])] = 1
        for r in racks:
            if str(r['id']) in rack_node_count.keys():
                r['node_count'] = rack_node_count[str(r['id'])]
            else:
                r['node_count'] = 0
        return render.rack_list(racks=racks)

class Node_dtls:
    @login_required
    def GET(self):
        try:
            id = web.input().id
        except:
            id = 1
        moni_nodes = bridge_moni.node(id=id)
        #########################################################
        node = bridge_node.node_list(id=id)[0]
        img_urls = []
        node_dir = os.path.join(ua_conf.rrd_imgs_dir, str(node['rack']), str(node['id']))
        try: mkdirs(node_dir)
        except: raise
        load_img_path = os.path.join(node_dir, 'load.png')
        mem_img_path = os.path.join(node_dir, 'mem.png')
        swap_img_path = os.path.join(node_dir, 'swap.png')
        nw_img_path = os.path.join(node_dir, 'nw.png')
        iops_img_path = os.path.join(node_dir, 'iops.png')
        img_urls.append(load_img_path)
        img_urls.append(mem_img_path)
        img_urls.append(swap_img_path)
        img_urls.append(nw_img_path)
        img_urls.append(iops_img_path)
        #############################################################
        return render.node_dtls(node=moni_nodes[0], img_urls=img_urls)

class Node_add:
    @login_required
    def GET(self):
        return render.node_add(racks=rackOpt.select())

class Node_role_list:
    @login_required
    def GET(self):
        nm = []
        try:
            nm = bridge_node.node_srvc_list()
        except:
            pass
        nodes_in_cluster = []
        if nm:
            for n in nm:
                try:
                    if n['cluster'] < 0 or n['cluster'] == None:
                        continue
                except:
                    continue
                nodes_in_cluster.append(n)
        return render.node_role_list(nodes=nodes_in_cluster)

class Node_role_set:
    @login_required
    def GET(self):
        return render.node_role_set()

class Node_rrd:
    @login_required
    def POST(self):
        input = web.input()
        try:
            nodes = bridge_node.node_list()
        except:
            print "-------------- %s , get message error,"%(edog_host)
            raise web.internalerror(message = "%s, get message error,"%(edog_host))
    #    print nodes
        if not nodes:
            return render.node_rrd(tip='No node in uss!', img_urls=[])
        img_urls = []
        for n in nodes:
            node_dir = os.path.join(ua_conf.rrd_imgs_dir, str(n['rack']), str(n['id']))
            try: mkdirs(node_dir)
            except: raise
            load_img_path = os.path.join(node_dir, 'load.png')
            mem_img_path = os.path.join(node_dir, 'mem.png')
            swap_img_path = os.path.join(node_dir, 'swap.png')
            nw_img_path = os.path.join(node_dir, 'nw.png')
            iops_img_path = os.path.join(node_dir, 'iops.png')
            disk_img_path = os.path.join(node_dir, 'disk.png')
            if input.attr == 'load':
                img_urls.append([n['ip'],load_img_path])
            elif input.attr == 'mem':
                img_urls.append([n['ip'],mem_img_path])
            elif input.attr == 'swap':
                img_urls.append([n['ip'],swap_img_path])
            elif input.attr == 'nw':
                img_urls.append([n['ip'],nw_img_path])
            elif input.attr == 'iops':
                img_urls.append([n['ip'],iops_img_path])
            elif input.attr == 'disk':
                img_urls.append([n['ip'],disk_img_path])
            else:
                img_urls.append([n['ip'],load_img_path])
        return render.node_rrd(tip='', img_urls=img_urls)

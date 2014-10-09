#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101015
Author: zkz
"""
import tree_builder

from ua.common.sqlitedb import LocalSqliteDB
from ua.common.utils import current_dir
from ua.common.ua_conf import db_dir, sql_dir, ua_password_file

import web
from web.contrib.template import render_jinja

import os
import json
import sqlite3
import time
import hashlib
import re

urls = (
        '/tree', 'Tree',
        '/page', 'Page',
        '/changepasswd', 'Changepasswd', #简化方案
        '/.*', 'Page'
        )

app = web.application(urls, locals())
render = render_jinja('webapps/user/templates/', encoding='utf-8',)

#########################################################
print 'user.py db_dir:', db_dir
print 'user.py sql_dir:', sql_dir
user_db = LocalSqliteDB('user', db_dir, sql_dir)
db = os.path.join(db_dir, 'user.db')
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

def read_password(path):
    with open(path, "r") as f:
        return (f.read()).strip()

def write_password(path, content):
    with open(path, "w") as f:
        f.write(content)

def encode_password(plain_text):
    return hashlib.md5(plain_text.strip()).hexdigest()

class Changepasswd():
    @login_required
    def GET(self):
        return render.changepasswd()
    @login_required
    def POST(self):
        x = web.input()
        currpasswd_hash = read_password(ua_password_file)
        oldpasswd_hash = encode_password(x['oldpasswd'])

        print x
        print 'current %s' % currpasswd_hash
        print 'input (%s -> %s'   % (x['oldpasswd'], oldpasswd_hash)

        if not oldpasswd_hash == currpasswd_hash:
            return '现有密码输入错误'

        if not x['newpasswd1'].strip() == x['newpasswd2'].strip():
            return '输入的新密码不一致'

        m = re.match("^[a-z0-9A-Z_]*$", x["newpasswd1"].strip())
        if not m:
            return '密码格式输入错误，应该是字母，数字和下划线的组合'

        try:
            newpasswd_hash = encode_password(x['newpasswd1'])
            print 'write %s to %s' % (newpasswd_hash, ua_password_file)
            write_password(ua_password_file, newpasswd_hash)
            return '密码修改成功'
        except Exception, e:
            print e
            return '密码修改失败'

class Page:
    @login_required
    def GET(self):
        return render.page()

class Tree:
    def GET(self):
        x = web.input()
        if x["operation"] == "get_children":
            nodeId = x["id"].split("_")[1]
            #暂时是一次行加载。下一步实现根据请求的id来实现动态加载。
            if str(nodeId) != str(-1):
                group1 = {}
            else:
                group1 = tree_builder.getJson(-1, "group")
            return json.dumps(group1)
        elif x["operation"] == "search":
            search_str = x["search_str"]
            group1 = tree_builder.getJson(-1, "group")
            return json.dumps(group1)
    def POST(self):
        x = web.input()
        conn=sqlite3.connect(db)
        conn.text_factory=str
        c=conn.cursor()

        if "operation" in x.keys() and x.operation == "create_node":
            assert len(x["id"].split("_")) == 3
            nodeType = x["type"]
            nodeName = x["title"]
            fnodeId = x["id"].split("_")[1]
            if nodeType == "user":
                print "in create user."
                password = x["password"]
                (num, ) = c.execute("select count(*) from user where name = '%s'"\
                        %(nodeName))
                if num[0] == 1:
                    response = {"status": False, "id":"", "msg":"this name was existed."}
                else:
                    (num, ) = c.execute("select max(id) from user ")
                    maxId = num[0]
                    pwhash = hashlib.md5(password).hexdigest()
                    c.execute("insert into user values (null, '%s', '%s', '%s', '%s')"\
                            %(nodeName, pwhash, fnodeId, time.ctime()))
                    response = {"status": True, \
                            "id":"node_" + fnodeId + "_" + str(maxId + 1) + "_user", "msg":"create user success."}
            elif nodeType == "group":
                print "in create group."
                permis = x["permis"]
                (num, ) = c.execute("select count(*) from grop where name = '%s'"\
                        %(nodeName))
                if num[0] == 1:
                    response = {"status": False, "id":"", "msg":"this name was existed."}
                else:
                    (num, ) = c.execute("select max(id) from grop ")
                    maxId = num[0]
                    c.execute("insert into grop values (null, '%s', '%s', '%s', '%s')"\
                            %(nodeName, fnodeId, permis, time.ctime()))
                    response = {"status": True, \
                            "id":"node_" + fnodeId + "_" + str(maxId + 1) + "_group", "msg":"create group success."}
            else:
                response = {"status": False, "id":"", "msg":"i don't know this type."}
        elif "operation" in x.keys() and x.operation == "remove_node":
            """         """
            #现在只支持的是cut操作。和copy操作不同点在于,x=web.input里面copy的值不同。
            #cut 操作，copy=0, copy操作,copy=1.
            assert len(x["id"].split("_")) == 3
            nodeId = x["id"].split("_")[1]
            fnodeId = x["id"].split("_")[0]
            nodeType = x["id"].split("_")[2]
            assert nodeType == "user" or nodeType == "group"
            if nodeType == "user":
                try:
                    c.execute("delete from user where id = '%s'"%(nodeId));
                    response = {"status": True, "id":"", "msg":"rename success."}
                except Exception, e:
                    response = {"status": False, "id":"", "msg":e}
            if nodeType == "group":
                try:
                    c.execute("delete from grop where id = '%s'"%(nodeId));
                    response = {"status": True, "id":"", "msg":"remove success."}
                except Exception, e:
                    response = {"status": False, "id":"", "msg":e}
        elif "operation" in x.keys() and x.operation == "rename_node":
            assert len(x["id"].split("_")) == 3
            nodeId = x["id"].split("_")[1]
            nodeType = x["id"].split("_")[2]
            newNodeName = x["title"]
            assert nodeType == "user" or nodeType == "group"
            if nodeType == "user":
                try:
                    c.execute("update user set name = '%s' where id = '%s'"%(newNodeName, nodeId));
                    response = {"status": True, "id":"", "msg":"rename success."}
                except Exception, e:
                    response = {"status": False, "id":"", "msg":str(e)}
            if nodeType == "group":
                try:
                    c.execute("update grop set name = '%s' where id = '%s'"%(newNodeName, nodeId));
                    response = {"status": True, "id":"", "msg":"rename success."}
                except Exception, e:
                    response = {"status": False, "id":"", "msg":str(e)}

        elif "operation" in x.keys() and x.operation == "move_node":
            copy = x["copy"]
            nodeId = x["id"].split("_")[1]
            nodeType = x["id"].split("_")[2]
            oldFnode = x["id"].split("_")[0]
            newFnodeId = x["ref"].split("_")[1]
            assert nodeType == "user" or nodeType == "group"
            if nodeType == "user":
                try:
                    c.execute("update user set grop = '%s' where id = '%s'"%(newFnodeId, nodeId));
                    response = {"status": True, "id":"", "msg":"move success."}
                except Exception, e:
                    response = {"status": False, "id":"", "msg":str(e)}
            if nodeType == "group":
                try:
                    c.execute("update grop set grop = '%s' where id = '%s'"%(newFnodeId, nodeId));
                    response = {"status": True, "id":"", "msg":"move success."}
                except Exception, e:
                    response = {"status": False, "id":"", "msg":str(e)}
        else:
            c.close()
            return web.internalerror(message = "now don't support this operation!")

        conn.commit()
        conn.close()
        return json.dumps(response)

#       return web.internalerror(message = "hello,for test!")

#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101222
Author: zkz
"""

from ua.common.secty.utils import request

import json
import httplib
import time
import commands
import os

####################给自己的ear程序发送消息,测试用
host = "192.168.1.189"
port = "9600"
url = "/ear/srvc/stat"
headers = {}
headers["Content-Type"] = "application/json"
method = "POST"
d = {}
d['info'] = u'alert 这个是出错测内容，请联系管理员'
d['time'] = 1303204714
d['catagory'] = 'alert'
d['event'] = 'test'
d['action'] = 'test'
d['level'] = 'warn'
"""{u'info': u'192.168.1.201 leaved', u'time':
u'1303204717', u'catagory': u'alert', u'event':
u'agent_leave', u'level': u'warn'}"""
params = json.dumps(d)
resp = request(host, method, url, headers, params, port)
print resp


##########用来实验读取文件
tmp_file = "/var/log/uss/ua/ear.log"
status, output = commands.getstatusoutput("tail -n 10 %s"
        %(tmp_file))
#print status, output
reverse_lines = []
if status == 0:
    tail_lines = output.split(os.linesep)
    print tail_lines
    reverse_lines = tail_lines[::-1]
print reverse_lines

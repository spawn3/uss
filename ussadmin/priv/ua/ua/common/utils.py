#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20101114
Author: zkz
"""

import os
import commands
import subprocess
import re
import httplib
import logging
import logging.handlers


def current_dir():
    return os.path.dirname(__file__)

def content_replace(file_path, tobe_prepared, prepared_by):
    c = open(file_path).read()
    c = re.sub(tobe_prepared, prepared_by, c)
    open(file_path, 'wb').write(c)

def mkdirs(path):
    try:
        os.makedirs(path)
    except OSError, e:
        if 17 == e[0]:  #OSError: [Errno 17] File exists: PATH
            pass
    except:
        raise

def strtolist(str):
    """[('/dev/sdb1', '5226916', '8956', '5217960', '1%', '/srv/node/sdb1'),
        ('/dev/sdc1', '5226916', '8956', '5217960', '1%', '/srv/node/sdc1')]"""

    strlist = re.findall('(\(.[^\)]+\))', str)
    list = []
    for s in strlist:
        slist = s[1:-1].split(""', '"")
        l = []
        for i in slist:
            l.append(i[1:-1])
        list.append(l)
    return list


def local_exec(cmd):
    try:
        p = subprocess.Popen(args=cmd,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE,
                             close_fds=True,
                             shell=True)
        p.wait()
        out = p.stdout.read()
        err = p.stderr.read()
        return out, err
    except:
#        raise Exception('Error: Local_exec failed')
        raise


def request(host, method, url, headers, params, port=80):
    try:
        conn = httplib.HTTPConnection(host, port)
        conn.request(method=method, url=url, body=params, headers=headers)
        resp = conn.getresponse()
        data = resp.read()
        conn.close()
        return data
    except:
        raise

def deployKey(newNodeIp, user, passwd, \
        genkey = "common/script/genkey.sh"):
    """
        newNodeIp:"192.168.1.1"
        调用genkey.sh来使集群内的机器相互登录，免除输入密码
        用到genkey.sh放在script目录。
    """
    status, output = commands.getstatusoutput("%s %s %s"%\
            (genkey, newNodeIp, passwd))
#    print output
    if status == 0 and len(output) != 0:
        return True
    else:
        return False


def get_log(log_name, log_file_path):
    """
        log_name,  日志系统的名字
        log_file_path, 日志文件的位置

        返回：
        return my_logger, handler
    """
    log_dir = os.path.dirname(log_file_path)
    LOG_FILENAME = log_file_path
    my_logger = logging.getLogger(log_name)
    my_logger.setLevel(logging.DEBUG)

    i = 20 #单位是一兆。
    handler = logging.handlers.RotatingFileHandler(
        LOG_FILENAME, maxBytes = 1024000*i, backupCount = 5
       )
    handler.setLevel(logging.DEBUG)
    formatter = logging.Formatter(
            "%(asctime)s-%(name)s-%(levelname)s-%(message)s")
    handler.setFormatter(formatter)
    my_logger.addHandler(handler)
    return my_logger


def sort_dict_list(target, sord, sidx):
    """
        target:[{"":"", "":"", ..}, {"":"", "":"", ..}, ...]
        sord: asc 升序/desc降序。
        sidx: 是target某个项目的key。
    """
    assert type(target) == list
    if len(target) > 0:
        assert type(target[0]) == dict
    #保证排序的关键字在列表的每个项里面都有。
    for x in target:
        assert sidx in x.keys()
    assert sord == "asc" or sord == "desc"
    c = {}
    c['asc'] = lambda x,y: x>y
    c['desc'] = lambda x,y: x<y
    for i in range(len(target)):
        for j in range(len(target)-i-1):
            x = target[j]
            y = target[j+1]
            if c[sord](x, y):
                target[j], target[j+1] = target[j+1], target[j]
    return target

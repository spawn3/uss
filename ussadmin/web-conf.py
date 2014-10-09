#!/usr/bin/env python
# -*- coding:utf-8 -*-

import os
import sys
import commands
import hashlib

#print __file__
#print os.path.realpath( __file__)
#print os.getcwd()

def ensure_dir(dir_name):
    """如果没有创建目录"""
    if not os.path.isdir(dir_name):
        print "*** make dir: %s" % dir_name
        os.makedirs(dir_name)

#根据用户输入的用户名来chown程序相关目录
def chown_walk(dir_name, uid, gid):
    """  """
    os.chown(dir_name, uid, gid)
    for root_dir_name, sub_dir_list, sub_file_list in os.walk(dir_name, topdown = False):
        # print root_dir_name, sub_dir_list
        for sub_file in sub_file_list:
            os.chown(os.path.join(root_dir_name, sub_file), uid, gid)
        for sub_dir in sub_dir_list:
            dir_t =  os.path.join(root_dir_name, sub_dir)
            if os.path.isdir(dir_t):
                os.chown(dir_t, uid, gid)

def clean_dir(dir_name):
    print '*** clean_dir: %s' % dir_name
    """ 接收一个目录名，清空该目录 """
    #删除文件
    for root_dir_name, sub_dir_list, sub_file_list in os.walk(dir_name, topdown = False):
        # print root_dir_name, sub_dir_list
        for sub_file in sub_file_list:
            os.remove(os.path.join(root_dir_name, sub_file))
        for sub_dir in sub_dir_list:
            dir_t =  os.path.join(root_dir_name, sub_dir)
            if os.path.isdir(dir_t):
                os.removedirs(dir_t)

##########################################################################
## main
##########################################################################
if not os.getuid() == 0:
    print "please use root or sudo"
    sys.exit(1)

#用户确认uss_ua处于停止状态
print u"Are you sure uss_ua has stopped？you can use the command 'stopua' to stop it."
answer = raw_input("continue, entry 'Y'：")
if not answer.strip() == "Y":
    sys.exit(1)

#输入你要运行给程序的用户名，创建它，并初始化该用户对相关目录的权限
user = None
uid = None
gid = None
while True:
    user = raw_input("user name? ")
    user = user.strip()
    has_user = None
    if user:
        with open("/etc/passwd", "r") as f:
            for l in f:
                l_list = (l.strip()).split(":")
                if user == l_list[0]:
                    uid = int(l_list[2])
                    gid = int(l_list[3])
                    has_user = True
                    break
    if has_user:
        break
    else:
        print "your entry user is not exist"

## global settings
working_directory = os.path.dirname(os.path.realpath( __file__))
os.chdir(working_directory)

ua_data_path = os.path.join(working_directory, "../data")

ua_conf_tpl  = os.path.join(working_directory, "conf/ua.conf.tpl")
ua_path      = os.path.join(working_directory, "priv/ua/ua")
sql_dir      = os.path.join(working_directory, "priv/ua/ua/model/sql/")
db_dir       = os.path.join(ua_data_path, "store/db/")
rrds_dir     = os.path.join(ua_data_path, "store/rrd/")

ensure_dir(ua_data_path)
ensure_dir(db_dir)
ensure_dir(sql_dir)
ensure_dir(rrds_dir)

##
ua_password_file = os.path.join(ua_data_path, 'conf/.password')
ua_conf          = os.path.join(ua_data_path, 'conf/ua.conf')

ensure_dir(os.path.dirname(ua_conf))
ensure_dir(os.path.dirname(ua_password_file))

print '*** current dir:    %s' % working_directory
print '*** ua_path:        %s' % ua_path
print '*** ua_data_path:   %s' % ua_data_path
print '*** sql_dir:        %s' % sql_dir
print '*** db_dir:         %s' % db_dir
print '*** rrds_dir:       %s' % rrds_dir

#根据当前的目录生成配置文件

with open(ua_conf_tpl, "r") as tpl:
    conf = tpl.read()
    conf = conf.replace("$UA_PATH", ua_path)
    conf = conf.replace("$DB_DIR", db_dir)
    conf = conf.replace("$SQL_DIR", sql_dir)
    conf = conf.replace("$RRDS_DIR", rrds_dir)
    conf = conf.replace("$UA_DATA_PATH", ua_data_path)
    with open(ua_conf, "w") as f:
        f.write(conf)

ua_run_dir = "/var/run/ua/"
ua_log_dir = "/var/log/ua/"

ensure_dir(ua_run_dir)
ensure_dir(ua_log_dir)

chown_walk(ua_path, uid, gid)
chown_walk(ua_run_dir, uid, gid)
chown_walk(ua_log_dir, uid, gid)

#初始化一个密码文件
with open(ua_password_file, "w") as f:
    passwd_hash = hashlib.md5("123456").hexdigest()
    f.write(passwd_hash)

#执行python setup.py install
# status, output = commands.getstatusoutput("%s %s %s" % ("python", "setup.py", "install"))

# print output
# if status == 0 and len(output) != 0:
#    print "install ok."

######
print "Default : user:root, passwd:root."
print ""
print "                      Welcome to use ussadmin!"
print ""

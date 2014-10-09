see docs/uss_user_guide.pdf for details.

依赖包
======

    expect
    gcc
    Erlang R14B01+
    Python 2.6.4+
        webpy
        Jinja2

识别代码版本
------------

Storage API
===========

SNMP
====

EVENT NOTIFICATION
------------------


2011-04-25
==========

- 测试
- 全面替换yfs-shell.sh的功能
- clean & dump log
- ssh


2011-04-13
==========

- change edog_master1.config to master1.config, etc
- volume management
    - lvm_create
    - lvm_resize
    - lvm_list
- get yfs service log
- event notification

Installation
============

See the following files:

- ussadmin/INSTALL.rst

Script
------

- ussadmin/bin/appctl

Ssh login
---------

各类服务器的选定
----------------

:c60: 前端指定，如果启动时没有指定，自动选择
:mds: 同上
:nfs: 目前默认每个节点启动一个
:proxy:
:cds: 扫描节点的目录结构获取

节点的加入和退出
----------------

启动，关闭等操作只针对集群节点起作用。

节点加入集群分为两步：

1. 节点加入到非集群节点列表
2. 节点加入到集群节点列表

节点退出集群也分为两步:

1. *从集群节点到非集群节点*
2. 从非集群节点列表脱离

非集群节点的状态
----------------

非集群节点要显示其实际状态

检查集群状态，看是否可以启动
----------------------------

- 所有节点处于alive状态
- 每个节点需要检查如下项
  | 目录/sysy/yfs/app/sbin
  | 目录/usr/local/db-4.8/


User management
---------------

数据采集
========

网络流量的计算方法
------------------

内存数据
--------

快速部署和扩容
--------------


WEB
===

当前文档:
--------------------------------------------------------------------------------------------------------

一, 部署步骤
1, 依赖软件：

    (1),(2)中描述的方法在centos 5.4 x86_64 2.6.18-164.el5版本测试安装通过。

    (1),web.py, Jinja2

        1，将ua/requirements/site-packages.tar.gz解压后，把解压后的site-packages/目录下的文件全部拷贝到python2.6安装目录下的site-packages目录下,
           如：/usr/local/lib/python2.6/site-packages
           这样就完成web.py，Jinja2，及python-setuptools(easy_install工具)的安装：

        2，如需使用python-setuptools(easy_install)工具，
           将python2.6/site-packages下的easy_install软链接到/usr/bin/easy_install即可

    (2),rrdtool

        centos中rrdtool安装方法参考：
        下载所需要的rpm包：
            wget http://dag.wieers.com/rpm/packages/rrdtool/perl-rrdtool-1.2.23-1.el5.rf.x86_64.rpm
            wget http://dag.wieers.com/rpm/packages/rrdtool/rrdtool-devel-1.2.23-1.el5.rf.x86_64.rpm
            wget http://dag.wieers.com/rpm/packages/rrdtool/rrdtool-1.2.23-1.el5.rf.x86_64.rpm
        安装方法：
            rpm -ivh rrdtool-1.2.23-1.el5.rf.x86_64.rpm rrdtool-devel-1.2.23-1.el5.rf.x86_64.rpm perl-rrdtool-1.2.23-1.el5.rf.x86_64.rpm

2, 配置：

    UA_ROOT=/sysy/yfs/ussadmin/ussadmin/priv/ua
    UA_DATA=/sysy/yfs/ussadmin/data/

    python web-conf.py
        - $UA_DATA/conf/ua.conf.tpl生成$UA_DATA/conf/ua.conf
        - 确保$UA_DATA/conf/ua.conf中的edog_info/host,port为edog manager的正确地址
        - See also: WEB_ROOT/ua/common/ua_conf.py


3, 安装( 使用root权限)

    a,	创建一个新的用户，比如说ua,此处创建的用户名，在执行python web-conf.py时会要求输入此处创建的用户名。
        useradd ua
        passwd ua
    b,	cd WEB_ROOT
    c,	python web-conf.py

4, 运行系统：

    su ua
    python WEB_ROOT/ua/ua_main.py 0.0.0.0:9600
    python WEB_ROOT/bin/stopua

二，访问方式

1，用浏览器访问 0.0.0.0:9600 （0.0.0.0根据实际情况）
2，打开页面后，使用user:root, passwd:root 访问。登录后可修改密码。也可使用命令行工具“passwdua”来更改密码。

三，工具说明

正常安装，会安装三个和uss_ua相关的命令行工具，方便进行系统的管理
1， passwdua
		用来修改uss_ua 的登录密码，用户名为root，对用户来说这个登录用户名是不可变的。
2,  cleanua
		用来清除uss_ua 过程中的临时数据，
3,  stopua
		用来停止uss_ua





history 1:
--------------------------------------------------------------------------------------------------------
1,依赖软件：
	(1),web.py
	(2),Jinja2
		1，将ua/requirements/site-packages.tar.gz(解压)中的文件全部拷贝到python2.6安装目录下的site-packages目录下,
		   如：/usr/local/lib/python2.6/site-packages
		   这样就完成web.py，Jinja2，及python-setuptools(easy_install工具)的安装：
		2，如需使用python-setuptools(easy_install)工具，
		   将python2.6/site-packages下的easy_install软链接到/usr/bin/easy_install即可

	(3),rrdtool
		centos中rrdtool安装方法参考：
			下载所需要的rpm包：
				wget http://dag.wieers.com/rpm/packages/rrdtool/perl-rrdtool-1.2.23-1.el5.rf.x86_64.rpm
				wget http://dag.wieers.com/rpm/packages/rrdtool/rrdtool-devel-1.2.23-1.el5.rf.x86_64.rpm
				wget http://dag.wieers.com/rpm/packages/rrdtool/rrdtool-1.2.23-1.el5.rf.x86_64.rpm
			安装方法：
				rpm -ivh rrdtool-1.2.23-1.el5.rf.x86_64.rpm rrdtool-devel-1.2.23-1.el5.rf.x86_64.rpm perl-rrdtool-1.2.23-1.el5.rf.x86_64.rpm

			以上方法在2.6.18-164.el5版本测试安装通过。

2, 配置：
	确保ua/ua.conf中的edog_info/host,port为edog的正确地址。若在本机，无须改动配置。

3, 运行系统：
	cd uss/ua
	python ua.py 0.0.0.0:9600 (root用户执行，备注edog也应该是root用户)


see docs/whs_user_guide.pdf for details.

依赖包
======

    erlang R14B01+
    python 2.6.4+
        django-1.0.1

    git clone http://192.168.1.2/uss.git
    cd uss
    git checkout -b test origin/test
    make deps
    make
    make install
    make version


工具
====

    cd /sysy/yfs/ussadmin/edog

    bin/appctl

    bin/appctl cluster conf master1_ip master2_ip ...
    bin/appctl cluster deploy
    bin/appctl cluster start
    bin/appctl cluster restart
    bin/appctl cluster stop
    bin/appctl cluster ping

Directory Structure
===================

    APP_ROOT=/sysy/yfs/ussadmin

    $APP_ROOT/edog
    $APP_ROOT/edog_runtime
    $APP_ROOT/edog_runtime/conf
    $APP_ROOT/edog_runtime/db
    $APP_ROOT/edog_runtime/manager.log
    $APP_ROOT/edog_runtime/agent.log
    $APP_ROOT/edog_runtime/web.log


conf说明
========

集群节点配置文件，主要是方便部署和启动各个slave节点
    conf/cluster.xml

模板配置文件
    conf/stddisk.conf

multi masters部署
=================

1. 生成配置文件

    bin/appctl cluster conf 192.168.1.21 192.168.1.22

2. 分发到各个节点上

    bin/appctl cluster deploy

3. 启动managers和agents

   bin/appctl cluster start

4. high availability

    failover & takeover


网络中断的处理
==============

1. agent检测到无法连接到manager，在一定的超时设定t1下，关闭其所在节点的所有VM
2. manager检测到无法连接到agent，在一定的超时设定t2下，重置数据库中VM的状态
3. 配置manager的处理策略

不变式

1. VM单实例运行
2. VM的实际状态与数据库中VM的状态一致

manager上数据库的维护
=====================

1. 从两个manager部署，在保留数据的情况下，改完单manager部署

    设两个manager分别为edog_master@192.168.1.21 (M1), edog_master@192.168.1.22(M2)
    保留M1，去掉M2

    步骤:
        a. 启动M1，M2
        b. 登录到M1，方法见[登录edog_master].
        c. 执行命令
           edog_mnesia:del_schema('edog_master@192.168.1.22').
        d. 重新生成单manager的配置文件
           bin/appctl cluster conf 192.168.1.21
        e. 关闭M2
        f. 重启M1


登录edog_master
===============

在manager所在的节点上，启动一新的erl进程，如

    $ erl -name t@192.168.1.21 -setcookie edog

在erl shell里，按Ctrl-g组合健进入-->

    --> h                                 查看帮助
    --> r 'edog_master@192.168.1.21'      打开远程shell
    --> j                                 查看当前打开的shell
    --> c 2<enter>                        连接到第N个shell，这里通常为2, 通过j命令查看

如此，就登录到manager shell了，可以执行相关命令。


LOG
===

    重启会重置日志文件，历史日志可以查看二进制日志。

Ip pool
=======

VM状态监控
==========

    http://192.168.1.21:9501/tables

用户管理
========

    权限按角色进行组织，用户被指定角色，从而获得相应权限
    至少分两种角色
        管理员
        普通用户
    可以添加角色，并分配合适的权限

    普通用户只能看到自己的信息，并进行操作的权限检查

VM状态的检测
============

    在libvirt关闭后，会发生误判，系统会尝试启动另一VM，造成镜像损坏。

    迁移过程中，目的端libvirtd关闭


添加VM模板
==========



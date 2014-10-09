BOOT
----

    bin/ussctl master start <ip> <config> [fg]


    >> 同时启动web前端, 假定其代码在/sysy/yfs/ussadmin/ussadmin/priv/ua/

LOG
---

    /sysy/yfs/ussadmin/data/master.log
    /sysy/yfs/ussadmin/data/agent.log

示例集群配置
    dir layout
    | /sysy/yfs/c60/
    | /sysy/yfs/mds/
    | /sysy/yfs/cds/
    | /sysy/yfs/nfs
    | /sysy/yfs/proxy/
    log
    | /var/log/uss/
    所有节点
    | t1: 10.0.213.194
    | t2: 10.0.213.195
    | t3: 10.0.213.196
    | t4: 10.0.213.197
    | t5: 10.0.213.198
    服务实例
    | c60: 第一次运行时自动选择，或静态指定(方法见下面的说明)
    | mds: 第一次运行时自动选择，或静态指定(方法见下面的说明)
    | cds: 通过扫描$PREFIX/cds发现
    | nfs: 每个节点上启动一个实例
    | proxy: 每个节点上启动一个实例
    目录扫描机制
    | 以CDS为例，如果在/sysy/yfs/cds/目录下有数字命名的目录，则认定为cds服务;
    | 不过，若该目录下有文件名为__skip_me__的文件，则跳过该目录。
    集群管理
    | web server 10.0.213.194
    | master 10.0.213.194
    | agent 每个节点上启动一个实例
依赖软件包
    目录层次
    | USSADMIN_ROOT=/sysy/yfs/ussadmin/ussadmin
    | USSADMIN_DATA=/sysy/yfs/ussadmin/data
    Erlang R14B01
    | ./configure
    | make
    | make install
    | 会安装到/usr/local/lib/erlang
    libdb-4.8.so
    | /etc/ld.so.conf.d/sysy.conf
    | /usr/local/db-4.8/lib
    共享库
    | $USSADMIN_ROOT/script/after-deploy.sh
在某一控制节点上，例如(10.0.213.194)
    准备ussadmin软件包
    | make
    | make nif
    | make install
    | 安装ussadmin到目录$USSADMIN_ROOT
    生成配置文件
    | cd $USSADMIN_ROOT
    | script/gen.sh web 10.0.213.194 10.0.213.194
    | /web后的参数是webserver的IP，默认为127.0.0.1
    | /后接master列表，为消除master的单点，可以指定多个。
    启动master
    | script/start_master.sh 10.0.213.194 master1
    添加节点
    | edog_mnesia:pm_add({1, <<"10.0.213.194">>}).
    | edog_mnesia:pm_add({1, <<"10.0.213.195">>}).
    | edog_mnesia:pm_add({1, <<"10.0.213.196">>}).
    | edog_mnesia:pm_add({1, <<"10.0.213.197">>}).
    | edog_mnesia:pm_add({1, <<"10.0.213.198">>}).
    选择c60
    | edog_mnesia:set_list2(c60, [1,2]).
    选择mds
    | edog_mnesia:set_list2(mds, [1,2]).
    启动关闭YFS
    | edog_master:yfs_start().
    | edog_master:yfs_stop().
    查看表的内容
    | edog_mnesia:show(uss_option_t).
    | edog_mnesia:show(uss_rack_t).
    | edog_mnesia:show(uss_pm_t).
    | edog_mnesia:show(uss_yfs_t).
    | edog_mnesia:show(uss_queue_t).
    查看集群各个节点的连通性
    | edog_pm:cluster_status().
    其他操作
    | L = [<<"10.0.213.195">>,<<"10.0.213.196">>,<<"10.0.213.197">>,<<"10.0.213.198">>],
    | edog_pm:cluster_deploy(L, ussadmin).
    | edog_pm:cluster_start(L).
注意事项
    清除数据
        物理清除
        | rm -rf $USSADMIN_DATA
        逻辑清除
        | edog_mnesia:clear_tables().
    网络配置
        ssh
        | 配置publickey：script/ssh.sh -t hostfile
        | 添加一个节点:  script/ssh.sh -a host passwd
        ip网段：YFS配置的network,ussadmin启动脚本参数ip和添加节点的ip必须位于一个网段
    如果启动集群失败, 请手工启动YFS的某个失败的服务，进行诊断
    USSAdmin与虚拟机管理系统共存
        USSAdmin
        | 软件安装到$USSADMIN_ROOT
        | data dir为$USSADMIN_DATA
        | 默认使用9600,9601系列端口
        | master和agent的erlang节点名依次为ussadmin_master,ussadmin
        虚拟机管理系统
        | 软件安装到/sysy/yfs/edog
        | data dir为/sysy/yfs/edog_runtime
        | 默认使用9500,9501系列端口
        | master和agent的erlang节点名依次为edog_master, edog

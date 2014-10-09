#!/usr/bin/env python

from optparse import OptionParser
from xml.etree.ElementTree import ElementTree
import os
import sys

working_dir = os.path.dirname(os.path.realpath( __file__))
sys.path.insert(0, '%s/priv' % working_dir)

app_root     = '/sysy/yfs/ussadmin'
app_src_dir  = working_dir
app_data_dir = '%s/../edog_runtime' % app_src_dir

import mdsoss.oss.toolbox_json as toolbox_json

def runit(cmd):
    print "***********************************************************"
    print "*** command *** %s" % cmd
    os.system(cmd)

def edog_deploy(Slaves):
    for x in Slaves:
        print "*****************************************************************"
        print "*** deploy edog on %s: " % (x)
        if options.edog:
            cmd = "rsync -avz %s/edog root@%s:%s" % (app_root, x, app_root)
            runit(cmd)
            cmd = "rsync -avz %s/edog_runtime/conf root@%s:%s/edog_runtime" % (app_root, x, app_root)
            runit(cmd)
            erl_start([x])
        if options.app:
            cmd = "rsync -avz /sysy/yfs/app root@%s:/sysy/yfs" % (x)
            runit(cmd)
            cmd = "ssh root@%s 'bash %s/script/set_local.sh %s'" % (x, working_dir)
            runit(cmd)
        if options.etc:
            cmd = "rsync -avz /sysy/yfs/etc root@%s:/sysy/yfs" % (x)
            runit(cmd)
        if options.kvm:
            cmd = "rsync -avz /sysy/yfs/kvm root@%s:/sysy/yfs" % (x)
            runit(cmd)
            cmd = "rsync -avz /sysy/yfs/libvirtd77 root@%s:/sysy/yfs" % (x)
            runit(cmd)
        if options.start:
            erl_start([x])
        if options.stop:
            erl_stop([x])
        if options.startlibvirtd:
            start_libvirtd([x])
        print "*** deploy completed!!!"

    if options.libvirtd:
        start_libvirtd_2()

def erl_start(Slaves):
    for x in Slaves:
        print "*****************************************************************"
        print "*** start erl on %s: " % (x)
        cmd = "ssh  root@%s '%s/script/start_agent.sh %s'" % (x, working_dir, x)
        runit(cmd)

def erl_stop(Slaves):
    for x in Slaves:
        print "stop erl on %s: " % (x)
        cmd = "ssh  root@%s '%s/script/start_agent.sh %s stop'" % (x, working_dir, x)
        runit(cmd)

def start_libvirtd(Slaves):
    for x in Slaves:
        print "*****************************************************************"
        print "*** start libvirtd on %s: " % (x)
        cmd = "ssh  root@%s 'pkill -9 libvirtd; rm -rf /sysy/yfs/edog/libvirtd.pid; /sysy/yfs/libvirtd77/sbin/libvirtd -d -p /sysy/yfs/edog/libvirtd.pid'" % (x)
        runit(cmd)

def start_libvirtd_2():
    #cmd = "erl_call -n edog_master@ecloud.org -c edog -a 'edog_master libvirt_start []'"
    #runit(cmd)
    toolbox_json.action('libvirt_start')

def main():
    edog_deploy(Slaves)

if __name__ == '__main__':
    working_dir = os.path.dirname(os.path.realpath(__file__))

    parser = OptionParser()
    parser.add_option("-a", "--app",      dest ="app",    action="store_true", default=False)
    parser.add_option("-c", "--etc",      dest ="etc",    action="store_true", default=False)
    parser.add_option("-e", "--edog",     dest ="edog",   action="store_true", default=False)
    parser.add_option("-k", "--kvm",      dest ="kvm",    action="store_true", default=False)
    parser.add_option("-s", "--start",    dest ="start",  action="store_true", default=False)
    parser.add_option("-t", "--stop",     dest ="stop",    action="store_true", default=False)
    parser.add_option("-l", "--startlibvirtd", dest ="startlibvirtd",  action="store_true", default=False)
    parser.add_option("-m", "--libvirtd", dest ="libvirtd",  action="store_true", default=False)
    (options, args) = parser.parse_args()

    #############################################
    # TODO
    # EdogMaster = '192.168.1.15'
    Slaves = []

    tree = ElementTree()
    tree.parse('%s/conf/cluster.xml' % app_data_dir)

    # e_master = tree.find('master')
    # EdogMaster = e_master.get('ip')

    e_slaves = tree.findall('slave')
    for i in e_slaves:
        Slaves.append(i.get('ip'))

    #############################################

    # print "EdogMaster is", EdogMaster
    print "Agents are", Slaves

    main()

#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import os
import sys
from optparse import OptionParser
from xml.etree.ElementTree import ElementTree

def gen_list(L):
    N = len(L)
    for i in xrange(N):
        if i == N-1:
            print "\t\t'edog_master@%s'" % (L[i])
        else:
            print "\t\t'edog_master@%s'," % (L[i])

def gen_kernel(idx):
    print "{kernel, ["
    if options.master:
        print "\t{distributed, [{edog, ['edog_master@%s', {" % (Masters[0])
        gen_list(Masters[1:])
        print "\t\t}]}]},"
        print "\t{sync_nodes_mandatory, ["
        L = Masters[:]
        del L[idx]
        gen_list(L)
        print "\t\t]},"
        print "\t{sync_nodes_timeout, 10000},"
        print "\t{error_logger, {file, \"%s/edog_runtime/master.log\"}}" % (YFS_PREFIX)
    else:
        print "\t{error_logger, {file, \"%s/edog_runtime/agent.log\"}}" % (YFS_PREFIX)
    print "\t]},"

def gen_sasl():
    print "{sasl, ["
    print "\t{sasl_error_logger, false},"
    print "\t{errlog_type, error},"
    print "\t{error_logger_mf_dir, \"%s/edog_runtime/error_logs\"}," % (YFS_PREFIX)
    print "\t{error_logger_mf_maxbytes, 10485760},"
    print "\t{error_logger_mf_maxfiles, 10}"
    print "\t]},"

def gen_inets():
    print "{inets, ["
    print "\t%% {services, [{httpd, [{proplist_file, \"%s/edog/www/inets.conf\"}]}]}" % (YFS_PREFIX)
    print "\t]},"

def gen_mnesia():
    print "{mnesia, ["
    print "\t{dir, \"%s/edog_runtime/edog_master\"}" % (YFS_PREFIX)
    print "\t]},"

def gen_edog():
    print "{edog, ["
    print "\t{ws_ip, \"%s\"}," % (WebServer)
    print "\t{ws_port, 9500},"
    print "\t{edog_master_timeout, 6},"
    print "\t{nodedown_timeout, 60},"
    print "\t{disk_format, \"raw\"},"
    print
    print "\t{edog_masters, ["
    gen_list(Masters)
    print "\t\t]},"
    print "\t{edog_slaves_timeout, 6},"
    print
    print "\t{storage_prefix, \"%s\"}," % ("/oss/")
    print
    print "\t{bin_kvm, \"%s/%s\"},"     % (YFS_PREFIX, "kvm/kvm88patch/bin/qemu-system-x86_64")
    print "\t{bin_kvm_img, \"%s/%s\"}," % (YFS_PREFIX, "kvm/kvm88patch/bin/qemu-img")
    print "\t{bin_virsh, \"%s/%s\"},"   % (YFS_PREFIX, "libvirtd77/bin/virsh")
    print "\t{bin_libvirtd, \"%s/%s\"}" % (YFS_PREFIX, "libvirtd77/sbin/libvirtd")
    print "\t]}"

def main():
    print "["
    gen_kernel(options.idx-1)
    if options.master:
        gen_inets()
        gen_mnesia()
    gen_sasl()
    gen_edog()
    print "]."

if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("-m", "--master", dest ="master", action="store_true", default=False)
    parser.add_option("-n", "--idx", dest ="idx", type="int",    default=1)
    parser.add_option("-w", "--web", dest ="web", type="string", default="127.0.0.1")
    (options, args) = parser.parse_args()

    WebServer = options.web
    if len(args) == 0:
        Masters = ["192.168.1.201", "192.168.1.202"]
    else:
        Masters = args

    assert len(Masters) > 0
    assert options.idx > 0 and options.idx <= len(Masters)

    YFS_PREFIX ="/sysy/yfs"

    main()

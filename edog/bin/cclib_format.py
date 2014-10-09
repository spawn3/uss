#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import os
import sys

## ---------------------------------------------------------------
## FORMAT
## ---------------------------------------------------------------
def to_string(x):
    if x == []:
        return ''
    else:
        return x

def int_to_string(x):
    return str(x)

class Format:
    def __init__(self, data):
        self.data = data
    def format(self):
        print '---------------------------------------------------------'
        print '*', self.header()
        print '---------------------------------------------------------'
        for x in self.data:
            self._format(x)
    def _format(self, info):
        pass

class VmFormat(Format):
    def __init__(self, data):
        Format.__init__(self, data)

    def header(self):
        return 'id|name|status|pm_id|vnc_port|cpu|memory|real_memory'

    def _format(self, info):
        print '|'.join([
            info['vm_id'],
            info['vm_name'],
            info['status'],
            to_string(info['pm_id']),
            int_to_string(info['port']),
            int_to_string(info['vm_cpu']),
            int_to_string(info['vm_mem'] / 1024),
            int_to_string(self._get_real_memory(info) / 1024)
            ])
    def _get_real_memory(self, info):
        extra = info['extra']
        if extra and extra.has_key('real_memory'):
            return extra['real_memory']
        return info['vm_mem']


class DiskFormat(Format):
    def __init__(self, data):
        Format.__init__(self, data)

    def header(self):
        return '|'.join([
            'userid',
            'diskid',
            'alias',
            'size(GB)',
            'boot',
            'path',
            ])

    def _format(self, info):
        attrs = info['canboot']
        boot = map_bool(attrs['boot'])
        print '|'.join([
            info['cust_id'],
            info['disk_id'],
            info['disk_alias'],
            str(info['size']),
            boot,
            info['path'],
            ])


def map_bool(x):
    if x:
        return "true"
    else:
        return "false"


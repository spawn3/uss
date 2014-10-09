#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20110123
Author: zkz
"""

#from common.utils import mkdir, local_exec

import os
import time

        
def mkdirs(path):
    try:
        os.makedirs(path)
    except OSError, e:
        if 17 == e[0]:  #OSError: [Errno 17] File exists: PATH
            pass
    except:
        raise
    
def local_exec(cmd):
    import subprocess
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
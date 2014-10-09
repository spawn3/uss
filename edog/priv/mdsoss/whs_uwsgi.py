#!/usr/bin/python
import os
import sys
from django.core.management import execute_manager
from django.core.handlers import wsgi


sys.path.append('/root/zhangjunfeng/uss/edog/priv/mdsoss')
sys.path.append('/root/zhangjunfeng/uss/edog/priv/')
os.environ['DJANGO_SETTINGS_MODULE'] = "settings"
application = wsgi.WSGIHandler()
   

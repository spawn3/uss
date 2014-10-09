#!/usr/bin/env python
#-*- coding: utf-8 -*-

import httplib
import json
import subprocess

api_web_host='127.0.0.1'
api_web_port=9501

erl_call_prefix = "erl_call -name ussadmin_master@ecloud.org -c ussadmin -a"

def run_erl_call(command):
    full_command = "%s %s" % (erl_call_prefix, command)
    infoP = subprocess.Popen(args=full_command,
            stdout=subprocess.PIPE,
            shell=True)
    info = infoP.stdout.read()
    print '--------------------------------------------------'
    print full_command
    print info
    print '--------------------------------------------------'
    return full_command, info

def api_get_request(url):
    try:
        conn = httplib.HTTPConnection(api_web_host, api_web_port, timeout=3)
        conn.request('GET', url)
        resp = conn.getresponse()
        info = resp.read()
        return info
    finally:
        conn.close()

def request(host, port, method, url, headers, params):
    data = ''
    try:
        conn = httplib.HTTPConnection(host, port, timeout=3)
        #conn.set_debuglevel(1)
        conn.request(method=method, url=url, body=params, headers=headers)
        resp = conn.getresponse()
        data = resp.read()
    except Exception, e:
        pass
        #print 'Exception', e
    finally:
        conn.close()

    return data

def request_json(url, params, host=api_web_host, port=api_web_port):
    '''这个地方的返回值应该修改为{None, None}--zhangjunfeng-10.31. '''
    headers = {}
    headers["Content-Type"] = "application/json"
    try:
        params = json.dumps(params)
        resp = request(host, port, method='POST', url=url, headers=headers, params=params)
        print url, resp
        if resp:
            return json.loads(resp)
        else:
            return {}
    except Exception, e:
        print 'Exception: ', e
        raise Exception('Bad response from %s:%d {%s, %s}' % (host, port, url, params))

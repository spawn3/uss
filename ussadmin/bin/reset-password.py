#!/usr/bin/env python
# -*- coding:utf-8 -*-

import hashlib

ua_password_file = '/sysy/yfs/ussadmin/data/conf/.password'

def encode_password(plain_text):
    return hashlib.md5(plain_text.strip()).hexdigest()

#初始化一个密码文件
with open(ua_password_file, "w") as f:
    passwd_hash = encode_password('123456')
    f.write(passwd_hash)

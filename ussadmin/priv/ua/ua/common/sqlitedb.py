#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Created on 20100920
Author: zkz
"""

import os
import sqlite3
from web.db import SqliteDB
from ua.common.ua_conf import *

class LocalSqliteDB(SqliteDB):

    def __init__(self, dbname, db_dir, sql_dir):
        self.db_dir = db_dir
        self.sql_dir = sql_dir

        self.dbpath = os.path.join(db_dir, dbname + '.db')
#        print self.dbpath
        if not os.path.exists(self.dbpath):
            conn = sqlite3.connect(self.dbpath)
            cursor = conn.cursor()
            sql = os.path.join(sql_dir, dbname + '.sql')
#            print sql
            cursor.executescript(open(sql).read())
            conn.commit()
            cursor.close()
            conn.close()
#            print 'create db: %s' % dbname

        SqliteDB.__init__(self, db=self.dbpath)
#        print 'connect db: %s' % dbname

    def __count__(self, table):
        conn = sqlite3.connect(self.dbpath)
        cursor = conn.cursor()
        result = cursor.execute("select count(*) from %s;" % table)
        conn.commit()
        count = result.fetchall()[0][0]
        cursor.close()
        conn.close()
        return count


if __name__ == '__main__':
    print db_dir, sql_dir
#    user_db = LocalSqliteDB('user', db_dir, sql_dir)
    alert_db = LocalSqliteDB('alert', db_dir, sql_dir)
    db = os.path.join(db_dir, "alert.db")
    conn = sqlite3.connect(db)
    conn.text_factory = str
    c = conn.cursor()

    level = 'warn'

    for i in range(10):
        c.execute("insert into alert_info (level, info, etime, event, catagory) \
                values (?, ?, ?, ?, ?)", (level, str(i), str(i), str(i), str(i)))
        conn.commit()

    rs = c.execute("select * from alert_info")
    for i in rs:
        print type(i), i
    print rs

    rs = c.execute("select count(*) from alert_info")
    for i in rs:
        print type(i), i
    print rs

    rs = c.execute("select * from alert_info limit 1")
    for i in rs:
        print type(i), i
    print rs


    rs = c.execute("select * from alert_info limit 1,1")
    for i in rs:
        print type(i), i
    print rs

    conn.close()


    #########################################################

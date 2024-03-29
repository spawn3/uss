#!/usr/bin/env python
#-*- coding:utf-8 -*-


from ua.common.utils import current_dir
from ua.common.ua_conf import db_dir

import web
import os
import sys
import sqlite3
import json
import time
import random

DEBUG = True

#########################################################
db = os.path.join(os.path.dirname(current_dir()), db_dir, 'user.db')

#db="/media/store/prog/garden/uss/ua/store/db/user_tree.db"
tableName = "jstree_view"

def getChildNode(nodeId, nodeType):
    """接受一个nodeId, 和该节点的类型。 返回它的子节点的Id和类型。 """
    if nodeType == "user":
        return []
    elif nodeType == "group":
        conn=sqlite3.connect(db)
        conn.text_factory=str
        c=conn.cursor()
        txt=c.execute("select * from '%s' where fnodeId = '%s' "%(tableName, nodeId))
        childNodeId = []
        for row in txt:
    #        print "row", row[2]
            childNodeId.append({"nodeId":row[1], "nodeType":row[3]})
    #        for el in row:
    #            print unicode(str(el),"utf-8"),
        conn.close()
        return childNodeId

def getJson(nodeId, nodeType):
    """
        在构建每个节点的id时候，是其父节点和节点的id的组合，为了方便上层的删除操作。
        nodeId为整数，nodeType 为group, user.
        使用递归的方式
    """
    assert type(nodeId) == int and ( nodeType == "group" or nodeType == "user")
    if nodeType == "user":
        conn=sqlite3.connect(db)
        conn.text_factory=str
        c=conn.cursor()
        txt=c.execute("select * from '%s' where nodeId = '%s' and nodeType = 'user' "%(tableName, nodeId))
        row = ""
        for row in txt:
#            print row
            nodeJson = {"data":row[0], "attr":{"id": "node_" + str(row[2]) + "_" + str(nodeId) + "_" + nodeType, "rel":row[3]}, }
#        print nodeJson
        return nodeJson
    elif nodeType == "group":
        if nodeId == -1:
            nodeJson = {"data":"all", "attr":{"id":"node_" + "null_" + str(nodeId) + "_group", "rel":"group"}, "children":[], "state":"open"}
            pass
        else:
            conn=sqlite3.connect(db)
            conn.text_factory=str
            c=conn.cursor()
            txt=c.execute("select * from '%s' where nodeId = '%s' and nodeType = 'group' "%(tableName, nodeId))
            nodeJson = {}
            row = ""
            for row in txt:
#                print row
                nodeJson = {"data":row[0], "attr":{"id":"node_" + str(row[2]) + "_" + str(nodeId) + "_" + nodeType, "rel":row[3]}, "children":[],"state":"open" }
        childNode = getChildNode(nodeId, nodeType)
        if childNode == []:
            return nodeJson
        else:
            for node in childNode:
#                print nodeJson
                nodeJson["children"].append( getJson(node["nodeId"], node["nodeType"]) )
                pass
            return nodeJson


if __name__=="__main__":
    print getJson(-1, "group")

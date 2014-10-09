#!/usr/bin/python
#-*- coding: utf-8 -*-
"""
Refectoring on 20101125
Author: zkz
"""

PERMIS = (
          (000, u'root'),
          
          (100, u'syst_view'),
          (101, u'syst_opt'),
          
          (200, u'node_view'),
          (200, u'rack_add'),
          
          (100, u'srvc_view'),
          
          (500, u'rbin_view'),
          
          (600, u'user_view'),
          (601, u'user_create'),
          (602, u'user_edit'),
          (603, u'user_destroy'),
          
          (600, u'group_view'),
          (601, u'group_create'),
          (602, u'group_edit'),
          (603, u'group_destroy'),
          
          (700, u'permissions_view'),
          (701, u'permissions_create'),
          (702, u'permissions_destroy'),
          (703, u'permissions_to_group'),
          )

SRVCTYPE = (
            (1, u'mds'),
            (2, u'cds'),
            (3, u'c60'),
            (4, u'ua'),
#            (5, u'uaa'),
            )

if __name__ == '__main__':
    print type(PERMIS)
    print PERMIS[0][1]
#from common.secty.node import NodeSql
#
#if __name__ == '__main__':
#    ns = NodeSql()
#    print ns.select()

##############################################################
from common.secty.node import RackOpt, NodeOpt
from secty.srvc import SrvcOpt
import json
import copy


#
if __name__ == '__main__':
#    print node_moni()
    
    
    no = NodeOpt()
    no.moni()
    
    
#    
#    so = SrvcOpt()
#    ss = so.select(where={'ip':'192.168.1.201', 'type':'cds'})
#    print json.dumps(ss)
    
#    mds_list = []
#    cds_list = []
#    c60_list = []
#    
#    for s in ss:
#        status = s['status']
#        pid = s['pid']
#        ip = s['sid']['Element1']
#        type = s['sid']['Element2']
#        no = s['sid']['Element3']
#        print '-----------------------------------'
#        
#        if type == 'mds':
#            mds_list.append(s)
#        elif type == 'cds':
#            cds_list.append(s)
#        elif type == 'c60':
#            c60_list.append(s)
#            
#    
#    print mds_list
#    print '======================================='
#    print cds_list
#    print '======================================='
#    print c60_list
    
    
#    node_moni()
#    ro = RackOpt()
#    rack_list = ro.list()
#    for r in rack_list:
#        print r['id']
#        print r['name']
#        print '****************************************'
#    
#    print ro.insert({"name":"rack1"})
#    
#    print ro.select()
#
#    no = NodeOpt()
#    print no.select(where={'ip':'192.168.1.201'})
#    print len(no.moni())
        
#    print no.insert({ "ip":"192.168.1.204", "hostname":"node4", "user":"root", "passwd":"root", "rack":"1" })
#    print no.delete(where = {"id":3})
#    print no.select(limit={'offset':0, 'count':2})
#    print no.moni(limit={'offset':0, 'count':2})
#    print json.dumps(no.moni(1)[0]['info']['mem'])
    
#    rst = no.moni(1)
#    for n in nodes:
#            print n['id']
#            print n['ip']
#            print n['hostname']
#            print n['rack']
#            print n['user']
#            print n['password']
#            print n['time']
#            print n['status']
#            
#            print n['info']['uptime']['user']
#            print n['info']['uptime']['time']
#            
#            for c in n['info']['cpu']['spec']:
#                print c['physical_id']
#                print c['model_name']
#                print c['count']
#            
#            print n['info']['cpu']['used']
#            print n['info']['cpu']['load']['avg1']
#            print n['info']['cpu']['load']['avg5']
#            print n['info']['cpu']['load']['avg15']
#            
#            print n['info']['mem']['mem_total']
#            print n['info']['mem']['mem_free']
#            print n['info']['mem']['mem_buffers']
#            print n['info']['mem']['mem_cached']
#            print n['info']['mem']['swap_total']
#            print n['info']['mem']['swap_free']
#            
#            print n['info']['netflow']
#            
#            print n['info']['disk']

#    nodes = []
#    for n in rst:
#        node = {}
#        node['id'] = n['id'] if n['id'] else None
#        node['ip'] = n['ip'] if n['ip'] else None
#        node['hostname'] = n['hostname'] if n['hostname'] else None
#        node['rack'] = n['rack']
#        node['user'] = n['user']
#        node['password'] = n['password']
#        node['time'] = n['time']
#        node['status'] = n['status']
#        
#        node['uptime_user'] = n['info']['uptime']['user']
#        node['uptime_time'] = n['info']['uptime']['time']
#        
#        node['cpu'] = n['info']['cpu']['spec']
#        
##            for c in n['info']['cpu']['spec']:
##                print c['physical_id']
##                print c['model_name']
##                print c['count']
#        
#        node['cpu_used'] = n['info']['cpu']['used']
#        node['cpu_avg1'] = n['info']['cpu']['load']['avg1']
#        node['cpu_avg5'] = n['info']['cpu']['load']['avg5']
#        node['cpu_avg15'] = n['info']['cpu']['load']['avg15']
#        
#        node['mem_total'] = n['info']['mem']['mem_total']
#        node['mem_free'] = n['info']['mem']['mem_free']
#        node['mem_buffers'] = n['info']['mem']['mem_buffers']
#        node['mem_cached'] = n['info']['mem']['mem_cached']
#        node['swap_total'] = n['info']['mem']['swap_total']
#        node['swap_free'] = n['info']['mem']['swap_free']
#        
#        node['netflow'] = n['info']['netflow']
#        
#        node['disk'] = n['info']['disk']
#        
#        node['disk_total'] = 0
#        node['disk_free'] = 0
#        for d in node['disk']:
#            node['disk_total'] += d['capacity'] 
#            node['disk_free'] += d['avail']
#        
#        print '********************************'
#        print node['disk_total']
#        print node['disk_free']
#        print '********************************'
#        
#        nodes.append(node)
        
#        print node
#        print nodes

#    print no.cpu(1)
#    print no.load(1)
#    print no.cpu_used(1)
#    print no.mem(1)
#    print no.nw(1)
#    print no.drv(1)
    
#    print type(rst)
    
#    for m in rst: 
#        print m['status']
#        print json.dumps(m['info']['cpu'])
#        print json.dumps(m['info']['mem'])
        
##############################################################
#class A(object):
#    def __init__(self, some_name='A'):
#        self.some_name = some_name
#    
#    def print_a(self):
#        print self.some_name
#        
#class B(object):
#    def __init__(self, some_name='B'):
#        self.some_name = some_name
#        
#    def print_b(self):
#        print self.some_name
#        
#class C(A, B):
#    def __init__(self):
#        A.__init__(self, some_name='AAAAA')
#        B.__init__(self, some_name='BBBBB')
#        
#if __name__ == '__main__':
#    c = C()
#    c.print_a()


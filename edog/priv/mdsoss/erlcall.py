'''
Created on Mar 30, 2010

@author: root
'''
from django.conf import settings
settings.configure()


from oss.models import *
#from mdsoss.oss.models import PM, VM, Customer, Virtual_Disk
import subprocess
import xml.etree.ElementTree as ET

def getInfo():
    try:
        xmlTree = ET.parse('info.xml')
        print("This is a good xml doc")

    except Exception, e:
        print("This may be a bad doc")
        print("Error:",e)

#    customer_list = []
#    virtualDisk_list = []
#    pm_list = []
#    vm_list = []

#    bridge_list = []
#    virtualNetworkCard_list = []
#    stdVmImg_list = []

    for table in xmlTree.findall('/table'):

        if 'cust_t' == table.get('name'):
            for cust in table.getchildren():
                customer = Customer()

                customer.uuid = cust.attrib['id']
                customer.company = cust.attrib['name']

                #customer.save()

        elif 'disk_t' == table.get('name'):
            for disk in table.getchildren():
                virtualDisk = VirtualDisk()

                virtualDisk.uuid = disk.attrib['id']
                virtualDisk.path = disk.attrib['path']
                virtualDisk.target = disk.attrib['target']
                virtualDisk.capacity = disk.attrib['size']

                virtualDisk.vm_uuid = disk.attrib['vmid']
                virtualDisk.customer_uuid = disk.attrib['custid']

                if 'false' == disk.attrib['canboot']:
                    virtualDisk.is_vm_img = False
                elif 'true' == disk.attrib['canboot']:
                    virtualDisk.is_vm_img = True

                #virtualDisk.save()
                print('virtualDisk.customer_uuid:' + virtualDisk.customer_uuid)

        elif 'pm_t' == table.get('name'):
            for pmpm in table.getchildren():
                pm = PM()

                pm.uuid = pmpm.attrib['pmid']
                pm.ip = pmpm.attrib['ip']

                if 'false' == pmpm.attrib['running']:
                    pm.is_running = False
                elif 'true' == pmpm.attrib['running']:
                    pm.is_running = True

                cpu_elem = pmpm.find('cpu')
                pm.cpu_cores = cpu_elem.attrib['number']

                mem_elem = pmpm.find('memory')
                pm.mem_capacity = mem_elem.attrib['total']
                pm.mem_used = mem_elem.attrib['used']

                bridge_elem = pmpm.find('bridges')
                #bridge_list = []
                for brid in bridge_elem.getchildren():
                    bridge = Bridge()

                    bridge.name = brid.attrib['name']
                    bridge.ip = brid.attrib['ip']
                    bridge.mask = brid.attrib['mask']
                    bridge.mac = brid.attrib['mac']
                    bridge.pm_uuid = pm.uuid

                    #bridge_list.append(bridge)
                    #bridge.save()

                #pm_list.append(pm)
                #pm.save()

        elif 'vm_t' == table.get('name'):
            for vmvm in table.getchildren():
                vm = PM()

                vm.uuid = vmvm.attrib['vmid']
                vm.alias = vmvm.attrib['name']

                if 'false' == vmvm.attrib['running']:
                    vm.is_running = False
                elif 'true' == vmvm.attrib['running']:
                    vm.is_running = True

                cpu_elem = vmvm.find('cpu')
                vm.cpu_cores = cpu_elem.attrib['number']
                mem_elem = vmvm.find('memory')
                vm.mem_capacity = mem_elem.attrib['size']
                boot_disk_elem = vmvm.find('boot')
                vm.boot_disk_uuid = boot_disk_elem.attrib['disk']

                vm.pm_uuid =vmvm.attrib['pmid']
                vm.customer_uuid = vmvm.attrib['custid']

                virtualNetworkCard_elem = vmvm.find('vmpcis')
                #bridge_list = []
                for vmpci in virtualNetworkCard_elem.getchildren():
                    virtualNetworkCard = VirtualNetworkCard()

                    virtualNetworkCard.bridge_mac = vmpci.attrib['bridge_mac']
                    virtualNetworkCard.mac = vmpci.attrib['mac']
                    virtualNetworkCard.vm_uuid = vm.uuid

                    #bridge_list.append(bridge)
                    #virtualNetworkCard.save()

                #vm.save()
                print('vm.customer_uuid:' + vm.customer_uuid)
        elif 'stddisk_t' == table.get('name'):
            for stddisk in table.getchildren():
                stdVmImg = StdVmImg()

                stdVmImg.uuid = stddisk.attrib['id']
                stdVmImg.os_release = stddisk.attrib['os_type']
                stdVmImg.os_version = stddisk.attrib['os_version']
                stdVmImg.path = stddisk.attrib['path']
                stdVmImg.capacity = stddisk.attrib['size']

                #stdVmImg.save()


getInfo()

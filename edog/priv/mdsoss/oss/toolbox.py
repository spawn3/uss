from mdsoss.oss.models import *
import xml.etree.ElementTree as ET
from erlapi import *

########################################################################################
def getInfo():
    info = api_get_request('/tables')
    #f = file("info.xml", "w+")
    #f.write(info)
    #f.close

    #info = ""
    #with open("tables.xml") as f:
        #for line in f:
            #info += line
    #print info

    try:
        xmlTree = ET.fromstring(info)
        print("This is a good xml doc")

        for table in xmlTree.findall('table'):
            print table.get("name")
            if 'cust_t' == table.get('name'):
                for cust in table.getchildren():
                    customer = Customer()
                    customer.uuid = cust.attrib['id']
                    customer.alias = cust.attrib['name']
                    customer.company = cust.attrib['company']
                    customer.address = cust.attrib['address']
                    customer.contact_person = cust.attrib['contact']
                    customer.telephone = cust.attrib['telephone']
                    customer.cellphone = cust.attrib['cellphone']
                    customer.email = cust.attrib['email']

                    status = cust.find('status')
                    if 'true' == status.attrib['locked']:
                        customer.locked = True
                    elif 'false' == status.attrib['locked']:
                        customer.locked = False

                    customer.locktime = status.attrib['locktime']

                    customer.save()

            elif "disk_vm_t" == table.get("name"):
                print table.getchildren()
                for disk_vm in table.getchildren():
                    diskvm = DiskVm()
                    diskvm.disk_id = disk_vm.attrib["disk_id"]
                    diskvm.vm_id = disk_vm.attrib["vm_id"]
                    diskvm.target = disk_vm.attrib["target"]
                    diskvm.save()

            elif 'disk_t' == table.get('name'):
                for disk in table.getchildren():
                    virtualDisk = VirtualDisk()

                    virtualDisk.uuid = disk.attrib['id']
                    virtualDisk.path = disk.attrib['path']
                    virtualDisk.target = disk.attrib['target']
                    virtualDisk.alias = disk.attrib['alias']
                    virtualDisk.shared = disk.attrib['shared']
                    virtualDisk.capacity = float(disk.attrib['size'])

                    virtualDisk.vm_id = disk.attrib['vmid']
                    virtualDisk.customer_id = disk.attrib['custid']

                    if 'false' == disk.attrib['canboot']:
                        virtualDisk.is_vmimg = False
                    elif 'true' == disk.attrib['canboot']:
                        virtualDisk.is_vmimg = True

                    if 'locked' == disk.attrib['status']:
                        virtualDisk.status = 'locked'
                    elif 'created' == disk.attrib['status']:
                        virtualDisk.status = 'unlocked'
                    elif 'creating' == disk.attrib['status']:
                        virtualDisk.stauts = 'creating'

                    virtualDisk.locktime = disk.attrib['locktime']

                    virtualDisk.save()

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

                    mem_elem        = pmpm.find('memory')
                    pm.mem_capacity = int(mem_elem.attrib['total'])/(1024*1024)
                    pm.mem_used     = int(mem_elem.attrib['used'])/(1024*1024)

                    pm.cpu_alloced = int(pmpm.attrib['alloc_cpu'])
                    pm.mem_alloced = int(pmpm.attrib['alloc_mem']) / (1024*1024)

                    bridge_elem = pmpm.find('bridges')
                    #bridge_list = []
                    for brid in bridge_elem.getchildren():
                        bridge = Bridge()

                        bridge.name  = brid.attrib['name']
                        bridge.ip    = brid.attrib['ip']
                        bridge.mask  = brid.attrib['mask']
                        bridge.mac   = brid.attrib['mac']
                        bridge.pm_id = pm.uuid

                        #bridge_list.append(bridge)
                        bridge.save()

                    #pm_list.append(pm)
                    pm.save()

            elif 'vm_t' == table.get('name'):
                for vmvm in table.getchildren():
                    vm = VM()

                    vm.uuid     = vmvm.attrib['vmid']
                    vm.alias    = vmvm.attrib['name']
                    vm.iodriver = vmvm.attrib['iodriver']

                    vm.status = vmvm.attrib['status']
                    vm.port = vmvm.attrib['port']

                    cpu_elem = vmvm.find('cpu')
                    vm.cpu_cores = cpu_elem.attrib['number']
                    mem_elem = vmvm.find('memory')
                    vm.mem_capacity = int(mem_elem.attrib['size'])/(1024*1024)
                    boot_disk_elem = vmvm.find('boot')
                    vm.boot_disk_id = boot_disk_elem.attrib['disk']

                    vm.pm_id =vmvm.attrib['pmid']
                    vm.customer_id = vmvm.attrib['custid']

                    vm.cpu_util = float(vmvm.attrib['cpu_util'])
                    vm.mem_util = float(vmvm.attrib['mem_util'])

                    vm.mem_real = int(vmvm.attrib['mem_real']) / (1024*1024)

                    virtualNetworkCard_elem = vmvm.find('vmpcis')
                    #bridge_list = []
                    for vmpci in virtualNetworkCard_elem.getchildren():
                        virtualNetworkCard = VirtualNetworkCard()

                        virtualNetworkCard.model = vmpci.attrib['model']
                        virtualNetworkCard.bridge = vmpci.attrib['bridge']
                        virtualNetworkCard.mac = vmpci.attrib['mac']
                        virtualNetworkCard.vm_id = vm.uuid

                        #bridge_list.append(bridge)
                        virtualNetworkCard.save()

                    vm.save()

            elif 'stddisk_t' == table.get('name'):
                for stddisk in table.getchildren():
                    stdVmImg = StdVmImg()

                    stdVmImg.uuid = stddisk.attrib['id']
                    stdVmImg.os_release = stddisk.attrib['os_type']
                    stdVmImg.os_version = stddisk.attrib['os_version']
                    stdVmImg.path = stddisk.attrib['path']
                    stdVmImg.capacity = float(stddisk.attrib['size'])

                    stdVmImg.save()


            elif 'yfs_t' == table.get('name'):

                for yfsm_elem  in table.getchildren():

                    yfs=Yfs()

                    yfs.name = yfsm_elem.attrib['name']
                    yfs.ip = yfsm_elem.attrib['ip']
                    yfs.num = yfsm_elem.attrib['num']
                    yfs.status = yfsm_elem.attrib['status']
                    yfs.save()
                    pid_elem = yfsm_elem.find('pids')
                    for pidm in pid_elem.getchildren():
                        pid = Pids()

                        pid.pid = pidm.attrib['id']
                        pid.cmd = pidm.attrib['cmd']
                        pid.yfs=yfs
                        pid.save()
#            elif 'option_t' ==table.get('name'):
#                for option_elem in table.getchildren():
#                    option = Option()
#
#                    option.key = option_elem.attrib['key']
#                    option.value = option_elem.attrib['value']

#                    option.save()

        return True
    except Exception, e:
        print("This may be a bad doc")
        print("Error:",e)
        return False


def urlDecode(str):
    return str.replace('%2B','+').replace('%2F','/')\
              .replace('%20',' ').replace('%3F','?')\
              .replace('%25','%').replace('%23','#')\
              .replace('%26','&').replace('%3D','=')

def urlEncode(str):
    return str.replace('+','%2B').replace('/','%2F')\
              .replace(' ','%20').replace('?','%3F')\
              .replace('%','%25').replace('#','%23')\
              .replace('&','%26').replace('=','%3D')

def line_count(filename):
    '''Count file's lines neglect '\n' '''
    count=0
    #print filename
    for line in open(filename):
        if(line!='\n'):count+=1
    return count
###################################END##################################

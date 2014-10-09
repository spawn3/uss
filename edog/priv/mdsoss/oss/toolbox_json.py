import xml.etree.ElementTree as ET
from erlapi import *

def get_url(method):
    return '/uss/edog_web/%s' % method

def action(Action, host=api_web_host, port=api_web_port):
    #url = get_url('actions')
    url = '/actions'

    d = {}
    d['action'] = Action
    info = request_json(url, d, host, port)
    return url, info


##############################################################################################
def customer_create(alias, company, address, contact_person, telephone, cellphone, email):
    url = '/users'
    d = {}
    d['alias']          = alias
    d['company']        = company
    d['address']        = address
    d['contact_person'] = contact_person
    d['telephone']      = telephone
    d['cellphone']      = cellphone
    d['email']          = email

    info = request_json(url, d)
    return url, info

def customer_update(uuid, alias, company, address, contact_person, telephone, cellphone, email):
    url = '/users/%s' % uuid
    d = {}
    #d['uuid']           = uuid
    ##
    d['action']         = 'update'
    d['alias']          = alias
    d['company']        = company
    d['address']        = address
    d['contact_person'] = contact_person
    d['telephone']      = telephone
    d['cellphone']      = cellphone
    d['email']          = email

    info = request_json(url, d)
    return url, info

def customer_lock(customer_uuid):
    # url = get_url('customer_lock')
    url = '/users/%s' % customer_uuid
    d = { 'uuid': customer_uuid, 'action': 'lock' }
    info = request_json(url, d)
    return url, info

def customer_unlock(customer_uuid):
    # url = get_url('customer_unlock')
    url = '/users/%s' % customer_uuid
    d = { 'uuid': customer_uuid, 'action': 'unlock' }
    info = request_json(url, d)
    return url, info

##############################################################################################
def virtualDisk_create(customer_uuid, alias, capacity, shared):
    #url = get_url('vd_create')
    print shared, shared.lower(), shared.lower() == "true"
    url = '/disks'
    d = {
        'custid': customer_uuid,
        'alias': alias,
        'capacity': capacity,
        'shared': shared.lower() == "true"
    }
    info = request_json(url, d)
    return url, info

def virtualDisk_lock(virtualDisk_uuid):
    #url = get_url('vd_lock')
    url = '/disks/%s' % virtualDisk_uuid
    d = { 'uuid': virtualDisk_uuid, 'action': 'lock' }
    info = request_json(url, d)
    return url, info

def virtualDisk_unlock(virtualDisk_uuid):
    #url = get_url('vd_unlock')
    url = '/disks/%s' % virtualDisk_uuid
    d = {'uuid': virtualDisk_uuid, 'action': 'unlock' }
    info = request_json(url, d)
    return url, info

##############################################################################################
def vm_create(customer_uuid, vm_name, cpu_cores, mem_capacity, pcis,
        vmImg_uuid, virtualDisk_set, iodriver ):
    # erl_call -name ussadmin_master@ecloud.org -c ussadmin -a 'edog_master vm_create
    # ["fe888f92-49e7-4d21-867c-cf7e8223a33f", "192.168.1.51", "zkzvm1", 2, 1000000,
    # ["virbr0", "virbr0"], "cbe99a12-194e-4342-82cc-797b090eff17", ["ff4044ab-8e90-44c0-8732-e5a17b0e5660"]]'

    #url = get_url('vm_create')
    url = '/vms'
    d = {}
    d['custid']    = customer_uuid
    d['vmname']    = vm_name
    d['cpu']       = cpu_cores
    d['memory']    = mem_capacity
    d['iodriver']  = iodriver
    d['pcis']      = pcis
    d['boot_disk'] = vmImg_uuid
    d['vdisks']    = virtualDisk_set
    info = request_json(url, d)
    return url, info

def vm_edit(vm_uuid, vm_name, cpu_cores, mem_capacity, pcis, vmImg_uuid,
        virtualDisk_set, iodriver):
    #url = get_url('vm_update')
    url = '/vms/%s' % vm_uuid

    d = {}
    d['action']    = 'update'
    d['vmid']      = vm_uuid
    d['vmname']    = vm_name
    d['cpu']       = cpu_cores
    d['memory']    = mem_capacity
    d['iodriver']  = iodriver
    d['pcis']      = pcis
    d['boot_disk'] = vmImg_uuid
    d['vdisks']    = virtualDisk_set

    info = request_json(url, d)
    return url, info

def vm_destroy(vm_uuid):
    #url = get_url('vm_destroy')
    url = '/vms/%s' % vm_uuid

    d = {'vmid': vm_uuid}
    d['action'] = 'destroy'
    info = request_json(url, d)
    return url, info

def vm_start(vm_uuid, pm_uuid):
    #url = get_url('vm_start')
    url = '/vms/%s' % vm_uuid

    d = {}
    d['vmid'] = vm_uuid
    d['action']    = 'start'
    if pm_uuid:
        d['pmid'] = pm_uuid
    info = request_json(url, d)
    return url, info

def vm_shutdown(vm_uuid):
    #url = get_url('vm_shutdown')
    url = '/vms/%s' % vm_uuid

    d = {'vmid': vm_uuid}
    d['action']    = 'stop'
    d['method']    = 'shutdown'
    info = request_json(url, d)
    return url, info

def vm_stop(vm_uuid):
    #url = get_url('vm_stop')
    url = '/vms/%s' % vm_uuid

    d = {'vmid': vm_uuid}
    d['action']    = 'stop'
    d['method']    = 'destroy'
    info = request_json(url, d)
    return url, info

def vm_pause(vm_uuid):
    #url = get_url('vm_pause')
    url = '/vms/%s' % vm_uuid

    d = {'vmid': vm_uuid}
    d['action']    = 'pause'
    info = request_json(url, d)
    return url, info

def vm_resume(vm_uuid):
    #url = get_url('vm_resume')
    url = '/vms/%s' % vm_uuid
    d = {'vmid': vm_uuid}
    d['action']    = 'resume'
    info = request_json(url, d)
    return url, info

def vm_migrate(vm_uuid, pm_uuid):
    #url = get_url('vm_migrate')
    url = '/vms/%s' % vm_uuid

    d = {}
    d['vmid'] = vm_uuid
    d['action']    = 'migrate'
    if pm_uuid:
        d['pmid'] = pm_uuid
    info = request_json(url, d)
    return url, info

########################################################################################
def getInfo():
    try:
        #info = api_get_request('/uss/edog_web/table_dumpxml')
        info = api_get_request('/tables')
        #f = file("info.xml", "w+")
        #f.write(info)
        #f.close()

        xmlTree = ET.fromstring(info)
        print("This is a good xml doc")

        for table in xmlTree.findall('table'):
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

            elif 'disk_t' == table.get('name'):
                for disk in table.getchildren():
                    virtualDisk = VirtualDisk()

                    virtualDisk.uuid = disk.attrib['id']
                    virtualDisk.path = disk.attrib['path']
                    virtualDisk.target = disk.attrib['target']
                    virtualDisk.alias = disk.attrib['alias']
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

                    mem_elem = pmpm.find('memory')
                    pm.mem_capacity = int(mem_elem.attrib['total'])/(1024*1024)
                    pm.mem_used = int(mem_elem.attrib['used'])/(1024*1024)

                    bridge_elem = pmpm.find('bridges')
                    #bridge_list = []
                    for brid in bridge_elem.getchildren():
                        bridge = Bridge()

                        bridge.name = brid.attrib['name']
                        bridge.ip = brid.attrib['ip']
                        bridge.mask = brid.attrib['mask']
                        bridge.mac = brid.attrib['mac']
                        bridge.pm_id = pm.uuid

                        #bridge_list.append(bridge)
                        bridge.save()

                    #pm_list.append(pm)
                    pm.save()

            elif 'vm_t' == table.get('name'):
                for vmvm in table.getchildren():
                    vm = VM()

                    vm.uuid = vmvm.attrib['vmid']
                    vm.alias = vmvm.attrib['name']

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

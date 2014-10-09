# Create your views here.
# -*- coding: UTF-8 -*-
import cStringIO
import re
import time
import os
import linecache
import monitorBox
import toolbox
import toolbox_json
import checker



from django.template import loader, Context
from django.http import HttpResponse, HttpResponseRedirect
from django.shortcuts import render_to_response
from mdsoss.oss.models import *
import settings
from django.contrib.auth.models import Group,Permission,User
from django.contrib.auth import authenticate, login,logout
from django.contrib.auth.decorators import permission_required
from django.core.paginator import Paginator, InvalidPage, EmptyPage

base_dir = os.path.dirname(__file__)
response_log_path = './response_log.txt'
action_succ_resp_msg = u"""
                        <html>
                         <head>
                         <script type="text/javascript">
                         function ok() {
                             parent.location.reload();
                             //window.close();
                         }
                         </script>
                         <head>
                         <body>
                         <!--
                         发送%s命令成功!
                         -->
                         <br/>
                         执行结果:%s
                         <button type="button" onclick="ok();">确认</button>
                         </body>
                         </html>
                       """

action_fail_resp_msg = u"""
                        <html>
                         <head>
                         <script type="text/javascript">
                         function ok() {
                             parent.location.reload();
                             //window.close();
                         }
                         </script>
                         <head>
                         <body>
                         <!--
                         发送%s命令成功!
                         <br>
                         -->
                         执行结果:
                         <p>%s</p>
                         <!--
                         <p>%s</p>
                         -->
                         <button type="button" onclick="ok();">确认</button>
                         </body>
                         </html>
                       """

action_succ_done_resp_msg = u"""
                        <html>
                         <head>
                         <script type="text/javascript">
                         function ok() {
                             parent.location.reload();
                             window.close();
                         }
                         </script>
                         <head>
                         <body>
                         虚拟机:%s ID:%s 编辑成功!
                         <button type="button" onclick="ok();">确认</button>
                         </body>
                         </html>
                       """
action_fail_done_resp_msg = u"""
                        <html>
                         <head>
                         <script type="text/javascript">
                         function ok() {
                             parent.location.reload();
                             window.close();
                         }
                         </script>
                         <head>
                         <body>
                         虚拟机:%s ID:%s 编辑失败!
                         <button type="button" onclick="ok();">确认</button>
                         </body>
                         </html>
                       """

def is_ok(info):
    if 'ok' in info:
        return True
    return False

def validateUser(func):
    def Fun(request):
        if not request.user.is_authenticated():
            return render_to_response('user_login.html')
        else:
            return func(request)
    return Fun

@validateUser
def frame(request):
    return render_to_response('frame.html')

def header(request):
    user=User.objects.get(username=request.user.username)
    return render_to_response('header.html',{'user':user})

def favicon(request):
    return HttpResponseRedirect("/static/image/favicon.ico")

def home(request):
    return render_to_response('home.html')

def events_scrolling(request):
    return render_to_response('events_scrolling.html')

def events_msg(request):
    style_succ  = '<div style="border:1px solid #00AA00">%s</div><br />'
    style_fail  = '<div style="border:1px solid #FF0302">%s</div><br />'
    style_event = '<div style="border:1px solid blue">%s</div><br />'
    style_doing = '<div style="border:1px solid yellow"><img src="../static/wait.gif" >%s</div><br />'

    lines_ = ''
    try:
        with open(response_log_path) as f:
            lines_ = f.readlines()
    except IOError, e:
        if 'No such file or directory' not in e:
            raise IOError,e
    msg = ''
    if not lines_:
        return HttpResponse('welcome')

    lines = lines_[::-1]
    count = len(lines)
    if count > 8:
        count = 8
    msg = ''
    for i in range(0, count):
        line = lines[i]
        line_msg = ''
        if line.find(u'成功'.encode('utf8')) > -1:
            line_msg = (style_succ % line)
        elif line.find(u'失败'.encode('utf8')) > -1:
            line_msg = (style_fail % line)
        else:
            line_msg = (style_event % line)
        msg += line_msg
    return HttpResponse(msg)

def getMonitorInfo(request):
    result = monitorBox.start_monitor()
    return HttpResponse(result)

def monitorPngs(request):
    pm_uuid = request.GET['pm_uuid']
    print 'monitorPngs:', pm_uuid
    pm = PM.objects.get(uuid=request.GET['pm_uuid'])
    t = loader.get_template('pm_detail_rrd.html')
    flag = time.time()
    c = Context({'pm':pm, 'flag':flag})
    return HttpResponse(t.render(c))

def test(request):
    return render_to_response('test.html')

def test_js(request):
    return render_to_response('test_js.html')

def test_validate(request):
    return render_to_response('test_validate.html')

def global_view(request):
    return render_to_response('global_view.html')

def getGlobalInfo(request):
    if not updateGlobalInfo():
        return HttpResponse(u'获取更新信息失败！')

    return HttpResponse("""
                        <html>
                        <head>
                            <script language='Javascript'>
                            function go(){
                                window.location=\"../home\";
                            }
                            setTimeout(\"go()\",800);
                            </script>
                        </head>
                        <body>
                            <p>更新全局信息成功!</p>
                        <body>
                        </html>
                        """
                        )

def updateGlobalInfo():
    PM.objects.all().delete()
    VM.objects.all().delete()
    Customer.objects.all().delete()
    VirtualDisk.objects.all().delete()
    VirtualNetworkCard.objects.all().delete()
    Bridge.objects.all().delete()
    StdVmImg.objects.all().delete()
    Yfs.objects.all().delete()
    Pids.objects.all().delete()
    Option.objects.all().delete()
    DiskVm.objects.all().delete()
    return toolbox.getInfo()

def recycle_bin(request):
    customer_set = Customer.objects.filter(locked=True)
    virtualDisk_set = VirtualDisk.objects.filter(status='locked')
    t = loader.get_template('recycle_bin.html')
    c = Context({'customer_set' : customer_set, 'virtualDisk_set' : virtualDisk_set})
    return HttpResponse(t.render(c))

def stdVmImg_list(request):
    stdVmImg_set = StdVmImg.objects.all()
    t = loader.get_template('stdVmImg_list.html')
    c = Context({'stdVmImg_set':stdVmImg_set })
    return HttpResponse(t.render(c))

def stdVmImg_detail(request):
    stdVmImg = StdVmImg.objects.get(uuid=request.GET['stdVmImg_uuid'])
    t = loader.get_template('stdVmImg_detail.html')
    c = Context({'stdVmImg':stdVmImg})
    return HttpResponse(t.render(c))
@validateUser
def pm_list(request):
    pm_set = PM.objects.all()
    t = loader.get_template('pm_list.html')
    c = Context({'pm_set' : pm_set})
    return HttpResponse(t.render(c))

@permission_required('oss.manage_pm',login_url='user_noPermission')
def pm_detail(request):
    pm = PM.objects.get(uuid=request.GET['pm_uuid'])
    vm_set = VM.objects.filter(pm=pm)
    bridge_set = Bridge.objects.filter(pm=pm)
    monitor_set = Monitor.objects.filter(pm=pm)
    t = loader.get_template('pm_detail.html')
    c = Context({'pm':pm, 'vm_set':vm_set, 'bridge_set':bridge_set, 'monitor_set':monitor_set})
    return HttpResponse(t.render(c))

@validateUser
def vm_list(request):
    customer_set = Customer.objects.filter(locked=False)
    vm_set = VM.objects.all()
    vm_normal_set = []
    for vm in vm_set:
        if (vm.customer in customer_set):
            vm_normal_set.append(vm)
    t = loader.get_template('vm_list.html')
    c = Context({'vm_set' : vm_normal_set})
    return HttpResponse(t.render(c))

@permission_required('oss.manage_vm',login_url='user_noPermission')
def vm_detail(request):
    vm_uuid = request.GET['vm_uuid']
    vm = VM.objects.get(uuid=vm_uuid)
    virtualNetworkCard_set = VirtualNetworkCard.objects.filter(vm=vm)

    disk_ids = []
    for i in  DiskVm.objects.filter(vm_id=vm_uuid):
        disk_ids.append(i.disk_id)
    virtualDisk_set = VirtualDisk.objects.filter(uuid__in=disk_ids).filter(status='unlocked')
    virtualDisk_set = update_virtualDisk_set(virtualDisk_set)
    virtualDisk_set = updata_virtualDisk_set_target(virtualDisk_set, vm_uuid)

    t = loader.get_template('vm_detail.html')
    c = Context({'vm':vm, 'virtualNetworkCard_set':virtualNetworkCard_set, 'virtualDisk_set':virtualDisk_set})
    return HttpResponse(t.render(c))

@validateUser
def customer_list(request):
    customer_set = Customer.objects.filter(locked=False)
    t = loader.get_template('customer_list.html')
    c = Context({'customer_set' : customer_set})
    return HttpResponse(t.render(c))

@permission_required('oss.manage_customer',login_url='user_noPermission')
def customer_detail(request):
    updateGlobalInfo()
    customer = Customer.objects.get(uuid=request.GET['customer_uuid'])
    vm_set = VM.objects.filter(customer=customer)
    virtualDisk_set = VirtualDisk.objects.filter(customer=customer).filter(status='unlocked')
    virtualDisk_set = update_virtualDisk_set(virtualDisk_set)
    t = loader.get_template('customer_detail.html')
    c = Context({'customer':customer, 'vm_set':vm_set, 'virtualDisk_set':virtualDisk_set})
    return HttpResponse(t.render(c))

def updata_virtualDisk_set_target(virtualDisk_set, vm_uuid):
    '''设置disk在针对vm_uuid上的target,比如vda '''
    print virtualDisk_set
    if virtualDisk_set:
        for disk in virtualDisk_set:
            diskvms = DiskVm.objects.get(disk_id=disk.uuid, vm_id=vm_uuid)
            if disk.vm:
                disk.target = diskvms.target
    return virtualDisk_set

def update_virtualDisk_set(virtualDisk_set):
    '''
        设置disk被几个虚拟机使用, virtualDisk_set 可以是django查询出来的
        virtualDisk_set = VirtualDisk.objects.filter(status='unlocked')
        或
        virtualDisk_set = [VirtualDisk.objects.get(uuid=disk_uuid)]
    '''
    print virtualDisk_set
    if virtualDisk_set:
        for disk in virtualDisk_set:
            diskvms = DiskVm.objects.filter(disk_id=disk.uuid)
            vms = []
            for i in diskvms:
                vm = VM.objects.get(uuid=i.vm_id)
                vms.append(vm.alias)
            print vms
            disk.vm = len(vms) #为了在页面上显示vm的个数而不是具体的vm_name
    return virtualDisk_set


@validateUser
def virtualDisk_list(request):
    virtualDisk_set = VirtualDisk.objects.filter(status='unlocked')
    virtualDisk_set = update_virtualDisk_set(virtualDisk_set)
    t = loader.get_template('virtualDisk_list.html')
    c = Context({'virtualDisk_set' : virtualDisk_set})
    return HttpResponse(t.render(c))

@permission_required('oss.manage_vd',login_url='user_noPermission')
def virtualDisk_detail(request):
    disk_uuid = request.GET['virtualDisk_uuid']
    virtualDisk = VirtualDisk.objects.get(uuid=disk_uuid)
    virtualDisk = update_virtualDisk_set([virtualDisk])[0]
    #diskvms用来在页面上展现磁盘在不同的vm上的target,所以把i.vm_id改为vm.alias
    diskvms = DiskVm.objects.filter(disk_id=disk_uuid)
    for i in diskvms:
        vm = VM.objects.get(uuid=i.vm_id)
        i.vm_id = vm.alias
    t = loader.get_template('virtualDisk_detail.html')
    c = Context({'virtualDisk':virtualDisk, 'diskvms':diskvms})
    return HttpResponse(t.render(c))


def virtualDisk_action(request):
    customer_uuid = request.GET['customer_uuid']
    if 'virtualDisk_create' == request.GET['action_type']:
        t = loader.get_template('virtualDisk_create.html')
        c = Context({'customer_uuid':customer_uuid})
        return HttpResponse(t.render(c))

@permission_required('oss.manage_vd',login_url='user_noPermission')
def virtualDisk_create(request):
    customer_uuid = request.POST['customer_uuid']
    alias = request.POST['alias']
    capacity = request.POST['capacity']
    shared = "false"
    if "shared" in request.POST and request.POST['shared'] == "on":
        shared = "true"
    erl_call_command, info = toolbox_json.virtualDisk_create(customer_uuid, alias, capacity, shared)
    if is_ok(info):
        if not updateGlobalInfo():
            return HttpResponse(u'获取更新信息失败！')
        return HttpResponse(action_succ_resp_msg % (u'虚拟磁盘创建', info))
    else:
        return HttpResponse(action_fail_resp_msg % (u'虚拟磁盘创建', info, erl_call_command))

@permission_required('oss.manage_customer',login_url='user_noPermission')
def customer_action(request):
    if 'customer_create' == request.GET['action_type']:
        return render_to_response('customer_create.html')
    if 'customer_update' == request.GET['action_type']:
        customer_uuid == request.GET['customer_selected']
        return render_to_response()

@permission_required('oss.manage_customer',login_url='user_noPermission')
def customer_create(request):
    customer = Customer()

    customer.alias = request.POST['alias']
    customer.company = request.POST['company']
    customer.address = request.POST['address']
    customer.telephone = request.POST['telephone']
    customer.contact_person = request.POST['contact_person']
    customer.cellphone = request.POST['cellphone']
    customer.email = request.POST['email']

    erl_call_command, info = toolbox_json.customer_create(
            customer.alias, customer.company, customer.address,
            customer.contact_person,
            customer.telephone, customer.cellphone, customer.email)
    if is_ok(info):
        customer.uuid = info['ok']
        customer.save()

        if not updateGlobalInfo():
            return HttpResponse(u'获取更新信息失败')

        return HttpResponse(action_succ_resp_msg % (u'创建用户', info))
    else:
        return HttpResponse(action_fail_resp_msg % (u'创建用户', info, erl_call_command))

@permission_required('oss.manage_customer',login_url='user_noPermission')
def get_customer_update(request):
    customer_uuid = request.GET['customer_uuid']
    print customer_uuid
    customer = Customer.objects.get(uuid = customer_uuid)
    t = loader.get_template('customer_update.html')
    c = Context({'customer': customer})
    return HttpResponse(t.render(c))

@permission_required('oss.manage_customer',login_url='user_noPermission')
def customer_update(request):
    customer = Customer()
    customer.uuid = request.POST['uuid']
    customer.alias = request.POST['alias']
    customer.company = request.POST['company']
    customer.address = request.POST['address']
    customer.telephone = request.POST['telephone']
    customer.contact_person = request.POST['contact_person']
    customer.cellphone = request.POST['cellphone']
    customer.email = request.POST['email']
    print dir(customer), customer.alias
    erl_call_command, info = toolbox_json.customer_update(customer.uuid, customer.alias, customer.company, customer.address, customer.contact_person,
            customer.telephone, customer.cellphone, customer.email)
    if True:
        return HttpResponse(action_succ_resp_msg%(u'用户更新成功', info))
    else:
        return HttpResponse(action_fail_resp_msg % (u'用户更新失败', info, erl_call_command))

@permission_required('oss.manage_customer',login_url='user_noPermission')
def customer_lock(request):
    customer_uuid = request.GET['customer_uuid']
    customer = Customer.objects.get(uuid = customer_uuid)
    vm_set = VM.objects.filter(customer=customer_uuid)
    for vm in vm_set:
        if vm.status == 'running':
            erl_call_command, info = toolbox_json.vm_stop(vm.uuid)
            if not is_ok(info):
                return HttpResponse(u"""
                            <html>
                            <body><p>虚拟机停止失败，请确保该用户的虚拟机都处于停止状态。</p>
                            <p>%s</p>
                            </html>
                            """%(info,
                                )
                        )

    erl_call_command, info = toolbox_json.customer_lock(customer_uuid)
    if is_ok(info):
        customer.locked = True
        customer.save()

        if not updateGlobalInfo():
            return HttpResponse(u'获取更新信息失败')

        return HttpResponse(u"""
                            <html>
                            <p>用户:%s抛弃成功!</p>
                            </html>
                            """
                            % (customer.alias,
                                )
                        )
    else:
        return HttpResponse(u"""
                            <html>
                            <p>用户:%s抛弃失败!</p>
                            <br /><p>%s</p>
                            </html>
                            """
                            % (customer.alias,
                                info,
                                )
                        )


@permission_required('oss.manage_customer',login_url='user_noPermission')
def customer_unlock(request):
    customer_uuid = request.GET['customer_uuid']
    customer = Customer.objects.get(uuid = customer_uuid)

    erl_call_command, info = toolbox_json.customer_unlock(customer_uuid)
    if is_ok(info):
        customer.locked = False
        customer.save()

        if not updateGlobalInfo():
            return HttpResponse(u'获取更新信息失败!')

        return HttpResponse(u"""
                            <html>
                            <p>用户:%s恢复成功!</p>
                            </html>
                            """
                            % (customer.alias,
                                )
                        )
    else:
        return HttpResponse(u"""
                            <html>
                            <p>用户:%s 恢复失败!</p>
                            <br /><p>%s</p>
                            </html>
                            """
                            %(customer.alias,
                                info,
                                )
                        )

@permission_required('oss.manage_vd',login_url='user_noPermission')
def virtualDisk_lock(request):
    virtualDisk_uuid = request.GET['virtualDisk_uuid']
    virtualDisk = VirtualDisk.objects.get(uuid = virtualDisk_uuid)
    virtualDisk = update_virtualDisk_set([virtualDisk])[0]

    if 'creating' == virtualDisk.status:
        return HttpResponse(u"""
                                <html>
                                <p>虚拟磁盘:%s 正在被创建!</p>
                                </html>
                                """
                                % (virtualDisk.alias)
                            )

    elif 'locked' == virtualDisk.status:
        return HttpResponse(u"""
                                <html>
                                <p>虚拟磁盘:%s已经被抛弃!</p>
                                </html>
                                """
                                % (virtualDisk.alias)
                            )
    elif 'unlocked' == virtualDisk.status:
        if int(virtualDisk.vm) >= 1:
            return HttpResponse(u"该磁盘正在被挂载使用，请先卸载!")
        else:
            erl_call_command, info = toolbox_json.virtualDisk_lock(virtualDisk_uuid)
            if is_ok(info):
                virtualDisk.locked = True
                virtualDisk.save()

                if not updateGlobalInfo():
                    return HttpResponse(u'获取更新信息失败!')

                return HttpResponse(u"""
                        <html>
                         <head>
                         <script type="text/javascript">
                         function ok() {
                             parent.location="customer_list?customer_uuid=%s"
                             window.close();
                         }
                         </script>
                         <head>
                         <body>
                         虚拟磁盘:%s抛弃成功!
                         <button type="button" onclick="ok();">确认</button>
                         </body>
                         </html>
                       """ % (virtualDisk.customer_id, virtualDisk.alias)
                            )
            else:
                return HttpResponse(u"""
                                    <html>
                                    <p>虚拟磁盘:%s抛弃失败!</p>
                                    <br /><p>%s</p>
                                    </html>
                                    """
                                    %(virtualDisk.alias,
                                        info,
                                        )
                                )

@permission_required('oss.manage_vd',login_url='user_noPermission')
def virtualDisk_unlock(request):
    virtualDisk_uuid = request.GET['virtualDisk_uuid']
    virtualDisk = VirtualDisk.objects.get(uuid = virtualDisk_uuid)

    if 'creating' == virtualDisk.status:
        return HttpResponse(u"""
                                <html>
                                <p>虚拟磁盘:%s正在被创建，请稍候!</p>
                                </html>
                                """
                                % (virtualDisk.alias)
                            )
    elif 'unlocked' == virtualDisk.status:
        return HttpResponse(u"""
                                <html>
                                <p>虚拟磁盘:%s未被抛弃!</p>
                                </html>
                                """
                                % (virtualDisk.alias)
                            )
    else:
        erl_call_command, info = toolbox_json.virtualDisk_unlock(virtualDisk_uuid)
        if is_ok(info):
            virtualDisk.locked = False
            virtualDisk.save()

            if not updateGlobalInfo():
                return HttpResponse(u'获取更新信息失败!')

            return HttpResponse(u"""
                                <html>
                                <p>虚拟磁盘:%s恢复成功!</p>
                                </html>
                                """
                                % (virtualDisk.alias,
                                    )
                            )
        else:
            return HttpResponse(u"""
                                <html>
                                <p>虚拟磁盘:%s恢复失败!</p>
                                <br /><p>%s</p>
                                </html>
                                """
                                %(virtualDisk.alias,
                                    info,
                                    )
                            )

def action_response(request):
    response_log = open(response_log_path, "ab+")
    ISOTIMEFORMAT='%Y-%m-%d %X'
    t = time.strftime( ISOTIMEFORMAT, time.localtime() )

    #http://192.168.1.192:8000/action_response?flag=vm_create&uuid=~w&done=true
    if 'vm_create' == request.GET['flag']:
        if request.GET['done'] == 'true':
            vm_uuid = request.GET['uuid']
            vm = VM.objects.get(uuid=vm_uuid)
            msg =  u"<br/> 虚拟机创建成功<br/> ID:%s<br/>  名称:%s\r\n" % (vm_uuid, vm.alias)
        elif request.GET['done'] == 'false':
            vm_uuid = request.GET['uuid']
            error = request.GET['error']
            error = toolbox_json.urlDecode(error)
            msg =  u"<br/>虚拟机创建失败 <br/>ID:%s <br/>错误:%s\r\n" % (vm_uuid, error)
        else:
            msg = "vm_create response no done no error!\r\n"
            msg = u"<br/>虚拟机创建中!\r\n"
    elif 'vm_destroy' == request.GET['flag']:
        if request.GET['done'] == 'true':
            vm_uuid = request.GET['uuid']
            msg = u"<br/>  虚拟机销毁成功<br/>  ID:%s\r\n" % (vm_uuid)
        elif request.GET['done'] == 'false':
            error = request.GET['error']
            error = toolbox_json.urlDecode(error)
            vm_uuid = request.GET['uuid']
            vm = VM.objects.get(uuid=vm_uuid)
            msg = u"<br/>  虚拟机销毁失败 <br/> ID:%s <br/> 名称:%s<br/>  错误:%s\r\n" % (vm_uuid, vm.alias, error)
        else:
            msg = u"<br/>虚拟机销毁中!\r\n"

    elif 'vm_start' == request.GET['flag']:
        if request.GET['done'] == 'true':
            port = request.GET['port']
            vm_uuid = request.GET['uuid']
            vm = VM.objects.get(uuid=vm_uuid)
            msg = u"<br/>  虚拟机启动成功<br/>  ID:%s <br/> 名称:%s\r\n" % (vm_uuid, vm.alias)
        elif request.GET['done'] == 'false':
            error = request.GET['error']
            error = toolbox_json.urlDecode(error)
            vm_uuid = request.GET['uuid']
            vm = VM.objects.get(uuid=vm_uuid)
            msg = u"<br/> 虚拟机启动失败 <br/> ID:%s <br/> 名称:%s <br/> 错误:%s\r\n"%(vm_uuid, vm.alias, error)
        else:
            msg = u"虚拟机启动没有完成!\r\n"
            response_log.write(msg.encode('utf8'))
            response_log.close()
            return HttpResponse("yes")
    elif 'vm_shutdown' == request.GET['flag']:
        if request.GET['done'] == 'true':
            vm_uuid = request.GET['uuid']
            vm = VM.objects.get(uuid=vm_uuid)
            msg = u"<br/>虚拟机关机成功<br/>  ID:%s <br/> 名称:%s\r\n" % (vm_uuid, vm.alias)
        elif request.GET['done'] == 'false':
            error = request.GET['error']
            error = toolbox_json.urlDecode(error)
            vm_uuid = request.GET['uuid']
            vm = VM.objects.get(uuid=vm_uuid)
            msg = u"<br/>  虚拟机关机失败 <br/> ID:%s <br/> 名称:%s<br/>  错误:%s\r\n" % (vm_uuid, vm.alias, error)
        else:
            msg = u"<br/>虚拟机关机没有完成!\r\n"

    elif 'vm_stop' == request.GET['flag']:
        if request.GET['done'] == 'true':
            vm_uuid = request.GET['uuid']
            vm = VM.objects.get(uuid=vm_uuid)
            msg = u"<br/>虚拟机关机成功<br/>  ID:%s <br/> 名称:%s\r\n" % (vm_uuid, vm.alias)
        elif request.GET['done'] == 'false':
            error = request.GET['error']
            error = toolbox_json.urlDecode(error)
            vm_uuid = request.GET['uuid']
            vm = VM.objects.get(uuid=vm_uuid)
            msg = u"<br/>虚拟机关机失败  <br/> 唯一:%s <br/> 名称:%s<br/>  错误:%s\r\n" % (vm_uuid, vm.alias, error)
        else:
            msg = u"<br/>虚拟机关机没有完成!\r\n"

    elif 'vm_pause' == request.GET['flag']:
        if request.GET['done'] == 'true':
            vm_uuid = request.GET['uuid']
            vm = VM.objects.get(uuid=vm_uuid)
            msg = u"<br/> 虚拟机挂起成功<br/>  ID:%s <br/> 名称:%s\r\n" % (vm_uuid, vm.alias)
        elif request.GET['done'] == 'false':
            error = request.GET['error']
            error = toolbox_json.urlDecode(error)
            vm_uuid = request.GET['uuid']
            vm = VM.objects.get(uuid=vm_uuid)
            msg = u"<br/>  虚拟机挂起失败<br/>  ID:%s<br/>  名称:%s<br/>  错误:%s\r\n" % (vm_uuid, vm.alias, error)
        else:
            msg = "<br/>vm_pause response no done no error!\r\n"
            msg = u"<br/>虚拟机挂起没有完成!\r\n"
    elif 'vm_resume' == request.GET['flag']:
        if request.GET['done'] == 'true':
            vm_uuid = request.GET['uuid']
            vm = VM.objects.get(uuid=vm_uuid)
            msg = u"<br/> 虚拟机恢复成功<br/>  ID:%s<br/>  名称:%s\r\n" % (vm_uuid, vm.alias)
        elif request.GET['done'] == 'false':
            error = request.GET['error']
            error = toolbox_json.urlDecode(error)
            vm_uuid = request.GET['uuid']
            vm = VM.objects.get(uuid=vm_uuid)
            msg = u"<br/>虚拟机恢复失败 <br/> ID:%s <br/> 名称:%s <br/> 错误:%s\r\n" % (vm_uuid, vm.alias, error)
        else:
            msg = u"<br/>虚拟机恢复有完成!\r\n"
    elif 'vm_migrate' == request.GET['flag']:
        if request.GET['done'] == 'true':
            vm_uuid = request.GET['uuid']
            port = request.GET['port']
            new_pm_id = request.GET['new_pm_id']
            vm = VM.objects.get(uuid=vm_uuid)
            msg = u"<br/>虚拟机迁移成功<br/>  ID:%s <br/> 名称:%s\r\n" % (vm_uuid, vm.alias)
        elif request.GET['done'] == 'false':
            error = request.GET['error']
            error = toolbox_json.urlDecode(error)
            vm_uuid = request.GET['uuid']
            vm = VM.objects.get(uuid=vm_uuid)
            msg = u"<br/> 虚拟机迁移失败<br/>  ID:%s <br/> 名称:%s <br/> 错误:%s\r\n" % (vm_uuid, vm.alias, error)
        else:
            msg = u"<br/>虚拟机迁移没有完成!\r\n"
    elif 'pm_down' == request.GET['flag']:
        pm_uuid = request.GET['uuid']
        #pm = PM.objects.get(uuid=pm_uuid)
        remark = request.GET['remark']
        msg = u"<br/>  物理机离线 <br/> IP地址:%s, <br/> 消息:%s\r\n" % (pm_uuid, remark)
    elif 'pm_up' == request.GET['flag']:
        pm_uuid = request.GET['uuid']
        #pm = PM.objects.get(uuid=pm_uuid)
        remark = request.GET['remark']
        msg = u"<br/>  物理机上线<br/>  IP地址:%s, <br/> 消息:%s\r\n" % (pm_uuid, remark)
    else:
        response_log.close()
        return HttpResponse(u"发生了一些错误")

    msg = t + msg
    response_log.write(msg.encode('utf8'))
    response_log.close()
    if not updateGlobalInfo():
        return HttpResponse(u'获取更新信息失败!')
    return HttpResponse("yes")



def vm_action(request):
    if request.method == 'GET':
        if 'vm_migrate' == request.GET['action_type']:
            vm_uuid = request.GET['vm_uuid']
            vm = VM.objects.get(uuid=vm_uuid)
            pm_set = PM.objects.exclude(uuid=vm.pm_id)
            t = loader.get_template('vm_migrate.html')
            c = Context({'vm_uuid' : vm_uuid, 'pm_set' : pm_set})
            return HttpResponse(t.render(c))

        elif 'vm_startOnPm' == request.GET['action_type']:
            vm_uuid = request.GET['vm_uuid']
            vm = VM.objects.get(uuid=vm_uuid)
            pm_set = PM.objects.all()
            t = loader.get_template('vm_startOnPm.html')
            c = Context({'vm_uuid' : vm_uuid, 'pm_set' : pm_set})
            return HttpResponse(t.render(c))

        elif 'vm_create' == request.GET['action_type']:
            customer_uuid = request.GET['customer_uuid']
            customer = Customer.objects.get(uuid=customer_uuid)

            virtualDisk_set = VirtualDisk.objects.filter(customer=customer).filter(status='unlocked')
            def filter_disk():
                '''过滤掉独享的已经被使用的磁盘, 过滤后数据类型发生了变化变成了list '''
                virtualDisks = []
                for disk in virtualDisk_set:
                    print disk.shared
                    if disk.shared == 'false':
                        diskvm = DiskVm.objects.filter(disk_id=disk.uuid)
                        if diskvm.count() >= 1:
                            continue
                    virtualDisks.append(disk)
                return virtualDisks
            virtualDisk_set = filter_disk()
            virtualDisk_set = update_virtualDisk_set(virtualDisk_set)

            stdVmImg_set = StdVmImg.objects.all()

            vnc_models_tuple = VNC_MODEL_CHOICES
            vnc_models = []
            for m in range(len(vnc_models_tuple)):
                vnc_models.append(vnc_models_tuple[m][0])

            t = loader.get_template('vm_create.html')
            c = Context({'vnc_models':vnc_models, 'stdVmImg_set':stdVmImg_set, 'customer_uuid':customer_uuid, 'virtualDisk_set':virtualDisk_set})
            return HttpResponse(t.render(c))

        elif 'vm_edit' == request.GET['action_type']:
            vm_uuid = request.GET['vm_uuid']
            vm = VM.objects.get(uuid=vm_uuid)
            customer_uuid = request.GET['customer_uuid']
            customer = Customer.objects.get(uuid=customer_uuid)

            vncs = VirtualNetworkCard.objects.filter(vm=vm_uuid)

            mounted_diskids = []
            for i in DiskVm.objects.filter(vm_id=vm_uuid):
                mounted_diskids.append(i.disk_id)
            vd_mounted_set = VirtualDisk.objects.filter(uuid__in=mounted_diskids).exclude(uuid=vm.boot_disk_id)#.filter(status='unlocked')

         #   boot_disk = VirtualDisk.objects.get(vm=vm.boot_disk)
            virtualDisk_set = VirtualDisk.objects.filter(customer=customer).filter(status='unlocked')
            def filter_disk():
                '''过滤掉独享的已经被使用的磁盘, 过滤后数据类型发生了变化变成了list '''
                virtualDisks = []
                for disk in virtualDisk_set:
                    print disk.shared
                    if disk.shared == 'false':
                        diskvm = DiskVm.objects.filter(disk_id=disk.uuid)
                        if diskvm.count() >= 1:
                            continue
                    if disk.shared == 'true':
                        diskvm = DiskVm.objects.filter(disk_id=disk.uuid)
                        if diskvm.count() >= 1:
                            flag = False
                            for i in diskvm:
                                if i.vm_id == vm_uuid:
                                    flag = True
                                    break
                            if flag:
                                continue
                    virtualDisks.append(disk)
                return virtualDisks
            virtualDisk_set = filter_disk()
            stdVmImg_set = StdVmImg.objects.all()

            vnc_models_tuple = VNC_MODEL_CHOICES
            vnc_models = []
            for m in range(len(vnc_models_tuple)):
                vnc_models.append(vnc_models_tuple[m][0])

            t = loader.get_template('vm_edit.html')
            c = Context({'vnc_models':vnc_models, 'stdVmImg_set':stdVmImg_set,
                'vm':vm,
                'iodrivers': ['virtio', 'ide'],
                'vncs':vncs,
                'vd_mounted_set':vd_mounted_set,
                'boot_disk':vm.boot_disk,
                'virtualDisk_set':virtualDisk_set})
            return HttpResponse(t.render(c))
        else:
            return HttpResponse("unknown vm_action")

@permission_required('oss.manage_vm',login_url='user_noPermission')
def vm_create(request):
    customer_uuid = request.POST['customer_uuid']
    vm_name = request.POST['vm_name']
    cpu_cores = request.POST['cpu_cores']
    mem_capacity = int(request.POST['mem_capacity'])*1024*1024
    iodriver = request.POST['iodriver']

    models = request.POST.getlist('model')
    bridges = request.POST.getlist('bridge')
    macs = request.POST.getlist('mac')
    print models
    print bridges
    print macs

    pcis = []
    for i in range(0, len(models)):
        pci = {'bridge': bridges[i], 'mac': macs[i], 'model': models[i]}
        pcis.append(pci)

    if 'from_std' == request.POST['select_vmimg']:
        vmImg_uuid = request.POST['stdVmImg_selected']
    elif 'from_cust' == request.POST['select_vmimg']:
        vmImg_uuid = request.POST['custVmImg_selected']

    virtualDisk_set = request.POST.getlist('virtualDisk_selected')

    erl_call_command, info = toolbox_json.vm_create(customer_uuid, vm_name,
            cpu_cores, mem_capacity, pcis, vmImg_uuid, virtualDisk_set,
            iodriver )
    if is_ok(info):
        if not updateGlobalInfo():
            return HttpResponse(u'获取更新信息失败!')
        return HttpResponse(action_succ_resp_msg % (u'创建虚拟机', info))
    else:
        return HttpResponse(action_fail_resp_msg % (u'创建虚拟机', info, erl_call_command))

@permission_required('oss.manage_vm',login_url='user_noPermission')
def vm_edit(request):
    vm_uuid = request.POST['vm_uuid']
    vm = VM.objects.get(uuid=vm_uuid)
    vmImg_uuid = vm.boot_disk_id
    vm_name = request.POST['vm_name']
    cpu_cores = request.POST['cpu_cores']
    mem_capacity = int(float(request.POST['mem_capacity']))*1024*1024
    iodriver = request.POST['iodriver']

    virtualDisk_set = request.POST.getlist('virtualDisk_selected')

    models = request.POST.getlist('model')
    bridges = request.POST.getlist('bridge')
    macs = request.POST.getlist('mac')

    pcis = []
    for i in range(0, len(models)):
        pci = {'bridge': bridges[i], 'mac': macs[i], 'model': models[i]}
        pcis.append(pci)


    erl_call_command, info = toolbox_json.vm_edit(vm_uuid, vm_name, cpu_cores,
            mem_capacity, pcis, vmImg_uuid, virtualDisk_set, iodriver )
    if is_ok(info):
        if not updateGlobalInfo():
            return HttpResponse(u'获取更新信息失败!')
        return HttpResponse(action_succ_done_resp_msg % (vm.alias, vm_uuid))
    else:
        return HttpResponse((action_fail_done_resp_msg % (vm.alias, vm_uuid)) + u'<br />错误:%s' % info)

@permission_required('oss.manage_vm',login_url='user_noPermission')
def vm_destroy(request):
    vm_uuid = request.GET['vm_uuid']
    customer_uuid = request.GET['customer_uuid']

    erl_call_command, info = toolbox_json.vm_destroy(vm_uuid)
    if is_ok(info):
        if not updateGlobalInfo():
            return HttpResponse(u'获取更新信息失败!')

        return HttpResponse(u"""
                            <html>
                             <head>
                             <script type="text/javascript">
                             function ok() {
                                 parent.location=\"customer_detail?customer_uuid=%s\";
                                 window.close();
                             }
                             </script>
                             <head>
                             <body>
                             发送销毁命令成功!
                             <button type="button" onclick="ok();">确认</button>
                             </body>
                             </html>
                            """ % (customer_uuid,
                                )
                            )
    else:
        return HttpResponse(action_fail_resp_msg % (u'销毁虚拟机', info, erl_call_command))

@permission_required('oss.manage_vm',login_url='user_user_noPermission')
def vm_start(request):
    print 'vm_start'
    vm_uuid = request.GET['vm_uuid']
    pm_uuid = ''

    erl_call_command, info = toolbox_json.vm_start(vm_uuid, pm_uuid)
    if is_ok(info):
        if not updateGlobalInfo():
            return HttpResponse(u'获取更新信息失败!')
        return HttpResponse(action_succ_resp_msg % (u'开机', info))

    else:
        return HttpResponse(action_fail_resp_msg % (u'开机', info, erl_call_command))

@permission_required('oss.manage_vm',login_url='user_noPermission')
def vm_startOnPm(request):
    print 'vm_startOnPm'
    vm_uuid = request.POST['vm_uuid']
    pm_uuid = request.POST['pm_selected']

    erl_call_command, info = toolbox_json.vm_start(vm_uuid, pm_uuid)
    if is_ok(info):
        if not updateGlobalInfo():
            return HttpResponse(u'获取更新信息失败!')
        return HttpResponse(action_succ_resp_msg % (u'虚拟机在指定物理机上启动', info))
    else:
        return HttpResponse(action_fail_resp_msg % (u'虚拟机在指定物理机上启动', info, erl_call_command))

@permission_required('oss.manage_vm',login_url='user_noPermission')
def vm_shutdown(request):
    vm_uuid = request.GET['vm_uuid']
    customer_uuid = request.GET['customer_uuid']

    erl_call_command, info = toolbox_json.vm_shutdown(vm_uuid)
    if is_ok(info):
        if not updateGlobalInfo():
            return HttpResponse(u'获取更新信息失败!')
        return HttpResponse(action_succ_resp_msg % (u'虚拟机关机', info))
    else:
        return HttpResponse(action_fail_resp_msg % (u'虚拟机关机', info, erl_call_command))


@permission_required('oss.manage_vm',login_url='user_noPermission')
def vm_stop(request):
    vm_uuid = request.GET['vm_uuid']
    customer_uuid = request.GET['customer_uuid']

    erl_call_command, info = toolbox_json.vm_stop(vm_uuid)
    if is_ok(info):
        if not updateGlobalInfo():
            return HttpResponse(u'获取更新信息失败!')
        return HttpResponse(action_succ_resp_msg % (u'关机', info))
    else:
        return HttpResponse(action_fail_resp_msg % (u'关机', info, erl_call_command))

@permission_required('oss.manage_vm',login_url='user_noPermission')
def vm_pause(request):
    vm_uuid = request.GET['vm_uuid']
    customer_uuid = request.GET['customer_uuid']

    erl_call_command, info = toolbox_json.vm_pause(vm_uuid)
    if is_ok(info):
        if not updateGlobalInfo():
            return HttpResponse(u'获取更新信息失败！')
        return HttpResponse(action_succ_resp_msg % (u'虚拟机挂起'))
    else:
        return HttpResponse(action_fail_resp_msg % (u'虚拟机挂起', info, erl_call_command))

@permission_required('oss.manage_vm',login_url='user_noPermission')
def vm_resume(request):
    vm_uuid = request.GET['vm_uuid']
    customer_uuid = request.GET['customer_uuid']

    erl_call_command, info = toolbox_json.vm_resume(vm_uuid)
    if is_ok(info):
        if not updateGlobalInfo():
            return HttpResponse(u'获取更新信息失败！')
        return HttpResponse(action_succ_resp_msg % (u'虚拟机恢复', info))
    else:
        return HttpResponse(action_fail_resp_msg % (u'虚拟机恢复', info, erl_call_command))

@permission_required('oss.manage_vm',login_url='user_noPermission')
def vm_migrate(request):
    vm_uuid = request.POST['vm_uuid']
    try:
        if 'liveMigrate_auto' == request.POST['select_pm']:
            pm_uuid = None
        elif 'liveMigrate_aim' == request.POST['select_pm']:
            pm_uuid = request.POST['pm_selected']
    except Exception, e:
        return HttpResponse(u"""
                            <html>
                             <body>
                             请选择一个物理机来迁移.
                             <button type="button" onclick="history.back();">返回</button>
                             </body>
                             </html>
                           """ )

    erl_call_command, info = toolbox_json.vm_migrate(vm_uuid, pm_uuid)
    if is_ok(info):
        if not updateGlobalInfo():
            return HttpResponse(u'获取更新信息失败！')
        return HttpResponse(action_succ_resp_msg % (u'虚拟机迁移', info))
    else:
        return HttpResponse(action_fail_resp_msg % (u'虚拟机迁移', info, erl_call_command))


@validateUser
def yfs(request):
    if not request.user.is_authenticated():
        print "validte false"
        return HttpResponseRedirect('login')
    else:
        yfs_result=Yfs.objects.all()
        pid_result=Pids.objects.all()
        return render_to_response('yfs.html',{'yfs_result':yfs_result,'pid_result':pid_result})

yfs_start_msg = u"""
                        <html>
                         <head>
                         <script type="text/javascript">
                         function ok() {
                             parent.location.reload();
                             //window.close();
                         }
                         </script>
                         <head>
                         <body>
                        <center>  %s</br></br></br></center>
                       <center> <button type="button" onclick="ok();">确认</button></center>
                         </body>
                         </html>
                       """

def user_login(request):
    user = request.POST['user']
    pwd = request.POST['password']
    auth_code=request.POST['auth_code']

    user = authenticate(username=user, password=pwd)
    if user is None:
        err_message= u'用户名或密码错误'
        return render_to_response('user_login.html',{'err_message':err_message })
    else:
        if not request.session['auth_code'] == auth_code:
            err_message= u'验证码错误'
            return render_to_response('user_login.html',{'err_message':err_message })
        login(request,user)
        request.session.set_expiry(0)
        return HttpResponseRedirect("/frame")

def user_logout(request):
    logout(request)
    return HttpResponse("<script>window.location.replace('/login');</script>")

def user_createUser(request):
    u = request.GET['user']
    pwd = request.GET['password']
    group=request.GET['group']
    g=Group.objects.get(name=group)
    try:
        user=User.objects.create_user(username=u,password=pwd,email="")
        user.groups.add(g)
        user.save()
    except :
        return HttpResponse(u"用户已经存在")
    return HttpResponse(u'成功创建用户')

def user_delUser(request):
    u = request.GET['user']
    if u == 'root':
        return HttpResponse(yfs_start_msg %u"root用户不能删除")
    if u == request.user.username:
        return HttpResponse(yfs_start_msg %u"当前用户不能删除")
    try:
        user=User.objects.get(username=u)
        user.delete()
    except:
        return HttpResponse(yfs_start_msg %u"删除用户失败")
    return HttpResponse(yfs_start_msg %u"用户成功删除")

def user_changeUserGroup(request):
    u=request.GET['user']
    newgroup=request.GET['group']
    if u=='root':
        return HttpResponse(u"root 用户不能修改")
    try:
        user=User.objects.get(username=u)
        g=Group.objects.get(name=newgroup)
    except:
        raise Http404
    user.groups.clear()
    user.groups.add(g)
    return HttpResponse(u"修改成功")

def user_delGroup(request):
    g = request.GET['group']
    if g == 'superUser':
        return HttpResponse(yfs_start_msg %u"superUser 不能删除")
    try:
        group = Group.objects.get(name=g)
        group.delete()
    except:
        return HttpResponse(yfs_start_msg %u"删除选定组失败")
    return HttpResponse(yfs_start_msg %u"选定的组成功删除")

def user_addGroup(request):
    g = request.GET['group']
    p_list=request.GET['permission']
    p_list=p_list.split(',')
    p_list=list(p_list)
    try:
        group = Group(name=g)
        group.save()
        for pn in p_list:
            p = Permission.objects.get(codename=pn)
            group.permissions.add(p)
            group.save()
    except:
        return HttpResponse(u"该组已存在")
    return HttpResponse(u"新用户组创建成功")


def user_setPassword(request):
    user = request.GET['user']
    pwd = request.GET['password']
    newpwd = request.GET['newpassword']
    try:
        user=User.objects.get(username__exact=user)
    except User.DoesNotExist:
        return HttpResponse(u'用户名错误')

    if  user.check_password(pwd):
        user.set_password(newpwd);
        user.save()
        return HttpResponse(u'修改密码成功')
    else:
        return HttpResponse(u'旧密码错误')

@permission_required('auth.manage_User',login_url='user_noPermission')
def user_userList(request):
    try:
        user_list=User.objects.all()
    except:
        raise Http404
    return render_to_response('user_userManagement.html',{'user_list':user_list})

def user_groupList(request):
    try:
        group_list=Group.objects.all()
    except:
        raise Http404
    return render_to_response('user_groupManagement.html',{'group_list':group_list})

def user_groupPermissionInfo(request):
    g = request.GET['group']
    try:
        group=Group.objects.get(name=g)
    except Group.DoesNotExist:
        raise Http404
    return render_to_response('user_changeGroupPermission.html',{'group':group})

def user_changeGroupPermission(request):
    g = request.GET['group']
    if g=='superUser':
        return HttpResponse(u"超级 用户不能修改")
    p_list=request.GET['permission']
    p_list=p_list.split(',')
    p_list=list(p_list)
    try:
        group = Group.objects.get(name=g)
        group.permissions.clear()
        for pn in p_list:
            p = Permission.objects.get(codename=pn)
            group.permissions.add(p)
            group.save()
    except:
        return HttpResponse(u"修改用户组权限失败")
    return HttpResponse(u"修改用户组权限成功")


def user_getUserGroup(request):
    u=request.GET['user']
    try:
        user=User.objects.get(username=u)
        group=Group.objects.all()
    except:
        raise Http404
    return render_to_response('user_changeUserGroup.html',{'user':user,'group_list':group})

def get_check_code_image(request):
    c = checker.picChecker() #验证码
    f = cStringIO.StringIO()
    t = c.createChecker(f)
    print t
    request.session['auth_code'] = t[0]
    return HttpResponse(f.getvalue(),'image/jpeg')

def get_vnc(request):
    html = '''<HTML>
        <HEAD>
        <TITLE>vnc窗口</TITLE>
        </HEAD>
        <BODY>
        <OBJECT ID="Exe1" CLASSID="CLSID:83831D65-B4CC-4A7D-B362-D9AF6495C5A9">
        <param name="Path0" value="%s">
        </OBJECT>
        </BODY>
        </HTML>'''
    value = "path={C:\\Program Files\\RealVNC\\VNC4\\vncviewer.exe} ip={192.168.1.21:11}"
    return HttpResponse(html%(value))
    #return render_to_response('vnc_view.html',{'value':value})

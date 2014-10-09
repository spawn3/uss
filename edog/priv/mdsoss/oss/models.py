from django.db import models
from django.contrib import admin

from django.db.models import signals
from django.contrib.auth.management import create_superuser
from django.contrib.auth import models as auth_models
# Prevent interactive question about wanting a superuser created.  (This
# code has to go in this otherwise empty "models" module so that it gets
# processed by the "syncdb" command during database creation.)

signals.post_syncdb.disconnect(
     create_superuser,
     sender=auth_models,
     dispatch_uid = "django.contrib.auth.management.create_superuser")

def create_whsuser(app, created_models, verbosity, **kwargs):
    try:
        auth_models.User.objects.get(username='root')
    except auth_models.User.DoesNotExist:
        print '*' * 80
        print 'Creating test user -- login: root, password: 123456'
        print '*' * 80
        assert auth_models.User.objects.create_superuser('root', 'x@x.com', '123456')
    else:
        print 'Test user already exists.'

signals.post_syncdb.connect(
     create_whsuser,
     sender=auth_models,
     dispatch_uid='common.models.create_whsuser')


#ACTION_CHOICES = (
#    ('', 'vm_create'),
#    ('', 'vm_destroy'),
#    ('', 'vm_start'),
#    ('', 'vm_stop'),
#    ('', 'vm_pause'),
#    ('', 'vm_resume'),
#    ('', 'vm_migrate'),
#)
#class EventMgs(models.Model):
#    action = models.CharField(max_length=20, choices=ACTION_CHOICES)
#    success = models.BooleanField()

class Monitor(models.Model):
    id = models.AutoField(primary_key=True)
    time = models.IntegerField()
    pm = models.ForeignKey("PM", related_name="monitor_info")
    cpuAvg1 = models.IntegerField(null=True)
    cpuAvg5 = models.IntegerField(null=True)
    cpuAvg15 = models.IntegerField(null=True)
    memTotal = models.IntegerField()
    memFree = models.IntegerField(null=True)
    cached = models.IntegerField(null=True)
    buffered = models.IntegerField(null=True)
    swapTotal = models.IntegerField()
    swapFree = models.IntegerField(null=True)
    netRecv = models.IntegerField(null=True)
    netSend = models.IntegerField(null=True)

class StdVmImg(models.Model):
    uuid = models.CharField(max_length=36, primary_key=True)
    os_release = models.CharField(max_length=15)
    os_version = models.CharField(max_length=15)
    path = models.CharField(max_length=500)
    capacity = models.FloatField()

    def __unicode__(self):
        return u'%s %s' % (self.os_release, self.os_vsrsion)

class Bridge(models.Model):
    name = models.CharField(max_length=30)
    ip = models.IPAddressField()
    mask = models.IPAddressField()
    mac = models.CharField(max_length=20, primary_key=True)

    pm = models.ForeignKey("PM", related_name="bridge_set")#, db_column='pm_uuid'

    def __unicode__(self):
        return self.name

    def __virtualNetworkCard_set__(self):
        return self.virtual_network_card_set

VNC_MODEL_CHOICES = (
                     (u'e1000', u'e1000'),
                     (u'rtl8139', u'rtl8139'),
                     (u'virtio', u'virtio'),
                     (u'ne2k_isa', u'ne2k_isa'),
                     (u'i82551', u'i82551'),
                     (u'i82557b', u'i82557b'),
                     (u'i82559er', u'i82559er'),
                     (u'ne2k_pci', u'ne2k_pci'),
                     (u'pcnet', u'pcnet'),
                     )
class VirtualNetworkCard(models.Model):
    #model = models.CharField(max_length=20, choices=VNC_MODEL_CHOICES)
    model = models.CharField(max_length=20)
    mac = models.CharField(max_length=20, primary_key=True)

    bridge = models.CharField(max_length=20)
    #bridge = models.ForeignKey("Bridge",related_name="virtualNetworkCard_set")#, db_column='bridge_mac'

    vm = models.ForeignKey("VM", related_name="virtualNetworkCard_set")#, db_column='vm_uuid'

    def __unicode__(self):
        return self.mac

class VirtualDisk(models.Model):
    uuid = models.CharField(max_length=36, primary_key=True)
    path = models.CharField(max_length=500)
    target = models.CharField(max_length=20, null=True)
    alias = models.CharField(max_length=50)
    shared = models.CharField(max_length=50)
    capacity = models.FloatField()

    is_vmimg = models.BooleanField()#canboot|os_installed

    status = models.CharField(max_length=20)#creating/locked/unlocked
#    locked = models.BooleanField(default=False)
    locktime = models.CharField(max_length=50)

    vm = models.CharField(max_length=500)
    #vm = models.ForeignKey("VM", null=True, related_name="virtualDisk_set")#vritualDisk maybe belong to none of vms   #, db_column='vm_uuid'
    customer = models.ForeignKey("Customer", related_name="virtualDisk_set")

    def __unicode__(self):
        return self.path


class PM(models.Model):
    ## IP
    uuid         = models.CharField(max_length = 36, primary_key = True)
    ## vm count
    ip           = models.IPAddressField()
    ##########
    cpu_cores    = models.IntegerField()
    cpu_alloced  = models.IntegerField()
    ##
    mem_capacity = models.FloatField()
    ##
    mem_alloced  = models.FloatField()
    mem_used     = models.FloatField(null      = True)
    is_running   = models.BooleanField()

    def __unicode__(self):
        return self.ip

    def __vm_set__(self):
        return self.vm_set

    def __bridge_set__(self):
        return self.bridge_set

class VM(models.Model):
    uuid = models.CharField(max_length=36, primary_key=True)
    alias = models.CharField(max_length=20, verbose_name="The name to identify the vm")
    cpu_cores = models.IntegerField()
    mem_capacity = models.IntegerField()
    iodriver = models.CharField(max_length=10)

    status = models.CharField(max_length=10)
    port = models.CharField(max_length=10, null=True)

    is_running = models.BooleanField()
    boot_disk = models.OneToOneField("VirtualDisk", related_name="vm_img")#, db_column='boot_disk_uuid'

    pm = models.ForeignKey("PM", related_name="vm_set", null=True)#, db_column='pm_uuid'
    customer = models.ForeignKey("Customer", related_name="vm_set")#, db_column='customer_uuid'

    cpu_util = models.FloatField()
    mem_util = models.FloatField()

    mem_real = models.IntegerField()

    def __unicode__(self):
        return self.alias

    def __virtualNetworkCard_set__(self):
        return self.virtual_network_card_set

    def __virtualDisk_set__(self):
        return self.virtual_disk_set

class DiskVm(models.Model):
    disk_id = models.CharField(max_length=36)
    vm_id = models.CharField(max_length=36)
    target = models.CharField(max_length=36)

    def __unicode__(self):
        return u'%s_%s'%(self.disk_id, self.vm_id)

class Customer(models.Model):
    uuid = models.CharField(max_length=36, primary_key=True)
    alias = models.CharField(max_length=20)
    company = models.CharField(max_length=50)
    address = models.CharField(max_length=80, null=True)
    telephone = models.CharField(max_length=15, null=True)
    cellphone = models.CharField(max_length=15, null=True)
    contact_person = models.CharField(max_length=20, null=True)
    email = models.EmailField(null=True)

    locked = models.BooleanField(default=False)
    locktime = models.CharField(max_length=50)

    def __unicode__(self):
        return self.alias

    def __vm_set__(self):
        return self.vm_set

    def __virtualDisk_set__(self):
        return self.virtualDisk_set

#class PMAdmin(admin.ModelAdmin):
#    list_display = ('uuid', 'ip', 'cpu_cores', 'mem_capacity', 'mem_used', 'vm_set', 'bridge_set')
#    list_per_page = 10
#
#class VMAdmin(admin.ModelAdmin):
#    list_display = ('uuid', 'alias', 'cpu_cores', 'mem_capacity', 'pm', 'customer','virtualDisk_set', 'virtualNetworkCard_set')
#    list_per_page = 20
#
#class CustomerAdmin(admin.ModelAdmin):
#    list_display = ('uuid', 'company', 'address', 'telephone', 'cellphone', 'contact_person', 'email', 'vm_set', 'virtualDisk_set')
#    list_per_page = 10
#    #list_editable = ('contact_person','address')
#    list_select_related = True
#admin.site.register(PM, PMAdmin)
#admin.site.register(VM, VMAdmin)
#admin.site.register(Customer, CustomerAdmin)


class Yfs(models.Model):
    name = models.CharField(max_length=10)
    ip = models.IPAddressField()
    num = models.IntegerField()
    status = models.CharField(max_length=10,)
    class Meta:
        ordering=['name']

class Pids(models.Model):
    pid=models.CharField(max_length=10,null=True)
    cmd=models.CharField(max_length=80,null=True)
    yfs=models.ForeignKey('Yfs')


class Option(models.Model):
    key=models.CharField(max_length=50,null=True)
    value=models.CharField(max_length=50,null=True)
    class Meta:
        ordering=['key']


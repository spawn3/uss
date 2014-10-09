from django.conf.urls.defaults import *
from django.views.generic.simple import direct_to_template
import settings
from django.views.generic.list_detail import object_list
from django.contrib.auth.models import Group
# Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()

from mdsoss.oss.views import *

urlpatterns = patterns('',
    # Example:
    # (r'^mdsoss/', include('mdsoss.foo.urls')),

    # Uncomment the admin/doc line below and add 'django.contrib.admindocs'
    # to INSTALLED_APPS to enable admin documentation:
    # (r'^admin/doc/', include('django.contrib.admindocs.urls')),

    (r'^test_validate$', test_validate),
    (r'^events_msg', events_msg),
    (r'^test$', test),
    (r'^test_js$', test_js),
    (r'^frame', frame),
    (r'^$', frame),
    (r'^events_scrolling', events_scrolling),
    (r'^header', header),
    (r'^home', home),
    (r'^getGlobalInfo', getGlobalInfo),
    (r'^getMonitorInfo', getMonitorInfo),
    (r'^global_view', global_view),
    (r'^action_response', action_response),
    (r'^recycle_bin', recycle_bin),
    (r'^monitorPngs', monitorPngs),

    (r'^vm_list', vm_list),
    (r'^vm_detail', vm_detail),
    (r'^vm_action', vm_action),
    (r'^vm_create', vm_create),
    (r'^vm_destroy', vm_destroy),
    (r'^vm_start$', vm_start),
    (r'^vm_startOnPm', vm_startOnPm),
    (r'^vm_shutdown', vm_shutdown),
    (r'^vm_stop', vm_stop),
    (r'^vm_pause', vm_pause),
    (r'^vm_resume', vm_resume),
    (r'^vm_migrate', vm_migrate),
    (r'^vm_edit', vm_edit),


    (r'^pm_list', pm_list),
    (r'^pm_detail', pm_detail),

    (r'^customer_list', customer_list),
    (r'^customer_detail', customer_detail),
    (r'^customer_action', customer_action),
    (r'^customer_create', customer_create),
    (r'^customer_lock', customer_lock),
    (r'^get_customer_update', get_customer_update),
    (r'^customer_update', customer_update),
    (r'^customer_unlock', customer_unlock),

    (r'^virtualDisk_create', virtualDisk_create),
    (r'^virtualDisk_action', virtualDisk_action),
    (r'^virtualDisk_list', virtualDisk_list),
    (r'^virtualDisk_detail', virtualDisk_detail),
    (r'^virtualDisk_lock', virtualDisk_lock),
    (r'^virtualDisk_unlock', virtualDisk_unlock),


    # (r'^yfs$',yfs),
    # (r'^yfs_startSelectedtYfs',yfs_startSelectedtNode),
    # (r'^yfs_startAllNode',yfs_startAllNode),
    # (r'^yfs_startSelectedKindNode',yfs_startSelectedKindNode),
    # (r'^yfs_stoptAllNode',yfs_stopAllNode),


    (r'^login$', direct_to_template,{'template':'user_login.html'}),
    #(r'^$', 'django.contrib.auth.views.login',{'template_name':'login.html'}),
    (r'^user_login$',user_login),
    (r'^user_createUser$',user_createUser),
    (r'^user_userManagement$',user_userList),
    (r'^user_groupManagement$',user_groupList),
    (r'^user_Password$',direct_to_template,{'template':'user_setPassword.html'}),
    (r'^user_setPassword$',user_setPassword),
    (r'^user_logout$',user_logout),
    (r'^user_addUser$',object_list,{'template_name':'user_addUser.html','queryset':Group.objects.all(),'template_object_name':'group'}),
    (r'^user_deleteUser$',user_delUser),
    (r'^user_deleteGroup$',user_delGroup),
    (r'^user_newGroup$',direct_to_template,{'template':'user_addGroup.html'}),
    (r'^user_addGroup$',user_addGroup),
    (r'^user_changeUserGroup$',user_changeUserGroup),
    (r'^user_groupPermission$',user_groupPermissionInfo),
    (r'^user_changeGroupPermission$',user_changeGroupPermission),
    (r'^user_noPermission$',direct_to_template,{'template':'user_noPermission.html'}),
    (r'^user_getUserGroup$',user_getUserGroup),


    (r'^get_check_code_image/$',get_check_code_image),
    (r'^get_vnc.html$',get_vnc),


#    (r'^option_list$',object_list,{'template_name':'option.html','queryset':Option.objects.all(),'template_object_name':'option'}),


    (r'^stdVmImg_list', stdVmImg_list),
    (r'^stdVmImg_detail', stdVmImg_detail),

    (r'^test', test),
    (r'^favicon.ico', favicon),

    # Uncomment the next line to enable the admin:
    (r'^admin/', include(admin.site.urls)),
)

urlpatterns += patterns('',
        (r'^static/(?P<path>.*)$', 'django.views.static.serve', {'document_root': settings.STATIC_PATH}),
        (r'^monitor/(?P<path>.*)$', 'django.views.static.serve', {'document_root': settings.MONITOR_PATH}),
    )
if settings.DEBUG:
    pass
else:
    print 'for test'

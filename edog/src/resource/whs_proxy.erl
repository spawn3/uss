-module(whs_proxy).
-compile(export_all).
-include("edog_common.hrl").

-define(Methods, ["CreateVolume", "DescribeVolumes", 
										"DeleteVolume", "CreateImage", 
										"ModifyImageAttribute", "DescribeImages",
										"DeleteImage", "RunInstance", 
										"TerminateInstance", "StartInstances", 
										"RebootInstances", "StopInstances",
										"DescribeInstances", "ModifyInstanceAttribute",
										"AttatcVolume", "DetachVolume"
									]).

-define(Errors, [
		{401, "unsupport"},
	]).

%检查Qs是否包含了Action并且Action的值包含在Methods，
%qs是用户发送过来的查询,
check_qs(Qs) ->
	Method = proplists:get_value("Action", Qs),
	case lists:member(Method, ?Methods) of
		true -> {ok, ok };
		false -> {error, 'not support your method'}
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%卷管理
create_volume(Qs)->
	%必要参数：AvailabilityZone(这个我们没有，可以使用默认default来代替)
	%size,              返回：volumeid有吗,   
	CustUuid = "CustUuid",
	Alias = "Alias", 
	Size = proplists:get_value("Size", Qs),
	Capacity = Size,
	case edog_master:disk_create(CustUuid, Alias, Capacity) of
		{ok, DiskId} ->
			{ok, list_to_binary(DiskId)};
		{error, {timeout, _Reason}} ->
			{error, timeout};
		{error, Reason} ->
			{error, Reason}
	end.

%describe_volumes(Qs)->
%	%先实现查询所有的卷和某些指定id的卷，过滤后考虑, 支持哪些过滤
%	%无
%	%edog_master:edog_resouce_disk(DiskId),
%	{ok, ok }.
%
%delete_volume(Qs)->
%	%DiskId怎么获得, 在创建时可以得到，
%	%edog_master:disk_distory(DiskId),
%	{ok, ok }.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%模板管理
%
%create_image(Qs)->
%	{ok, ok}.
%
%modify_image_attribute(Qs)->
%	{ok, ok}.
%
%describe_images(Qs)->
%	{ok, ok }.
%
%delete_image(Qs)->
%	{ok, ok }.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%虚拟机管理
%
%%args = [{key, value}, {}]
%run_instance(Args)->
%	%必要参数：ImageId, MinCount, MaxCount,  
%	%edog_master:vm_create(CustId, VmName, Cpu, Mem, Pcis, VmImgID, Disks),
%	io:format("~w ~n", [Args]),
%	{Args, runInstance}.
%
%terminate_instance(Qs)->
%	%必要参数：接受一个或多个id
%	%edog_master:vm_destory(VmID),
%	{ok, ok}.
%
%start_instance(Qs)->
%	%必要参数：接受一个或多个id
%	%edog_master:vm_start(VmID),
%	{ok, ok}.
%
%stop_instance(Qs)->
%	%必要参数：接受一个或多个id。  另外：force参数
%	%edog_master:vm_stop(VmID),
%	{ok, ok}.
%
%reboot_instance(Qs)->
%	%无, 取决于kvm，libvirt是否支持，
%	{ok, ok}.
%
%describe_instances(Qs)->
%	%先实现查询所有的卷和某些指定id的卷，过滤后考虑
%	%疑问
%	%edog_master:vm_show(vm),
%	{ok, ok}.
%
%modify_instance_attribute(Qs)->
%	%我们暂时支持修改配置，
%	%接受到的是类型，转换为具体的值
%	%edog_master:vm_update(VmId, VmName, Vcpus, Mem, Pcis, VmImgId, Disks),
%	{ok, ok}.
%
%attach_volume(Qs)->
%	%暂实现的为冷，虚拟机先关闭，vm_update后，启动
%	{ok, ok}.
%
%detach_volume(Qs)->
%	%暂实现的为冷，虚拟机先关闭，vm_update后，启动
%	{ok, ok}.

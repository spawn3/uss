
def vm_create():
    #edog_master vm_create [CustID, PmID, VmName, Cpu, Mem, Pcis, VmImgID, Disks]

    #erl_call -name ussadmin_master@ecloud.org -c edog -a 'edog_master vm_create
    #["fe888f92-49e7-4d21-867c-cf7e8223a33f", "192.168.1.51", "zkzvm1", 2, 1000000, ["virbr0", "virbr0"], "cbe99a12-194e-4342-82cc-797b090eff17", ["ff4044ab-8e90-44c0-8732-e5a17b0e5660"]]'

    customer_uuid = "fe888f92-49e7-4d21-867c-cf7e8223a33f"
    pm_uuid = "192.168.1.51"
    vm_name = "zkzvm1"
    cpu_cores = "2"
    mem_capacity = "1000000"
    virtualNetworkCard_num = "2"
    vmImg_uuid = "cbe99a12-194e-4342-82cc-797b090eff17"
    virtualDisk_set = ["ff4044ab-8e90-44c0-8732-e5a17b0e5660"]

    virtualDisk_str = ''
    for virtualDisk_uuid in virtualDisk_set:
        virtualDisk_str = virtualDisk_uuid + ','
    virtualDisk_str = virtualDisk_str[0:-1]
    print virtualDisk_str

    virtualNetworkCard_str = ''
    i = 0
    while i<virtualNetworkCard_num :
        virtualNetworkCard_str += "'virbr0',"
        i += 1
    virtualNetworkCard_str = "[" + virtualNetworkCard_str[0:-1] + "]"
    print virtualNetworkCard_str

    erl_call_command = 'edog_master vm_create ["%s", "%s", "%s", "%s", "%s", "%s", [' \
                % (customer_uuid, pm_uuid, vm_name, cpu_cores, mem_capacity, virtualNetworkCard_str, vmImg_uuid) \
                + virtualDisk_str + ']]'
    print erl_call_command



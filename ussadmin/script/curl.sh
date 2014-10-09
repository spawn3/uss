SERVER=192.168.1.201

# node
curl http://$SERVER:9501/erl/edog_web/node_add?ip=192.168.1.201
curl http://$SERVER:9501/erl/edog_web/node_add?ip=192.168.1.202

curl http://$SERVER:9501/erl/edog_web/node_delete?ip=192.168.1.201
curl http://$SERVER:9501/erl/edog_web/node_delete?ip=192.168.1.202

curl http://$SERVER:9501/erl/edog_web/node_add?ip=192.168.1.201
curl http://$SERVER:9501/erl/edog_web/node_add?ip=192.168.1.202

# service
curl http://$SERVER:9501/erl/edog_web/yfs_service_add?proc=c60"&"ip=192.168.1.201"&"num=1
curl http://$SERVER:9501/erl/edog_web/yfs_service_add?proc=mds"&"ip=192.168.1.201"&"num=1
curl http://$SERVER:9501/erl/edog_web/yfs_service_add?proc=cds"&"ip=192.168.1.201"&"num=1
curl http://$SERVER:9501/erl/edog_web/yfs_service_add?proc=cds"&"ip=192.168.1.201"&"num=2
curl http://$SERVER:9501/erl/edog_web/yfs_service_add?proc=cds"&"ip=192.168.1.201"&"num=3

curl http://$SERVER:9501/erl/edog_web/yfs_service_delete?proc=c60"&"ip=192.168.1.201"&"num=1
curl http://$SERVER:9501/erl/edog_web/yfs_service_delete?proc=mds"&"ip=192.168.1.201"&"num=1
curl http://$SERVER:9501/erl/edog_web/yfs_service_delete?proc=cds"&"ip=192.168.1.201"&"num=1
curl http://$SERVER:9501/erl/edog_web/yfs_service_delete?proc=cds"&"ip=192.168.1.201"&"num=2
curl http://$SERVER:9501/erl/edog_web/yfs_service_delete?proc=cds"&"ip=192.168.1.201"&"num=3

curl http://$SERVER:9501/erl/edog_web/yfs_service_add?proc=c60"&"ip=192.168.1.201"&"num=1
curl http://$SERVER:9501/erl/edog_web/yfs_service_add?proc=mds"&"ip=192.168.1.201"&"num=1
curl http://$SERVER:9501/erl/edog_web/yfs_service_add?proc=cds"&"ip=192.168.1.201"&"num=1
curl http://$SERVER:9501/erl/edog_web/yfs_service_add?proc=cds"&"ip=192.168.1.201"&"num=2
curl http://$SERVER:9501/erl/edog_web/yfs_service_add?proc=cds"&"ip=192.168.1.201"&"num=3

# curl http://$SERVER:9501/erl/edog_web/yfs_start_noArg
# curl http://$SERVER:9501/erl/edog_web/yfs_start?proc=c60
# curl http://$SERVER:9501/erl/edog_web/yfs_start?proc=mds
# curl http://$SERVER:9501/erl/edog_web/yfs_start?proc=cds

echo ok

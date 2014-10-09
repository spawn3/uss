pkill -9 proxy_server
/sysy/yfs/app/sbin/proxy_server -h 192.168.1.2 -m 255.255.255.0

rm -rf /sysy/yfs/edog/libvirtd.pid
/sysy/yfs/libvirtd77/sbin/libvirtd -d -p /sysy/yfs/edog/libvirtd.pid

pkill -9 heart
pkill -9 beam
/sysy/yfs/edog/script/start_agent.sh $1

#!/usr/bin/env bash

# ovsdb-tool create /usr/local/etc/ovs-vswitch.conf.db vswitchd/vswitch.ovsschema

ovsdb-server /usr/local/etc/ovs-vswitchd.conf.db \
    --remote=punix:/usr/local/var/run/openvswitch/db.sock \
    --remote=db:Open_vSwitch,managers \
    --private-key=db:SSL,private_key \
    --certificate=db:SSL,certificate \
    --bootstrap-ca-cert=db:SSL,ca_cert \
    --pidfile --detach

ovs-vsctl --no-wait init

ovs-vswitchd unix:/usr/local/var/run/openvswitch/db.sock \
    --pidfile --detach

# ovs-vsctl add-br br0
# ovs-vsctl add-port br0 eth0
# ovs-vsctl add-port br0 vif1.0

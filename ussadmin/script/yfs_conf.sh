#!/usr/bin/env bash

CONF_FILE=/sysy/yfs/etc/yfs.conf

get_count()
{
    MDS_COUNT=$(awk '$1 ~ /mds_count/ { print $2 }' $CONF_FILE|cut -d";" -f1)
    CDS_COUNT=$(awk '$1 ~ /cds_count/ { print $2 }' $CONF_FILE|cut -d";" -f1)
    CLI_COUNT=$(awk '$1 ~ /client_count/ { print $2 }' $CONF_FILE|cut -d";" -f1)
    echo $MDS_COUNT
    echo $CDS_COUNT
    echo $CLI_COUNT
}

get_network()
{
    CLUSTER=$(awk '$1 ~ /cluster_name/ { print $2 }' $CONF_FILE|cut -d";" -f1)
    NETWORK=$(awk '$1 ~ /network/ { print $2 }' $CONF_FILE|cut -d";" -f1)
    MASK=$(awk '$1 ~ /mask/ { print $2 }' $CONF_FILE|cut -d";" -f1)
    echo $CLUSTER
    echo $NETWORK
    echo $MASK
}

get_items()
{
    echo "cluster_name", "cluster name",      $(awk '$1 ~ /cluster_name/ { print $2 }' $CONF_FILE|cut -d";" -f1)
    echo "yfs_home_dir", "yfs home dir",      $(awk '$1 ~ /yfs_home_dir/ { print $2 }' $CONF_FILE|cut -d";" -f1)
    echo "rjnl_path",    "reiserfs jnl path", $(awk '$1 ~ /rjnl_path/ { print $2 }' $CONF_FILE|cut -d";" -f1)
    echo "chunk_rep",    "chunk replication", $(awk '$1 ~ /chunk_rep/ { print $2 }' $CONF_FILE|cut -d";" -f1)
    echo "network",      "subnet ip",         $(awk '$1 ~ /network/ { print $2 }' $CONF_FILE|cut -d";" -f1)
    echo "netmask",      "subnet mask",       $(awk '$1 ~ /mask/ { print $2 }' $CONF_FILE|cut -d";" -f1)
    echo "mds_count",    "mds count",         $(awk '$1 ~ /mds_count/ { print $2 }' $CONF_FILE|cut -d";" -f1)
    echo "cds_count",    "cds count",         $(awk '$1 ~ /cds_count/ { print $2 }' $CONF_FILE|cut -d";" -f1)
    echo "client_count", "client count",      $(awk '$1 ~ /client_count/ { print $2 }' $CONF_FILE|cut -d";" -f1)
}

# mds cds client
set_count()
{
    sed -i "s/^mds_count.*/mds_count $1;/g" $CONF_FILE
    sed -i "s/^cds_count.*/cds_count $2;/g" $CONF_FILE
    sed -i "s/^client_count.*/client_count $3;/g" $CONF_FILE
}

set_network()
{
    sed -i "s/^network.*/network $1;/g" $CONF_FILE
    sed -i "s/^mask.*/mask $2;/g" $CONF_FILE
}

###################################################
while getopts "gs:" opt;
do
    case $opt in
        s)
            case $OPTARG in
                network)
                    set_network $3 $4
                    ;;
                count)
                    set_count $3 $4 $5
                    ;;
            esac
            ;;
        g)
            get_items
            ;;
        *)
            ;;
    esac
done

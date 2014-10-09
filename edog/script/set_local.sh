YFS_LIB=/sysy/yfs/app/lib
LIBVIRTD=/sysy/yfs/libvirtd77
HOSTFILE=/etc/hosts

# adjust /etc/ld.so.conf.d/sysy.conf
uname -a | grep -Eq gentoo && { echo LDPATH=$YFS_LIB > /etc/env.d/50sysy; env-update 2>/dev/null;} || { echo $YFS_LIB > /etc/ld.so.conf.d/sysy.conf; ldconfig 2>/dev/null;}
uname -a | grep -Eq gentoo && { echo LDPATH=$LIBVIRTD > /etc/env.d/50sysy; env-update 2>/dev/null;} || { echo $LIBVIRTD >> /etc/ld.so.conf.d/sysy.conf; ldconfig 2>/dev/null;}

# adjust /etc/hosts
# grep -Eq "^[[:space:]]*(((2[0-4][0-9]|25[0-5]|[01]?[0-9]?[0-9])\.){3}(2[0-4][0-9]|25[0-5]|[01]?[0-9]?[0-9]))([[:space:]]+)mds1([[:space:]]*)$" $HOSTFILE &&
# sed -i "s/^\(\s*\)\(\(\(2[0-4][0-9]\|25[0-5]\|[01]\?[0-9]\?[0-9]\)\.\)\{3\}\(2[0-4][0-9]\|25[0-5]\|[01]\?[0-9]\?[0-9]\)\)\(\s\+\)mds1\(\s*\)$/$1 mds1/g" $HOSTFILE || echo "$1 mds1" >> $HOSTFILE

ldconfig

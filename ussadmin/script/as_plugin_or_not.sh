#!/usr/bin/env bash

current_dir=$(dirname $0)

for i in $(ls $current_dir/../plugins)
do
    echo "disabling plugin $i..."
    bash $current_dir/../plugins/$i/script/as_plugin_or_not.sh no
done

PLUGINS_TPL=$current_dir/../conf/plugins.tpl

rm $PLUGINS_TPL
touch $PLUGINS_TPL

for i in $*
do
    echo "$i" >> $PLUGINS_TPL
    bash $current_dir/../plugins/$i/script/as_plugin_or_not.sh yes
done

[ua_main]
ua_data_path = $UA_DATA_PATH
ua_path = $UA_PATH
ear_log = /var/log/ua/ear.log
request_log = /var/log/ua/request.log
main_log = /var/log/ua/main.log
pid_file = /var/run/ua/uss_main.pid


[db_info]
db_dir = $DB_DIR
sql_dir = $SQL_DIR

[rrd_info]
rrds_dir = $RRDS_DIR
imgs_dir = static/rrd
#因为imgs_dir要被web.py使用，所以暂设为相对路径
moni_interval = 30
grapher_moni_interval = 30
updater_moni_interval = 30
rrd_step = 30
rrd_heartbeat = 60
rrdtool_log = /var/log/ua/rrdtool.log
pid_file = /var/run/ua/rrdtool.pid
grapher_pid_file = /var/run/ua/graph_rrdtool.pid
updater_pid_file = /var/run/ua/update_rrdtool.pid

[edog_info]
host = 127.0.0.1
port = 9601
sql_url = /uss/sql
misc_url = /uss/misc

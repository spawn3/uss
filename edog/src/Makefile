APPLICATION=edog
DOC_OPTS={dir,\"../doc\"}

MODS= edog_dbg\
	 edog_common\
	 edog_uuid\
	 edog_cluster\
	 edog_web\
	 edog_fg\
	 edog_os\
	 edog_yfs\
	 edog_app\
	 edog_sup\
	 edog_master\
	 edog_master_rt\
	 edog_libvirt\
	 edog_vmsup\
	 edog_mnesia\
	 edog_select\
	 edog_ops\
	 edog_load\
	 edog_slaves\
	 edog_slaves_rt

all:$(MODS:%=%.beam)

%.beam:%.erl
ifeq ($(STORAGE), proxy)
	erlc -Wall -Ddebug -Duse_proxy +debug_info -o ../ebin $<
else
	erlc -Wall -Ddebug +debug_info -o ../ebin $<
endif

edoc:
	erl -noshell -pa ../ebin \
		-eval "edoc:application($(APPLICATION), \".\", [$(DOC_OPTS)])" \
		-s init stop

clean:
	rm -rf *.beam
	rm -rf ../ebin/*.beam

INSTALL_PATH=/sysy/yfs/ussadmin
MASTER_CONF_TPL=/sysy/yfs/ussadmin/edog/conf/master.config.tpl

VSN    := $(shell awk -F'=' '{print $$2}' vsn.mk)
BRANCH := $(shell git branch|grep '^\*'|awk '{print $$2}')
COMMIT := $(shell git show|grep -i ^commit|awk '{print $$2}'|cut -c1-8)

CONF_EXIST := $(findstring $(MASTER_CONF_TPL), $(wildcard /sysy/yfs/ussadmin/edog/conf/*.tpl))

ifeq ($(CONF_EXIST), $(MASTER_CONF_TPL))
	EXCLUDES=--exclude 'master.config.tpl'
else
	EXCLUDES=
endif

vpath %.tpl /sysy/yfs/ussadmin/edog/conf


.PHONY: conf version check deps xref

all: deps dtl
	./rebar compile

dtl:
	@./script/compile.erl


deps:
	make -C deps

version:
	@echo "uss.git" > ebin/git.vsn
	@git branch|grep '^\*'|awk '{print $$2}' >> ebin/git.vsn
	@git show|grep -i ^commit|awk '{print $$2}' >> ebin/git.vsn
	@cat ebin/git.vsn

proxy:
	STORAGE=proxy make -C src

edoc:
	make -C src edoc

install: all version
	mkdir -p $(INSTALL_PATH)
	rsync -avz $(EXCLUDES) . $(INSTALL_PATH)/edog
ifeq ($(CONF_EXIST), $(MASTER_CONF_TPL))
	@echo
	@echo '############################################################'
	@echo '### /sysy/yfs/ussadmin/edog/conf/master.config.tpl exists'
	@echo '### and will not be updated. please backup it first!!!!'
	@echo '############################################################'
endif

tar: all version
	@tar cjvf ../edog-$(VSN)-$(BRANCH)-$(COMMIT).tar.bz2 ../edog

uninstall:
	rm -rf $(INSTALL_PATH)/edog

clean:
	make -C src clean
	make -C deps clean

test:
	echo make test

check:
	@python script/check_deps.py

xref:
	@escript utils/cclib_xref.erl

maint:
	erl -name maint@$(IP) -setcookie edog -hidden

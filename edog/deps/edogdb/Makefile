INSTALL_PATH=/sysy/yfs/ussadmin
OLDCONFDIR=/sysy/yfs//ussadmin/edog-oldconf

.PHONY: conf version check deps get-deps xref

all:
	./rebar compile

deps:
	make -C deps

get-deps:
	./rebar get-deps

version:
	git show|grep -i ^commit|awk '{print $$2}' > git.vsn

vsn:
	git show|grep -i ^commit|awk '{print $$2}' > git.vsn.parent

nfs:
	make -C src

proxy:
	STORAGE=proxy make -C src

edoc:
	make -C src edoc

save-conf:
	mkdir -p $(INSTALL_PATH)/edog/conf
	mkdir -p $(OLDCONFDIR)
	cp -rf $(INSTALL_PATH)/edog/conf $(OLDCONFDIR)

restore-conf:
	cp -rf $(OLDCONFDIR)/conf $(INSTALL_PATH)/edog

install:
	mkdir -p $(INSTALL_PATH)
	cp -rf ../edog $(INSTALL_PATH)

install_src:
	mkdir -p $(INSTALL_PATH)/edog
	rsync -avz src $(INSTALL_PATH)/edog/
	rsync -avz include $(INSTALL_PATH)/edog/
	rsync -avz script $(INSTALL_PATH)/edog/
	rsync -avz priv $(INSTALL_PATH)/edog/

install_ebin:
	mkdir -p $(INSTALL_PATH)
	cp -rf ebin $(INSTALL_PATH)/edog/

uninstall:
	rm -rf $(INSTALL_PATH)/edog

clean:
	@./rebar clean

test:
	echo make test

check:
	@python script/check_deps.py

xref:
	@./rebar skip_deps=true xref

maint:
	erl -name maint@$(IP) -setcookie edog -hidden

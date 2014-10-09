USSDIR=/sysy/yfs/ussadmin/ussadmin
# for release
ERLDIR=/usr/local/otp_R14B01/lib/erlang
ERLDEV=/usr/local/lib/erlang
NIF_SO=uss_nif.so

.PHONY: dev nif version clean deps

all: nif version deps
	./rebar compile

dev: dev_nif version deps
	./rebar compile

deps:
	make -C deps

nif:
	ERL_ROOT=$(ERLDIR) make -C priv/uss_nif
	mv priv/uss_nif/$(NIF_SO) ebin

dev_nif:
	ERL_ROOT=$(ERLDEV) make -C priv/uss_nif
	mv priv/uss_nif/$(NIF_SO) ebin

version:
	git show|grep -i ^commit|awk '{print $$2}' > git.vsn

vsn:
	git show|grep -i ^commit|awk '{print $$2}' > git.vsn.parent

release:
	echo "version: $(REL)" > ABOUTS
	git tag v$(REL)

install:
	mkdir -p $(USSDIR)
	rsync -avz ../ussadmin/* $(USSDIR)

ebin_install:
	rsync -avz src  $(USSDIR)
	rsync -avz test $(USSDIR)
	rsync -avz ebin $(USSDIR)

check:
	python script/check_deps.py

clean:
	./rebar clean
	make -C deps clean

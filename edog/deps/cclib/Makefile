USSDIR=/sysy/yfs/ussadmin/ussadmin
# for release
ERLDIR=/usr/local/otp_R14B01/lib/erlang
ERLDEV=/usr/local/lib/erlang
NIF_SO=uss_nif.so


.PHONY: nif version clean

all:
	./rebar compile

version:
	git show|grep -i ^commit|awk '{print $$2}' > git.vsn

xref:
	@./rebar skip_deps=true xref

clean:
	./rebar clean

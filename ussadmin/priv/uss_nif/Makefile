TARGETS=uss_nif.so
ERL_ROOT=$(shell escript get_erl_root.erl)

all:
	gcc -fPIC -shared -o uss_nif.so uss_nif.c -I${ERL_ROOT}/usr/include

clean:
	rm -f $(TARGETS)
	rm -f *.o

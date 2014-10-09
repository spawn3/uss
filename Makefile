APP_ROOT=/sysy/yfs/ussadmin/
DEPS_VERSION=20110804

.PHONY: deps get-deps

all: version
	make -C edog
	make -C ussadmin

install:
	make -C edog install
	@echo "*************************************************************"
	@echo "** enter into ussadmin(c60 version) for install if neccessary"
	@echo "*************************************************************"

version:
	make -C edog version
	make -C ussadmin version

clean:
	make -C edog clean
	make -C ussadmin clean

tar:
	make -C edog tar

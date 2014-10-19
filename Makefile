
# Before Makefile.local include so it can be used there
ARCH ?= $(shell uname)

Makefile_local = \
	$(shell if test -f Makefile.local; then echo Makefile.local; fi)

ifneq ($(Makefile_local),)
include $(Makefile_local)
endif

DESTDIR = /usr/local

ifeq ($(ARCH),Darwin)
LISP = /Applications/AllegroCL64.app/Contents/Resources/mlisp
else
LISP ?= mlisp
endif

runlisp = $(LISP) -q -batch -L build.tmp -kill

ifdef INSTALL_CONFIG_FILE
build_config = config.cl
endif

default: clean build $(build_config)

config.cl: tget-config/config.cl
	sed -e 's,"\(http.*://[^/]*/\).*","\1...",g' \
		< tget-config/config.cl > config.cl

# removed `install' since that's a little dangerous
all:	default test

build: FORCE
	cd bittorrent; make
	rm -fr tget build.tmp
	cat deliver.cl >> build.tmp
	$(runlisp)
	rm -f build.tmp
ifdef INSTALL_CONFIG_FILE
	cp -p $(INSTALL_CONFIG_FILE) tget/config.cl
endif
	cp -p seedstatus.sh tget
	@if [ README.md -nt README.html ]; then \
	    echo Building README.html; \
	    ./markdown-to-html.cl; \
	fi

.PHONY: test
test: test-lisp test-other
# Don't remove, so we can examine the database after the tests run
#	rm -fr test.db*

test-lisp: FORCE
	rm -f build.tmp
	echo '(load "load.cl")' >> build.tmp
	echo '(exit (test-tget))' >> build.tmp
	$(runlisp)

test-other: FORCE
	./test.sh

install_config: FORCE
ifdef INSTALL_CONFIG_FILE
	cp -p $(INSTALL_CONFIG_FILE) $(DESTDIR)/lib/tget/config.cl
else
	@echo INSTALL_CONFIG_FILE not defined; exit 1
endif

install: FORCE
ifdef DESTDIR
	rm -fr $(DESTDIR)/lib/tget.old
	-mv $(DESTDIR)/lib/tget $(DESTDIR)/lib/tget.old
	cp -rp tget $(DESTDIR)/lib/tget
	rm -f $(DESTDIR)/bin/tget
	cd $(DESTDIR)/bin; ln -s $(DESTDIR)/lib/tget/tget .
else
	@echo There is no DESTDIR defined in Makefile.
	@exit 1
endif

clean: FORCE
	rm -fr tget BUILD RPMS SRPMS BUILDROOT SPECS
	rm -fr main.db*
	rm -fr temp.db*
	rm -f archive.before archive.after
	rm -f *.fasl */*.fasl *.out *.log build.in *.debug build.tmp

###############################################################################

seedstatus: FORCE
	cd bittorrent; make
	rm -fr seedstatus build.tmp
	cat deliver_seedstatus.cl >> build.tmp
	$(runlisp)

install_seedstatus: FORCE
ifdef DESTDIR
	rm -fr $(DESTDIR)/lib/seedstatus.old
	-mv $(DESTDIR)/lib/seedstatus $(DESTDIR)/lib/seedstatus.old
	cp -rp seedstatus $(DESTDIR)/lib/seedstatus
	rm -f $(DESTDIR)/bin/seedstatus
	cd $(DESTDIR)/bin; ln -s $(DESTDIR)/lib/seedstatus/seedstatus .
else
	@echo There is no DESTDIR defined in Makefile.
	@exit 1
endif

###############################################################################

tags: FORCE
	etags *.cl bittorrent/*.cl

###############################################################################
FORCE:

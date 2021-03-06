
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

runlisp = $(LISP) -q -batch -backtrace-on-error -L build.tmp -kill

ifdef INSTALL_CONFIG_FILE
build_config = config.cl
endif

default: clean build $(build_config) plexfix

config.cl: tget-config/config.cl
	sed -e 's,"\(http.*://[^/]*/\).*","\1...",g' \
		< tget-config/config.cl > config.cl

# removed `install' since that's a little dangerous
all:	default test

build: FORCE
	test -d bittorrent || git clone https://github.com/e40/bittorrent
	rm -fr tget build.tmp
	cat deliver.cl >> build.tmp
	$(runlisp)
	rm -f build.tmp
ifdef INSTALL_CONFIG_FILE
	cp -p $(INSTALL_CONFIG_FILE) tget/config.cl
endif
	@if [ README.md -nt README.html ]; then \
	    echo Building README.html; \
	    ./markdown-to-html.cl; \
	fi

.PHONY: test
test: test-lisp test-other

test-lisp: FORCE
	rm -f build.tmp main.db.lock
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
	rm -fr tget plexfix
	rm -fr main.db*
	rm -fr temp.db*
	rm -f archive.before archive.after
	rm -f *.fasl */*.fasl *.out *.log build.in *.debug build.tmp

###############################################################################
# plexfix

plexfix: FORCE
	rm -fr plexfix build.tmp
	cat deliver_plexfix.cl >> build.tmp
	$(runlisp)

install_plexfix_docker: FORCE
	for f in plexfix/lib*.so plexfix/files.bu plexfix/plexfix*; do \
	    echo $$f...; \
	    docker cp $$f transmission:$(DESTDIR)/lib/plexfix/; \
	done

###############################################################################

tags: FORCE
	etags *.cl bittorrent/*.cl

###############################################################################
FORCE:

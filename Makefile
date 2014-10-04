
at_franz = $(shell if test -d /fi/cl/8.2/acl; then echo t; else echo nil; fi)

# Before Makefile.local include so it can be used there
ARCH ?= $(shell uname -i)

use_dcl = $(shell if test -f ../dcl.dxl; then echo yes; else echo no; fi)
ifeq ($(use_dcl),yes)
LISP = ../lisp -I dcl.dxl
endif

Makefile_local = \
	$(shell if test -f Makefile.local; then echo Makefile.local; fi)

ifneq ($(Makefile_local),)
include $(Makefile_local)
endif

ifeq ($(at_franz),t)
LISPROOT ?= /fi/cl/9.0
LISP ?= $(LISPROOT)/bin/$(shell if [ $(ARCH) = x86_64 ]; then echo mlisp-64; else echo mlisp; fi)
DESTDIR = /usr/fi
else
DESTDIR = /usr/local
endif

LISP ?= mlisp

runlisp = $(LISP) -q -batch -L build.tmp -kill

version := $(shell fgrep 'defvar *tget-version* ' tget.cl | sed -e 's,.*"\(.*\)".*,\1,')

tardir = tget-$(version)
tarball = tget-$(version).tar.gz

# We use FI_APPS_COMMON instead of at_franz because the CL builds don't need
# build the rpm and don't need the fi-apps-common repo.
ifeq ($(FI_APPS_COMMON),t)
ALL_EXTRA = repo_check
endif

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

ifeq ($(FI_APPS_COMMON),t)
repo_check: FORCE
	@if test ! -d fi-apps-common; then \
	    echo fi-apps-common does not exist.; \
	    exit 1; \
	fi
endif

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

tarball: FORCE
	mkdir $(tardir)
	cp -p Makefile* *.cl *.md *.sh *.spec $(tardir)
	mkdir $(tardir)/tget-config
	cp -p tget-config/config.cl $(tardir)/tget-config/config.cl
	tar zcf $(tarball) $(tardir)
	rm -fr $(tardir)

#SIGN ?= --sign

REMOVE_PREVIOUS_VERSIONS ?= no
REPOHOST                 ?= relay
REPOBASE                 ?= /tftpboot/local

REPODIR=$(REPOBASE)/$(ARCH)

release ?= $(shell . /usr/local/bin/rpm-utils.sh && rpm_next_release2 tget)

rpm:	tget.spec $(ALL_EXTRA) tarball
	mkdir -p BUILD RPMS/$(ARCH) SRPMS
	rpmbuild $(SIGN) --define "_sourcedir $(CURDIR)" \
		--define "_topdir $(CURDIR)" \
		--define "_builddir $(CURDIR)/BUILD" \
		--define "_rpmdir $(CURDIR)/RPMS" \
		--define "_srcrpmdir $(CURDIR)/SRPMS" \
		--define "version $(version)" \
		--define "release $(release)" \
		--define "mlisp $(LISP)" \
		--target $(ARCH) -ba tget.spec
	rm $(tarball)

install-repo: FORCE
ifeq ($(REMOVE_PREVIOUS_VERSIONS),yes)
	ssh root@$(REPOHOST) "rm -f $(REPODIR)/tget-*"
endif
	scp -p RPMS/$(ARCH)/tget-$(version)-*.rpm root@$(REPOHOST):$(REPODIR)
	ssh root@$(REPOHOST) "createrepo -s sha -q --update $(REPODIR)"

###############################################################################
FORCE:

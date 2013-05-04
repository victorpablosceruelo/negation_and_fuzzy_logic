# -*- mode: Makefile; -*-
##
## Makefile
##  
## Made by Edison Mera
## Login   <edison@clip.dia.fi.upm.es>
##
## Started on  Tue Aug 28 16:55:54 2007 Edison Mera
## Last update Mon Dec 28 13:27:21 2009 Edison Mera
## 

ifeq ($(shell uname -s),SunOS)
TAR=gtar
CMAKE=cmake
else

ifeq ($(shell uname -s),Linux)
CMAKE=cmake
else
ifeq ($(shell uname -s),Darwin)
CMAKE=cmake
else
ifeq ($(shell uname -o),Cygwin)
CMAKE=/usr/bin/cmake
EXECSUFFIX=.exe
else
CMAKE=cmake
endif
endif
endif

ifneq ($(shell which gnutar 2&>/dev/null),)
TAR=gnutar
else
TAR=tar
endif

endif

SRCDIR=$(CURDIR)

ifndef MPARCH
MPARCH := $(shell $(CC) -dumpmachine)
endif

include SETTINGS

ifeq ($(MPARCH),win32vc)
BUILDCMD=$(SRCDIR)/buildvc.bat
else
ifeq ($(MPARCH),win32bcc)
BUILDCMD=$(SRCDIR)/buildbcc.bat
else
BUILDCMD=$(CMAKE) ../../src && $(MAKE) $(MFLAGS)
endif
endif

# HOSTNAME?=$(shell echo $${HOSTNAME})
HOSTNAME?=$(shell hostname)

TARGET?=_$(MPARCH)

default: all

.PHONY: $(PHONY) default all clean repclean svnclean realclean tests	\
	filelist pack showinfo stats binclean config common bin

all: all_$(CROSSCOMPILE)

all_yes: CROSSCOMPCMD="all binpack"
all_yes: crosscompcmd all2_yes

all2_yes: CROSSCOMPTAR=bin
all2_yes: crosscompget

crosscompcmd:
	echo "scratchbox -d $(CROSSCOMPDIR) $(MAKE) $(MFLAGS) $(CROSSCOMPCMD)" \
	  | $(SSH) $(CROSSCOMPUSER)@$(CROSSCOMPSERVER)

crosscompget:
	mkdir -p $(CROSSCOMPTAR)pack
	scp $(CROSSCOMPUSER)@$(CROSSCOMPSERVER):$(CROSSCOMPHOME)$(CROSSCOMPDIR)/$(CROSSCOMPTAR)pack/* $(CROSSCOMPTAR)pack/
	$(MAKE) $(CROSSCOMPTAR)unpack

all_:
	@echo {generating bin files in $(HOSTNAME), please wait ...}
	mkdir -p bin/_$(MPARCH)
	cd bin/_$(MPARCH) && $(BUILDCMD)
	@echo {finished bin generation in $(HOSTNAME)}

repclean svnclean:
	$(RM) -r `svn st $(CURDIR) --no-ignore | grep "I      " \
	  | sed -e s:"I      "::g`

binclean:
	$(RM) -r bin

commonclean:
	$(RM) -r common

realclean clean: binclean commonclean $(EXTRACLEAN) crossclean_$(CROSSCOMPILE)
	$(RM) -r REVISION miniprolog.tar filelist.txt tests/*.po *.itf
	-find . -name "*~" -exec $(RM) {} \;


crossclean_yes:
	$(MAKE) CROSSCOMPCMD=clean crosscompcmd

crossclean_:

run:
	$(PROLOG) -t -C tests/ -C tests/calibs/ -C benchcommon/_all/

stats:
	-@echo "PROLOGSize=`stat -c %s $(PROLOG)`" > statistics/$(MPARCH).txt || $(RM) statistics/$(MPARCH).txt
	@(echo "consult('../tests/fib.pl')."; \
	  echo "hrtime(_T1), fib(20,N), hrtime(_T2), TimeFib is _T2 - _T1."; \
	  echo; \
	  echo "consult('../tests/hanoi.pl')."; \
	  echo "hrtime(_T1), hanoi(8,a,b,c,_N), hrtime(_T2), TimeHanoi is _T2 - _T1."; \
	  echo; \
	  echo "halt.")| \
	$(PROLOG) -s >> statistics/$(MPARCH).txt
	@(echo "consult('../tests/nreverse.pl')."; \
	  echo "consult('../tests/length.pl')."; \
	  echo "hrtime(_T1), length(X,20), nreverse(X,Y),hrtime(_T2), TimeRev is _T2 - _T1."; \
	  echo; \
	  echo "halt.")| \
	$(PROLOG) -s >> statistics/$(MPARCH).txt
	-svn diff statistics/$(MPARCH).txt
#	@echo "time of fib(25,N): " >> Statistics.txt

filelist:
	( for file in `svn ls -R` ; do \
	    if [ -f $${file} ] ; then \
	      echo miniprolog/$${file}; \
	    fi; \
	  done ) > filelist.txt
	svnversion > REVISION
	echo "miniprolog/REVISION" >> filelist.txt

pack: filelist
	$(TAR) --owner=0 --group=0 --directory=.. --files-from=filelist.txt -cf \
	  miniprolog.tar

showinfo:
	@echo MPARCH=$(MPARCH)
	@echo EXECSUFFIX=$(EXECSUFFIX)
	@echo MAKEOPTS=$(MAKEOPTS)
	@$(MAKEREC) TARGET=_$(MPARCH) OBJBASE=bin   showinfo
	@$(MAKEREC) TARGET=_$(MPARCH) OBJBASE=bench showinfo

packdir:
	mkdir -p $(AUTODIR)pack
	$(TAR) --owner=0 --group=0 --directory=$(AUTODIR) -czf \
	   $(AUTODIR)pack/$(TARGET).tar.gz $(TARGET)

unpackdir:
	mkdir -p $(AUTODIR)
	$(TAR) --directory=$(AUTODIR) -xzf $(AUTODIR)pack/$(TARGET).tar.gz

binpack:
	$(MAKE) AUTODIR=bin packdir

binunpack:
	$(MAKE) AUTODIR=bin unpackdir

get_prolog:
	@echo $(PROLOG)

get_profreport:
	@echo $(PROFREPORT)

# -*- mode: Makefile; -*-
##
## Makefile
##  
## Made by Edison Mera
##
## Started on  Wed Oct  4 16:49:45 2006 Edison Mera
## Last update Fri Jul  9 14:50:15 2010 Edison Mera
## 
## Note:
##
## Master is a machine that runs tests by extracting the copy from SVN
##
## Slave is a machine that runs tests by extracting the copy from the tar
## generated in the Master machine

# Example to generate /etc/init.d/virtualbox-vmwinxp managed by ciaotester:
# sed -e s:"\!\!SHORTNAME":vmwinxp:g -e s:"\!\!USERNAME":ciaotester:g -e s:"\!\!LONGNAME":vmwinxp:g virtualbox.skel > /etc/init.d/virtualbox-vmwinxp

# Once created, you can run the virtual machina as a service at startup using:
# update-rc.d virtualbox-vmwinxp defaults

include SETTINGS
include COMMON

.PHONY: install install_master install_slaves clean cleantest commandmaster \
	install_full install_norm testtool testtoolnohup

default: help

help:
	@echo "Usage: $(MAKE) [Command]"
	@echo "Where command is one of the following:"
	@echo
	@echo "help:           Show this help."
	@echo
	@echo "install:        Install the scripts in the master and slave machines."
	@echo
	@echo "cleantest:      Remove all logs and working copies in all machines."
	@echo "                Do not use this option in production machines."
	@echo
	@echo "kill:           Kill a test in progress."
	@echo
	@echo "reset:          Prepare versions in error status for recompilation."
	@echo
	@echo "showinfo:       Show the status of the repository and the values that"
	@echo "                some variables will take in the scripts."
	@echo
	@echo "status:         Show the status and the subversion revision number"
	@echo "                if a test is in process."
	@echo
	@echo "testnet:        Test the network connection."
	@echo
	@echo "list:           Show the list of all hosts (master and slaves)."
	@echo
	@echo "test:           Start the build in the different machines."
	@echo
	@echo "showlpv:        Show the last packed version for the trunk, i.e.,"
	@echo "                the last version whose main packaged bundles were"
	@echo "                (all of them) generated."
	@echo
	@echo "showlpvs:       Show the last packed version for the trunk and"
	@echo "                all the branches."

install: install_master install_slaves

CONFIGURECRONCMD[yes]=crontab $(BUILDROOTDIR)cronfile
CONFIGURECRONCMD[no]=crontab -r
CONFIGURECRONCMD[ignore]=true

# TODO: part DUPLICATED in SHARED!
# TODO: ../ciao/etc/ciao_get_arch is copied from the wrong place! (it should not be copied anyway)

install_master:
	sed -e s:"<BUILDROOTDIR>":"$(BUILDROOTDIR)":g \
	      -e s:"<HOSTNAME>":"$(MASTER_HOST)":g cronfile.tmpl > cronfile && \
	  sed -e s:"<SAVELOGFILES>":"yes":g runtool.sh.tmpl > runtool.sh && \
	  sed -e s:"<SAVELOGFILES>":"no":g runtool.sh.tmpl > runtoolv.sh && \
	  chmod a+x *.sh && \
	  ./sshcmd.sh "$(REMOTEACCESS_RUN_AT_PBUNDLE_REPO_HOST)" "mkdir -p $(PBUNDLE_REPO_DIR)" && \
	  cp ../ciao/etc/ciao_get_arch . && \
	  echo "NOTE: Installing scripts in $(MASTER_HOST)" && \
	  ( $(TAR) -c SETTINGS \
	    SETTINGS_$(PROFILE) \
	    cronfile \
	    SHARED \
	    COMMON \
	    ciao_get_arch \
	    childpids.sh \
	    runtool.sh \
	    runtoolv.sh \
	    sshcmd.sh \
	    scpcmd.sh \
	    timeout.sh \
	    procexists.sh \
	    virtualbox.skel \
	    filter_author.c \
	    hosts/*.cfg | ( ./sshcmd.sh "$(REMOTEACCESSMASTER)" \
	    "mkdir -p $(BUILDROOTDIR) $(LOCKDIR) $(VERSIONDIR) \
	      && cd $(BUILDROOTDIR) \
	      && $(TAR[$(MASTER_HOST)]) -x \
	      && ( $(CONFIGURECRONCMD[$(CONFIGURECRON)]) ; true )" ) )
	$(RM) cronfile runtool.sh runtoolv.sh ciao_get_arch

clean:
	$(RM) *~ *.ascii *.tmp

MAKEMASTER= \
	$(MAKE) $(MFLAGS) -f SHARED \
	SSH='$(SSH)' \
	HOSTNAME=$(MASTER_HOST) \
	TIMESTAMP=$(TIMESTAMP)

commandmaster:
	./sshcmd.sh "$(REMOTEACCESSMASTER)" "cd $(BUILDROOTDIR) && $(MAKEMASTER) $(COMMANDMASTER) $(OPTIONSMASTER) SOURCETYPE=svn"

commandmasterv:
	./sshcmd.sh "$(REMOTEACCESSMASTER)" "cd $(BUILDROOTDIR) && HOSTNAME=$(MASTER_HOST) ./runtoolv.sh $(COMMANDMASTER) $(MFLAGS) $(OPTIONSMASTER)"

commandmasterfixed:
	./sshcmd.sh "$(REMOTEACCESSMASTER)" "cd $(BUILDROOTDIR) && HOSTNAME=$(MASTER_HOST) FIXEDBRANCH=$(FIXEDBRANCH) ./runtoolv.sh $(COMMANDMASTER) $(MFLAGS) $(OPTIONSMASTER)"

testtool:
	./sshcmd.sh "$(REMOTEACCESSMASTER)" "cd $(BUILDROOTDIR) && HOSTNAME=$(MASTER_HOST) ./runtool.sh test $(MFLAGS) $(OPTIONSMASTER)"

test:
	./sshcmd.sh "$(REMOTEACCESSMASTER)" "cd $(BUILDROOTDIR) && (HOSTNAME=$(MASTER_HOST) nohup ./runtool.sh test $(MFLAGS) $(OPTIONSMASTER) > /dev/null &)" && echo ":: Running on background"

testfg:
	echo ":: Running on foreground"
	$(MAKE) $(MFLAGS) commandmasterv COMMANDMASTER=test
# 	./sshcmd.sh "$(REMOTEACCESSMASTER)" "cd $(BUILDROOTDIR) && HOSTNAME=$(HOSTNAME) ./runtool.sh test -sj"

cleantest:
	$(MAKE) $(MFLAGS) commandmasterv COMMANDMASTER=cleantest OPTIONSMASTER="CIAOVERSION= CIAOPPVERSION= LPDOCVERSION="

kill:
	$(MAKE) $(MFLAGS) commandmasterv COMMANDMASTER=kill OPTIONSMASTER="CIAOVERSION= CIAOPPVERSION= LPDOCVERSION="

reset:
	$(MAKE) $(MFLAGS) commandmasterv COMMANDMASTER=reset OPTIONSMASTER="CIAOVERSION= CIAOPPVERSION= LPDOCVERSION="

showinfo:
	$(MAKE) $(MFLAGS) commandmasterv SSH=ssh COMMANDMASTER=showinfo

# TODO: Nasty code; trunk/ branch is hardwired
showlpv:
	$(MAKE) $(MFLAGS) commandmasterfixed COMMANDMASTER=showlpv OPTIONSMASTER="CIAOPPVERSION= LPDOCVERSION=" FIXEDBRANCH=trunk

showlpvs:
	$(MAKE) $(MFLAGS) commandmasterv COMMANDMASTER=showlpv OPTIONSMASTER="CIAOPPVERSION= LPDOCVERSION="

status:
	$(MAKE) $(MFLAGS) commandmasterv SSH=ssh COMMANDMASTER=status OPTIONSMASTER="CIAOVERSION= CIAOPPVERSION= LPDOCVERSION="

list:
	$(MAKE) $(MFLAGS) commandmaster COMMANDMASTER=list OPTIONSMASTER="CIAOVERSION= CIAOPPVERSION= LPDOCVERSION="

install_slaves: install_master
	$(MAKE) $(MFLAGS) commandmaster COMMANDMASTER=install_slaves OPTIONSMASTER="NEWVERSION= CIAOVERSION= CIAOPPVERSION= LPDOCVERSION="

testnet:
	$(MAKE) $(MFLAGS) commandmaster SSH=ssh COMMANDMASTER=testnet OPTIONSMASTER="CIAOVERSION= CIAOPPVERSION= LPDOCVERSION="

doshowinfo:
	@echo MFLAGS=$(MFLAGS)
	@echo BUILDUSER=$(BUILDUSER)
	@echo BUILDROOTDIR=$(BUILDROOTDIR)
	@echo BRANCHROOTDIR=$(BRANCHROOTDIR)
	@echo SVN_REPO_USER=$(SVN_REPO_USER)
	@echo SVN_REPO_HOST=$(SVN_REPO_HOST)
	@echo SVN_REPO_ROOT=$(SVN_REPO_ROOT)
	@echo MASTER_HOST=$(MASTER_HOST)
	@echo BUILDSLAVES_SRC=$(BUILDSLAVES_SRC)
	@echo BUILDSLAVES_NOA=$(BUILDSLAVES_NOA)
	@echo BUILDSLAVES_BIN=$(BUILDSLAVES_BIN)
	@echo LOGBASE=$(LOGBASE)
	@echo CONFIGURECRON=$(CONFIGURECRON)
	@echo PBUNDLE_REPO_DIR=$(PBUNDLE_REPO_DIR)

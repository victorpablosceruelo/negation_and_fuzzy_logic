CIAOBUILDDIRtemp=../../../build
-include $(CIAOBUILDDIRtemp)/CONFIG_mkf

PROLOGSOURCES=pythonsock.pl pythonrt.pl pytopl.pl plserver.pl
PROLOGSERVER=plserver

all: 
	$(SETLOCALCIAO) $(CIAOC) -c $(PROLOGSOURCES)
	$(SETLOCALCIAO) $(CIAOC) -x -s $(PROLOGSERVER)
	-chmod  $(EXECMODE) $(PROLOGSERVER)

clean realclean:
	/bin/rm -f *.itf *.po *.dep *.asr

# Copyright (C) 1997, UPM-CLIP

CIAOBUILDDIRtemp=../../../build
include $(CIAOBUILDDIRtemp)/CONFIG_mkf

suite:
	$(SETLOCALCIAO) $(CIAOC) suite

exec_suite:
	$(SETLOCALCIAO) ./suite

clean:
	/bin/rm -rf *.itf *.dep *.po 

realclean: clean
	/bin/rm -rf suite

distclean: realclean
	@echo "*** ---------------------------------------------------------"
	@echo "*** Cleaning up tests directory."
	@echo "*** ---------------------------------------------------------"
	/bin/rm -rf *~

#*	       Copyright (C)1990-95 UPM-CLIP				*

# ****** CIAO CLP(X) systems Makefile ***************************************
# ***************************************************************************
#	qlfiles		makes every .ql file
# ***************************************************************************


SHELL=/bin/sh

PROLOG=/home/clip/Systems/ciao/ciao-SUN4

## Uncomment one
# DOMAIN=real
DOMAIN=rational

.SUFFIXES: .ql .pl

.pl.ql: expand_$(DOMAIN).pl
	@echo "compile(expand_$(DOMAIN)), fcompile($*), !." \
		| $(PROLOG) || \rm -f $*.ql
	@-chmod ugo-x $*.ql


qlfiles: $(DOMAIN).ql \
	nf.ql solve.ql nl_eval.ql simplex.ql fourier_motzkin.ql \
	compiler.ql clp_dump.ql expand.ql

$(DOMAIN).ql:	$(DOMAIN).pl
	@echo "fcompile($(DOMAIN))." \
		| $(PROLOG) || \rm -f $@
	@-chmod ugo-x $*.ql

clean:
	@rm -f *~ *#

realclean:	clean
	@rm -f *.ql

tags:
	etags -l none -r '/^[a-zA-Z0-9_]+(/' *.pl

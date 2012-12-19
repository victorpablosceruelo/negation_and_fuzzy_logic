SHELL=/bin/bash
SUBFILES=$(shell ls -a1)

# Var that contains the input arguments: $@

all:
	@echo " "
	@echo "Only implemented clean, clean_all, recursive_clean, kill_processes and mrproper."
	@echo " "

clean:
	@# Removal of tmp files
	@rm -fRv *.bbl *.bbg *.blg *.aux *.toc *.out *.brf *.lof *.nav *.snm *.vrb
	@rm -fRv *.lol *.lot *.log *.backup *.tex~ *.bib~ *.tex.* comment.cut 
	@rm -fRv *.itf *.po *.pl~ *.pl.* *_co.pl *.idx *.ast *_shfr_co.pl *_co.pl *_shfr_co*
	@rm -fRv *.xwam
	@echo " "

recursive_clean: clean
	@for i in ${SUBFILES}; do \
		if [ -d "$$i" ] && [ "$$i" != "." ] && [ "$$i" != ".." ]; then \
			cd "$$i" && $(MAKE) -i recursive_clean; \
		fi \
	done

mrproper: 
	@make -i kill_processes
	@make -i recursive_clean
	@echo " "

kill_processes: 
	@echo "-> Kill all ciao and xsb processes."
	@killall xsb ciaoengine ciaoengine.LINUXi86_64
	@echo " "

clean_all:
	cd ../debugger_pkg ; make -i mrproper  
	cd ../examples ; make -i mrproper
	cd ../manual ; make -i mrproper
	cd ../rfuzzy_ciao ; make -i mrproper
	cd ../rfuzzy_ciao_no_package ; make -i mrproper
	cd ../rfuzzy_xsb ; make -i mrproper

all:
	@echo "Only implemented clean and mrproper."
clean:
	@echo "-> Cleaning tmp files ..."
	@# Tmp files
	@rm -fRv *.bbl *.bbg *.blg *.aux *.toc *.out *.brf *.lof *.nav *.snm *.vrb
	@rm -fRv *.lol *.lot *.log *.backup *.tex~ *.bib~ *.tex.* comment.cut 
	@rm -fRv *.itf *.po *.pl~ *.pl.* *_co.pl *.idx *.ast *_shfr_co.pl *_co.pl *_shfr_co*
	@rm -fRv *.xwam
	@echo " "

mrproper: clean
	@echo "-> Kill all ciao and xsb processes."
	@killall xsb ciaoengine

cleanall:
	cd debugger_pkg ; make -i mrproper  
	cd examples ; make -i mrproper
	cd manual ; make -i mrproper
	cd rfuzzy_ciao ; make -i mrproper
	cd rfuzzy_ciao_no_package ; make -i mrproper
	cd rfuzzy_xsb ; make -i mrproper

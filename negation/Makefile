all:
	@echo "Only implemented make restart, make clean, make cleandist, make cleanall and make mrproper."
clean:
	rm -fv *_shfr_co.pl *_co.pl *_shfr_co* *.itf *.pl~ *.po *.ast *.P~ *.xwam 
cleandist: clean
	rm -fv debug_file_*.pl debug_pkg_*.pl intneg_debugging *_intneg.P .\#* \#*
cleanall:
	cd intneg; make cleandist
	cd examples; make cleandist
kill:
	killall ciaoengine; killall xsb; killall swipl;
mrproper: 
	make -i kill
	make -i cleandist

restart: kill mrproper


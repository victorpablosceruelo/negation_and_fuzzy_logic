all:
	@echo "Only implemented clean."
clean:
	rm -fv *_shfr_co.pl *_co.pl *_shfr_co* *.itf *.pl~ *.po *.ast *.P~ *.xwam intneg_debugging 
	rm -fv debug_file_*.pl
cleanall:
	cd intneg; make clean
	cd examples; make clean
clean-examples:
	rm -fv *_intneg.P 
mrproper:
	killall ciaoengine; killall xsb; killall swipl; make clean


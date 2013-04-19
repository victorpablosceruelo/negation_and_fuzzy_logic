all:
	@echo "Only implemented make clean, make cleandist, make cleanall and make mrproper."
clean:
	rm -fv *_shfr_co.pl *_co.pl *_shfr_co* *.itf *.pl~ *.po *.ast *.P~ *.xwam 1>&2
	rm -fv debug_file_*.pl debug_pkg_*.pl intneg_debugging *_intneg.P .\#* \#* 1>&2
	echo " " 1>&2

cleanall:
	cd examples; make -i -s clean

kill:
	killall ciaoengine; killall ciaoengine.LINUXi86_64; killall xsb; killall swipl;
	echo " " 1>&2

mrproper: 
	make -i -s kill
	make -i -s cleanall


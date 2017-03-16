compile:
	ciaoc -x server_rt
	ciaoc -x agents_d
	ciaoc -x examples/qsort
	ciaoc -x examples/fibo
	ciaoc -x examples/backtracking

clean:
	rm -f server_rt.itf server_rt.po server_rt
	rm -f agents_d.itf agents_d.po agents_d
	rm -f examples/qsort.itf examples/qsort.po examples/qsort
	rm -f examples/fibo.itf examples/fibo.po examples/fibo
	rm -f examples/backtracking.itf examples/backtracking.po examples/backtracking

realclean distclean: clean
	/bin/rm -f *~ *.itf *.po *.asr 
	/bin/rm -f examples/*~ examples/*.itf examples/*.po examples/*.asr 


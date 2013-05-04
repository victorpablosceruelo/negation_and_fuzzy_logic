
clean:
	'rm' -f *_co.pl *_opt.pl *.itf *.mpp *.asr *.po *.reg *.imdg
	cd db; \
	'rm' -f *_co.pl *_opt.pl *.itf *.mpp *.asr *.po *.reg *.imdg
	cd nl; \
	'rm' -f *_co.pl *_opt.pl *.itf *.mpp *.asr *.po *.reg *.imdg
	cd top; \
	'rm' -f *_co.pl *_opt.pl *.itf *.mpp *.asr *.po *.reg *.imdg

keep: clean
	cd nl; \
	for i in *.keep ; do \
		cp $$i `echo $$i | sed -e "s:.keep::"` ; \
	done

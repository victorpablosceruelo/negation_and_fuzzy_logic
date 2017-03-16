
clean:
	'rm' -f *_co.pl *_opt.pl *.itf *.mpp *.asr *.po *.reg *.imdg
	cd algebraic; \
	'rm' -f *_co.pl *_opt.pl *.itf *.mpp *.asr *.po *.reg *.imdg
	cd color; \
	'rm' -f *_co.pl *_opt.pl *.itf *.mpp *.asr *.po *.reg *.imdg
	cd csp; \
	'rm' -f *_co.pl *_opt.pl *.itf *.mpp *.asr *.po *.reg *.imdg
	cd dependency; \
	'rm' -f *_co.pl *_opt.pl *.itf *.mpp *.asr *.po *.reg *.imdg
	cd determinacy; \
	'rm' -f *_co.pl *_opt.pl *.itf *.mpp *.asr *.po *.reg *.imdg
	cd diff_equation; \
	'rm' -f *_co.pl *_opt.pl *.itf *.mpp *.asr *.po *.reg *.imdg
	cd init; \
	'rm' -f *_co.pl *_opt.pl *.itf *.mpp *.asr *.po *.reg *.imdg
	cd size; \
	'rm' -f *_co.pl *_opt.pl *.itf *.mpp *.asr *.po *.reg *.imdg
	cd solution; \
	'rm' -f *_co.pl *_opt.pl *.itf *.mpp *.asr *.po *.reg *.imdg
	cd time; \
	'rm' -f *_co.pl *_opt.pl *.itf *.mpp *.asr *.po *.reg *.imdg
	cd top; \
	'rm' -f *_co.pl *_opt.pl *.itf *.mpp *.asr *.po *.reg *.imdg

keep: clean
	cd nl; \
	for i in *.keep ; do \
		cp $$i `echo $$i | sed -e "s:.keep::"` ; \
	done

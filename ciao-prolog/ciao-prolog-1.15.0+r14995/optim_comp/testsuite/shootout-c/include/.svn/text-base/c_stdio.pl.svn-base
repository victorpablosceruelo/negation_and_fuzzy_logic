% interface to stdio.h

%:- '$native_weak_inline'(include('stdio.h')).
:- '$improlog_begin'.
:- lowinclude_foreign(predef_h, 'stdio.h').
:- pred printf1/1 + foreign([cstring], det, 'printf').
:- pred printf2/2 + foreign([cstring, iany], det, 'printf').
:- pred printf3/3 + foreign([cstring, iany, iany], det, 'printf').
:- pred printf4/4 + foreign([cstring, iany, iany, iany], det, 'printf').
:- pred printf5/5 + foreign([cstring, iany, iany, iany, iany], det, 'printf').
:- pred fprintf2/2 + foreign([ref1('FILE'), cstring, iany], det, 'fprintf').
:- pred fprintf3/3 + foreign([ref1('FILE'), cstring, iany, iany], det, 'fprintf').
:- pred fprintf4/4 + foreign([ref1('FILE'), cstring, iany, iany, iany], det, 'fprintf').
:- pred sprintf4/4 + foreign([cstring, cstring, iany, iany], det, 'sprintf').
:- pred sscanf4/4 + foreign([cstring, cstring, iany, iany], det, 'sscanf').

:- pred fread/5 + foreignfun([mut(char), intmach, intmach, ref1('FILE')], intmach, 'fread').
:- pred fwrite/4 + foreign([ref1(array(ref0(mut(char)))), intmach, intmach, ref1('FILE')], det, 'fwrite').

:- pred ferror/2 + foreignfun([ref1('FILE')], 'intmach', ferror).

:- pred perror/1 + foreign([cstring], det, 'perror').

%:- pred puts/1 + foreign([ref1(array(ref0(mut(char))))], det, 'puts').
:- pred puts/1 + foreign([ref1(array(ref0(mut(ref0(char)))))], det, 'puts').
:- pred fgets/4 + foreignfun([mut(char), intmach, ref1('FILE')], mut(char), fgets).

:- pred stdin/1 + foreigncons(ref1('FILE'), 'stdin').
:- pred stdout/1 + foreigncons(ref1('FILE'), 'stdout').
:- pred stderr/1 + foreigncons(ref1('FILE'), 'stderr').

:- '$improlog_end'.


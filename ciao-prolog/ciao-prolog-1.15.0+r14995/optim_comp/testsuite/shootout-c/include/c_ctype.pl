% interface to ctype.h

%:- '$native_weak_inline'(include('ctype.h')).
:- '$improlog_begin'.
:- lowinclude_foreign(predef_h, 'ctype.h').

:- pred toupper/2 + foreignfun([char], char, 'toupper').
:- pred tolower/2 + foreignfun([char], char, 'tolower').
:- pred isalpha/1 + foreign([char], semidet, 'isalpha').

:- '$improlog_end'.


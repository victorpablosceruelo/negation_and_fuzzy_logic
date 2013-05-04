% interface to stdlib.h

%:- '$native_weak_inline'(include('stdlib.h')).
:- '$improlog_begin'.
:- lowinclude_foreign(predef_h, 'stdlib.h').
% TODO: DET is not valid!
:- pred exit/1 + foreign([intmach], det, 'exit').
:- pred atoi/2 + foreignfun([cstring], intmach, 'atoi').
:- '$improlog_end'.


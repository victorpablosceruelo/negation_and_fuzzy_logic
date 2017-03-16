% interface to string.h

%:- '$native_weak_inline'(include('string.h')).
:- '$improlog_begin'.
:- lowinclude_foreign(predef_h, 'string.h').

:- pred strcmp/3 + foreignfun([cstring, cstring], intmach, 'strcmp').
:- pred strlen/2 + foreignfun([cstring], intmach, 'strlen').
:- pred strdup/2 + foreignfun([cstring], cstring, 'strdup'). % TODO: this calls malloc!?

:- '$improlog_end'.


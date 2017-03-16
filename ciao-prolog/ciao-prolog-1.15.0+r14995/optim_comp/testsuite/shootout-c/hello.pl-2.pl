:- module(_, [], ['$purest']).
:- include(.(include(common))).

:- '$native_weak_inline'(include('engine/hello.pl-2.native.h')).
:- '$native_weak_inline'(include('unistd.h')).

:- '$improlog_begin'.
:- pred write/4 + foreignfun([intmach, mut(char), intmach], intmach, 'write').

:- pred begin/1 + lowentry(det, [intmach], 'begin') + prop(foreign__static).
begin(_Arg) :-
	_ = ~write(1, "hello world\n", 12).

:- '$improlog_end'.


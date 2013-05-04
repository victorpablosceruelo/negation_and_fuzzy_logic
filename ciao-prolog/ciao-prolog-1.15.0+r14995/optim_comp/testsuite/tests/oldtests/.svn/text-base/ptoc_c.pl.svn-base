% test...

:- module(_, _, []).

:- use_module(library(strings)).

:- '$preddef'(a/1, ptoc).
:- '$ptoc_prop'('ptoc_c:a'/1, [imp = semidet, register = true]).
a(X) :-
%	A = a, B = b,
	X = f(_, _), !,
	A = 10.2000000000,
	B = 100000000000000000000000000000000000000,
%	X = f(A, f(g, h(1,2,3,4))).
	X = f(A,B),
	B = 100000000000000000000000000000000000000.
%a(X) :-
%	X = a.

/*
p(X, Y) :-
	X = a, !,
	Y = b.
p(X, Y) :-
	Y = a.
*/

/*
:- '$preddef'(p/1, ptoc).
:- '$ptoc_prop'('ptoc_c:p'/1, [imp = nondet, register = false, indexed = false]).
p(1).
p(2).
p(3).
*/

q.

:- '$preddef'(b/1, ptoc).
:- '$ptoc_prop'('ptoc_c:b'/1, [imp = nondet, indexed = false, register = true]).
b(X) :- get_arch(X), mydisplay(X).

%:- '$nativedef'(mydisplay/1, cbool(prolog_display)).
mydisplay(X) :- display(X).


%:- '$preddef'(p/0, ptoc).
% TODO: devise a method to write prototypes of C functions of predicates
%       (do it for each used funcion or do it for each imported one (?))
% TODO: in compiler/frontend.pl, get ptoc_prop from nativedefs... ?? make public that part of definitions

:- '$preddef'(p/0, ptoc).
:- '$ptoc_prop'('ptoc_c:p'/0, [imp = nondet, indexed = false, register = true]).
p.
p.
p.

m :-
	p,
	display(jeje),
	nl,
	fail.
m.

pc.
pc.

mc :-
	pc,
	display(jejec),
	nl,
	fail.
mc.
%:- '$preddef'(q/2, ptoc).
%:- '$ptoc_prop'('ptoc_c:q'/2, [imp = semidet, indexed = false, register = true]).
%q(A,B) :- B is A + 1.


:- '$preddef'(s/2, ptoc).
:- '$ptoc_prop'('ptoc_c:s'/2, [imp = semidet, indexed = false, register = true]).
s(X, Y) :- Y is X + 1.

:- '$preddef'(t/2, ptoc).
:- '$ptoc_prop'('ptoc_c:t'/2, [imp = semidet, indexed = false, register = true, call_types=[smallint, var]]).
t(X, Y) :- Y is X + 1.

:- '$nativedef'(jejeblt/2, cblt(jejeblt_c,34,m(0,0))).
:- '$nativedef'(jejefun/2, cfun('jejefun'/1,jejefun_c,0,yes)).

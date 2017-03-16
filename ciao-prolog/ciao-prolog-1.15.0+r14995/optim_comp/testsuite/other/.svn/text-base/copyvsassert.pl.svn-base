:- module(_, _, []).

% Tests copy-term versus assert. 

% ?- do.
% t(a,a1000,4226.357000000004).
% t(c,c1000,2281.652999999991).
% t(a,a1,35.9950000000099).
% t(c,c1,7.998999999996158).

% copy_term vs assert benchmark
:- use_module(library(lists)).
:- use_module(library(prolog_sys)).

'$cputime'(X) :- statistics(runtime, [X|_]).

do :-
	length(X0, 1000),
	append(X0, X0, X),
	try(10000, a1000, a, X),
	try(10000, b1000, b, X),
	try(10000, c1000, c, X),
	try(10000, d1000, d, X),
	ex(Z),
	try(10000, az, a, Z),
	try(10000, bz, b, Z),
	try(10000, cz, c, Z),
	try(10000, dz, d, Z),
	length(Y, 1),
	try(10000, a1, a, Y),
	try(10000, b1, b, Y),
	try(10000, c1, c, Y),
	try(10000, d1, d, Y).

ex((try(Count, Name, What, Data) :-
	'$cputime'(T1),
	loop(Count, What, Data),
	'$cputime'(T2),
	'$cputime'(T1),
	loop(Count, What, Data),
	'$cputime'(T2),
	'$cputime'(T1),
	loop(Count, What, Data),
	'$cputime'(T2),
	'$cputime'(T1),
	loop(Count, What, Data),
	'$cputime'(T2),
	'$cputime'(T1),
	loop(Count, What, Data),
	'$cputime'(T2),
	'$cputime'(T1),
	loop(Count, What, Data),
	'$cputime'(T2),
	Time is T2-T1,
	display(t(What, Name, Time)), display('.'), nl)).

try(Count, Name, What, Data) :-
	'$cputime'(T1),
	loop(Count, What, Data),
	'$cputime'(T2),
	Time is T2-T1,
	display(t(What, Name, Time)), display('.'), nl.

loop(Count, What, Data) :-
	repeat(Count),
	b(What, Data),
	fail.
loop(_, _, _).

repeat(_N).
repeat(N) :-
	N > 1,
	N1 is N - 1,
	repeat(N1).

:- data a/1.

b(a, L) :-
	asserta_fact(a(L)),
	retract_fact(a(L2)).
b(b, L) :-
	asserta_fact(a(L)),
	current_fact(a(_L2)),
	retract_fact(a(L)).
b(c, L) :-
	copy_term(L, _L2),
	copy_term(L, _L3).
b(d, L) :-
	copy_term(L, L2),
	L = L2.

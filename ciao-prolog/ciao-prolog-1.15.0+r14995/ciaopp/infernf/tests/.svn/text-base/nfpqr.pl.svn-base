:- module(nfpqr, [p/0], [assertions, nativeprops, regtypes]).

:- use_module(library(operators)).
:- use_module(library(write)).
:- use_module(library(aggregates)).
:- use_module(library(ttyout)).

p :-
	2=2,
	r.

% q :-
% 	p.

% put here all the builtins you like to test, but in such a way that don't fail:

r :-
	2=2,
	2==2,
	2\==3,
	2=:=2,
	2=\=3,
	5>3,
	2<3,
	3=<3,
	3>=3,
	arithexpression(2 + 3*2/7.0),
	number(1),
	integer(2),
	num(1),
	int(1),
	atm(1),
	var(A),
	ground(a),
	float(1.0),
	atom(a),
	atomic(1),
	gnd(f(a)),
	get_code(A),
	current_op(_B, _C, _D),
	functor(f(a, b), _F, _N),
	findall(X, fa(X), _L),
	arg(3, f(a, b, c, d), _Arg),
	_R is 2 +2,
	4 is 2 +2,
	!,
	nl,
	ttynl,
	ttyput(3),
	write(a),
	tab(2),
	writeq(a),
	display(b),
	print(a),
	true,
	fail,
	false,
	\+(false),
	indep(a),
	indep(_A, _B).

fa(a).
fa(b).
fa(c).

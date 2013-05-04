% A test for instantiable modules based on context variables
%
% Author: Jose F. Morales

:- module(_, [main/0], [assertions, 'compiler/compiler_object']).

:- use_module(library(write)).
:- use_module(library(prolog_sys)).
:- use_module(library(aggregates)).
:- use_package(hiord).
:- use_module(.(f)).

:- export(test/0).
test :-
	test_3,
	test_2(X, (_:g(A),p(A,P), display(P), nl, P(X)), Xs),
	display(Xs), nl.

test_3 :-
	Y = call(display, got),
	Y,
	Z = e:g(X),
	Z,
	display(j), nl,
	W = _:g(A),
	W,
	display((X, A)), nl.

:- meta_predicate test_2(?, goal, ?).
test_2(X, G, Xs) :-
	findall(X, G, Xs).

:- meta_predicate g(spec).
g(X) :- X = g/1.
g(g/1).

:- meta_predicate p(pred(1)).
p(A,(''(X) :- true,current_module(X0),X=f(A,X0))).

main :-
	this_module(M),
	display(thismod(M)), nl,
	e:foo,
	f:foo.

:- '$context'(foo/0, module).
foo :-
	'$this'(X),
	display(X), nl.

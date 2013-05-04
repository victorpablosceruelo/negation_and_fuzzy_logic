:- module(foo, [], [assertions, regtypes, nativeprops]).

:- doc(module, "Simple test for type inference").

:- doc(author, "Jose F. Morales").

:- doc(bug, "bug: e-terms loses precision when the terms are build
	top-down (@term{X=f(Y), Y=a}) instead of bottom-up (@term{Y=a,
	X=f(Y)}).").

:- export(expr1/1).
expr1(X) :- number(X).
expr1(X+Y) :- expr1(X), expr1(Y).
expr1(X*Y) :- expr1(X), expr1(Y).

:- export(expr2/1).
expr2(X) :- number(X).
expr2(Z) :- Z = X+Y, expr2(X), expr2(Y).
expr2(Z) :- Z = X*Y, expr2(X), expr2(Y).

:- export(expr3/1).
expr3(X) :- number(X).
expr3(Z) :- expr3(X), expr3(Y), Z = X+Y.
expr3(Z) :- expr3(X), expr3(Y), Z = X*Y.

:- export(foo1/1).
foo1([1,2,3]).
:- export(foo2/1).
foo2(X) :- X = [1,2,3].
:- export(foo3/1).
foo3(X) :- X = [1|Y], Y = [2|Z], Z = [3].
:- export(foo4/1).
foo4(X) :- Z = [3], Y = [2|Z], X = [1|Y].

:- export(mrec/1).
mrec(f(A)) :- mrec2(A).
mrec(n).

mrec2(B) :- B = g(A), mrec(A).

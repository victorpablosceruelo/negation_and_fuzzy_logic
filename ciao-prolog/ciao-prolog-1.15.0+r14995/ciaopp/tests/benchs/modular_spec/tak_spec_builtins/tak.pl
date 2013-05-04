:- module(_, _, [assertions]).

:- export(tak/4).
:- entry tak(A,B,C,D)
          : ( ground(A), ground(B), ground(C), var(D) ).


:- use_module(fast_unif).
:- use_module(fast_arith).

tak(X,Y,Z,A) :-
	le(X, Y), !,
	unif(Z, A).
tak(X,Y,Z,A) :-
	% X > Y,
	sub(X, 1, X1),
	tak(X1,Y,Z,A1),
	sub(Y, 1, Y1),
	tak(Y1,Z,X,A2),
	sub(Z, 1, Z1),
	tak(Z1,X,Y,A3),
	tak(A1,A2,A3,A).


/* Original program

tak(X,Y,Z,A) :-
	X =< Y, !,
	Z = A.
tak(X,Y,Z,A) :-
	% X > Y,
	X1 is X - 1,
	tak(X1,Y,Z,A1),
	Y1 is Y - 1,
	tak(Y1,Z,X,A2),
	Z1 is Z - 1,
	tak(Z1,X,Y,A3),
	tak(A1,A2,A3,A).

*/


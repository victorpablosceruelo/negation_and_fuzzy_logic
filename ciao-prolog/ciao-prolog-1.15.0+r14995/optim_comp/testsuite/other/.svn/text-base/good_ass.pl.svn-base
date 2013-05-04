:- module(_, _, [assertions]).

:- export(tak/4).

:- trust pred sub(A, B, C) : int * int * var
   + equiv(fast_sub(A,B,C)).
:- trust pred sub(A, B, C) + equiv(C is A - B).
:- impl_defined(sub/3).
:- impl_defined(fast_sub/3).

:- trust pred le(A, B) : int * int
   + equiv(fast_le(A,B,C)).
:- trust pred le(A, B) + equiv(A =< B).
:- impl_defined(le/2).
:- impl_defined(fast_le/2).

:- trust pred unif(A, B) : int * var
   + equiv(unif_int_var(A,B)).
:- trust pred unif(A, B) + equiv(A = B).
:- impl_defined(unif/2).
:- impl_defined(unif_int_var/2).

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

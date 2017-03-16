:- module(fast_arith,[sub/3,fast_sub/3,le/2,fast_le/2]).

:- use_package(assertions).

:- trust pred sub(A, B, C) : ground * ground * var
   => ground * ground * ground + equiv(fast_sub(A,B,C)).
:- trust pred sub(A, B, C) => ground * ground * ground. 

sub(A,B,C):-
	C is A - B.

:- impl_defined(fast_sub/3).

:- trust pred le(A, B) : ground * ground
   + equiv(fast_le(A,B,C)).
:- trust pred le(A, B) => ground * ground.

:- impl_defined(fast_le/2).

le(A,B):-
	A =< B.



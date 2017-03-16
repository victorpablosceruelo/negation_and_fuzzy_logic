:- module(divide, [divide/3], [assertions, nativeprops, predefres(res_all)]).

:- entry divide(A, B, C) : ( list(A, num), var(B), var(C), mshare([[
			B], [C]]) ).

divide([],         [],      []).
divide([X],        [X],     []).
divide([X1, X2|L], [X1|L1], [X2|L2]) :-
	divide(L, L1, L2).

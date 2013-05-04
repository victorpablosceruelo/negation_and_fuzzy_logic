:- module(powset, [powset/2, int_list/1], [assertions, regtypes, nativeprops,
		ciaopp(examples(resources(exectimehl)))]).

:- entry powset(A, B) : int_list * var.

:- regtype int_list/1.

int_list([]).
int_list([X|L]) :-
	int(X),
	int_list(L).

% This is the optimized version of powerset:
% :- entry powset2( A, B, C ) : int_list * list * var.
% powset2( [],        X,  X ).
% powset2( [ X | L ], P0, P ) :-
% 	append_elem( P0, X, P1, P0 ),
% 	powset2( L, P1, P ).

powset([],    [[]]).
powset([X|L], P) :-
	powset(L, P0),
	append_elem(P0, X, P, P0).

append_elem([],     _X, T,          T).
append_elem([L|Ls], X,  [[X|L]|Rs], T) :-
	append_elem(Ls, X, Rs, T).

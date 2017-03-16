:- module(_, [int_list/1], [assertions, regtypes]).

:- regtype int_list/1.

int_list([]).
int_list([X|L]) :-
	int(X),
	int_list(L).

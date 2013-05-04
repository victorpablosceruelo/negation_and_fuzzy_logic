:- module(test,[test/1],[assertions]).

:- use_module(same_length).
:- use_module(mylists).

test(L):-
	same_length([1,2,3],L),
	length(L,s(s(s(0)))),
	same_length(L,[4,5,6]).

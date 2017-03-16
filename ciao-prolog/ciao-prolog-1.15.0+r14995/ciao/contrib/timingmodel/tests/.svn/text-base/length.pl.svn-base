length(X, N) :-
	length2(X, 0, N).

length2([],    N,  N) :- !.
length2([X|L], N0, N) :-
	N1 is N0 + 1,
	length2(L, N1, N).

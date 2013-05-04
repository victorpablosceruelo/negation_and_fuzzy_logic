:- module(_test,[test/1],[]).


test(L):-
	same_length([1,2,3],L),
	length(L,s(s(s(0)))),
	same_length(L,[4,5,6]).

same_length(X,Y):-
	length(X,N),
	length(Y,N).

length([],0).
length([_|Xs],s(N)):-
	length(Xs,N).

% length(L, N) :- var(N), !, llength(L, 0, N).
% length(L, N) :- dlength(L, 0, N).

% llength([], I, I).
% llength([_|L], I0, I) :- I1 is I0+1, llength(L, I1, I).

% dlength([], I, I) :- !.
% dlength([_|L], I0, I) :- I0<I, I1 is I0+1, dlength(L, I1, I).





main :-
	p(15000000, X),
	k(X).

k(_).

p(0,[]).
p(N,[a|T]) :- 
	!,
	N1 is N-1,
	p(N1,T).

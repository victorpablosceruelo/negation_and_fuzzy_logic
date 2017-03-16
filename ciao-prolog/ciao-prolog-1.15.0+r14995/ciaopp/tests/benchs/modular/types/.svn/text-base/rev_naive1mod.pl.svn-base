:- module(rev_naive1mod,[rev/2],[]).

rev([],[]).
rev([X|Xs],Ys):-
	rev(Xs,Ys0),
	append(Ys0,[X],Ys).

append([], L, L).
append([E|Es], L, [E|R]) :- append(Es, L, R).



	

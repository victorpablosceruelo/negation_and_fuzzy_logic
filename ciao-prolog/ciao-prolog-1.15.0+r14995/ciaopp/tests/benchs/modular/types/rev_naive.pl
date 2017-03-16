:- module(rev_naive,[rev/2],[assertions]).

:- use_module(app).

rev([],[]).
rev([X|Xs],Ys):-
	rev(Xs,Ys0),
	append(Ys0,[X],Ys).



	

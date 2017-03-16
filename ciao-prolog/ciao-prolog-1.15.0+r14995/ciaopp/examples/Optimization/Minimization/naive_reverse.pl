:- module(_rev,[rev/2],[assertions]).

:- entry rev([1,2,3,4,5,6|L],R).
%:- entry rev(L,R) : ground(L).

rev([],[]).
rev([X|Xs],R):-
	rev(Xs,R1),
	app(R1,[X],R).

app([],X,X).
app([H|X],Y,[H|Z]):- app(X,Y,Z).

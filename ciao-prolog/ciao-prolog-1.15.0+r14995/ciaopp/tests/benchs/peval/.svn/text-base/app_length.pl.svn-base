
:- module( app_length, [myapp/2], [assertions] ).

:- entry myapp(Bs,Cs)
	: (list(Bs), var(Cs)).

myapp(L,Length):- append([a,b],L,R), length(R,Length). 


append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).

length([],0).
length([_|Xs],L):-
	length(Xs,L1),
	L is L1 + 1.

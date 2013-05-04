
:- module( _app, [myapp/2], [assertions] ).

:- entry myapp(Bs,Cs)
	: (var(Cs)).

myapp(L,R):- append([a,b],L,R), append([a,b],L,R).

append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).



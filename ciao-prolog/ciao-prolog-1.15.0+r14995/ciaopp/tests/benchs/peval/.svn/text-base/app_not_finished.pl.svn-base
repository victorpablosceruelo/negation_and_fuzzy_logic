
:- module( _app, [myapp/3], [assertions] ).

:- entry myapp(As,Bs,Cs)
	: (var(Cs)).

myapp(A,B,C):- append([a,b|A],B,C).

append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).





:- module( miniapp, [myapp/2], [assertions] ).

:- entry myapp(Bs,Cs)
	: (list(Bs), var(Cs)).

myapp(L,R):- append(L,L,R). 


append([],X,X).
%append([H|X],Y,[H|Z]):- append(X,Y,Z).



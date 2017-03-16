:- module( _ , [app/3], [assertions]).

:- pred app(A,B,C) : (list(A),list(B)) => list(C).



app([],X,X).
app([H|X],Y,[H|Z]):- app(X,Y,Z).

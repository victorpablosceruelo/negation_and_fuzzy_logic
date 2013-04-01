
:- module(proof3,_,[.(cneg)]).
 

p(X) :- X=a.
p(X) :- r(X).
r(b).


% Ejemplo sacado de Stuckey95
proof3(X):-
	dist(X,c),
	cneg((p(X),q(X))).

proof31(X):-
	dist(X,c),
	cneg(p(X)).
proof31(X):-
	dist(X,c),
	cneg(q(X)).

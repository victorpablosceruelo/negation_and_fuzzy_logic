
:- module(proof1,_,[.(cneg)]).

:- use_module(dist). 

p(a,b,c).
p(b,a,c).
p(c,a,b).

% Ejemplo sacado de Stuckey95
proof1(X,Y,Z):-
	dist(X,a),
	Z=c,
	cneg(p(X,Y,Z)).

q(X,Y):-
	p(Y,X,Z).

proof2(X,Y,Z):-
	cneg(p(X,Y,Z)).

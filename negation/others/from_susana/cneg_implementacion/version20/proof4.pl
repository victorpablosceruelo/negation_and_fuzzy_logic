
:- module(proof4,_,[.(cneg)]).
 

p(X) :- X=s(T), q(T).
 
q(X) :- q(X).

r(X) :- cneg(p(X)).


% Ejemplo sacado de Stuckey95

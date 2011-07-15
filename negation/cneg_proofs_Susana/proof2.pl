
:- module(proof2,_,[.(cneg)]).
 
% Ejemplo sacado de Stuckey95


q(T):- q(T).
no_q:-cneg(q(T)).  %% no existe T tal que q(T)
no_q(T):-cneg(q(T)). %% existe T tal que no q(T)

p(X):- X = s(T), q(T).
no_p:- cneg(p(X)).
no_p(X):- cneg(p(X)).

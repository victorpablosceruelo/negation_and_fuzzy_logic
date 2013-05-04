:- module(_,[main/1],[]).
:- use_package(assertions).

:- use_module(library(aggregates)).

main(L):-
%% 	p.
%% 
%% p:- 
%	X = a,
	findall(Y, test_findall:q(X,Y), L).

:- trust comp q(_,_) + (eval, sideff(free)).

q(a,b).
q(b,c). 

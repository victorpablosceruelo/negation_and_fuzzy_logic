:- module(_,[p/2],[]).

:- use_module(library(write)).

%% :- use_package(assertions).
%% 
%% :- entry p(X,Y) : (ground(X), var(Y)).

p(Value,Res):- q(Tmp, Value), Tmp2 is Tmp + 3, q(Tmp2,Res).

q(A,B):-
	% may do other things as well
	other(A,B),
	plus1(A,B).

other(A,B):- write(A), write(B).

plus1(X,Y):- ground(X), Y is X + 1.
plus1(X,Y):- var(X), X is Y - 1.

:- module(metacalls, [main1/1, main2/1, main3/1], [assertions]).

:- use_module(mylists2).
:- use_module(library(aggregates)).

%:- entry main1(A) : ground(A).
main1(X):-
	call(q(X)).

%:- entry main2(A) : ground(A).
main2(X):-
	findall(Y, p(Y,X), X).

%:- entry main3(A) : ground(A).
main3(X):-
	q(X),
	\+ s(X).

p(Y,X):- mymember(Y,X).

q(X) :- length(X,2).

s(X) :- length(X,N), N > 2.


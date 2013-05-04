:- module(_,[main/2]).

:- use_package(assertions).

%:- entry main(s(s(s(L))),R) : (ground(L),var(R)).
:- entry main(L,R) : (ground(L),var(R)).

main(X,X2):-
	ground(X),
	var(W),
	two(T),
	minus(T,X,X2),
	twice(X2,W).
 	formula(X1,X2).

formula(X,W):-

two(s(s(0))).

minus(0,X,X).
minus(s(X),s(Y),R):-
	minus(X,Y,R).
minus(s(_X),0,_R).

twice(X,_Y):-
	var(X).
twice(X,Y):-
	ground(X),
	tw(X,Y).

tw(0,0).
tw(s(X),s(s(NX))):-
	tw(X,NX).

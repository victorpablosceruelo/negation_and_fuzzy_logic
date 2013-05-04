:- module(_,[main/2],[]).

:- use_package(assertions).

:- entry main(s(s(s(L))),R) : (ground(L),var(R)).
%:- entry main(L,R) : (ground(L),var(R)).

main(In,Out):-
	formula(In,Tmp),
 	formula(Tmp,Out),
	ground(Out).

formula(X,W):-
	ground(X),
	var(W),
	two(T),
	minus(X,T,X2),
	twice(X2,W).

two(s(s(0))).

minus(X,0,X).
minus(s(Y),s(X),R):-
	minus(Y,X,R).
minus(0,s(_X),_R).

twice(X,_Y):-
	var(X).
twice(X,Y):-
	ground(X),
	tw(X,Y).

tw(0,0).
tw(s(X),s(s(NX))):-
	tw(X,NX).

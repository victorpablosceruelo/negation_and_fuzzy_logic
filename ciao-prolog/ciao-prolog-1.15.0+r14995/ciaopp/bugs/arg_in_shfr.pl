/*
 This example shows in p/2 how a call to arg with 
 the second argument instantiated to a functor
 produces bottom in shfr, while q/2, equivalent
 to the previous predicate but with a variable
 in the second argument of the call, gives correct
 results. 
*/
:- module(_,_,[assertions]).

:- entry p(A,B) : ground(A).
:- entry q(A,B) : ground(A).

p(X,Z):-
	arg(X,f(_A,_B,_C),Z).

q(X,Z):-
	Y = f(_A,_B,_C),
	arg(X,Y,Z).

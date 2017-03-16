%------------------------------------------------------------------------------
%	Benchmark Program - Takeuchi 
%	Independent AND-parallel version
%
%	Copyright by Evan Tick and Manuel Hermenegildo
%	Date: December 17 1987
%
%	To test: run test/0 .  Should print "7".
%------------------------------------------------------------------------------

:- module(tak,[tak/4], [ assertions ]).

:- entry tak/4 : num * num * num * var.

goal(X) :- tak(14,12,6,X). % takes finite amount of time, should give 7
/* if you want it to run longer, use tak(18,12,6,X). */


tak(X,Y,Z,A) :-
   X =< Y, !,
   Z = A.
tak(X,Y,Z,A) :- 
   X1 is X - 1,
   Y1 is Y - 1,
   Z1 is Z - 1,
   tak(Z1,X,Y,A3),
   tak(Y1,Z,X,A2),
   tak(X1,Y,Z,A1),
   tak(A1,A2,A3,A).

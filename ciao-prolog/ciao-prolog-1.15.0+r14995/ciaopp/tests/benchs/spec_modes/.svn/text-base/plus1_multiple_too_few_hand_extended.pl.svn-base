:- module(_,[p/2]).

p(Value,Res):- q_1(Tmp, Value), Tmp2 is Tmp + 3, q_2(Tmp2,Res).

q_1(A,B):-
	% may do other things as well
	other(A,B),
	plus1_1(A,B).

q_2(A,B):-
	% may do other things as well
	other(A,B),
	plus1_2(A,B).

other(A,B):- write(A), write(B).

plus1_1(X,Y):- ground(X), Y is X + 1.
plus1_1(X,Y):- var(X), X is Y - 1.

plus1_2(X,Y):- ground(X), Y is X + 1.
plus1_2(X,Y):- var(X), X is Y - 1.

:- module(success_rt, [top/0], [assertions]).


:- entry top.

top:-
	data1(D1),
	compute(D1,Res1), % violates assertion for compute
	show_result(Res1).


data1(2).


:- success compute(X,Y) => valid_number(X).

valid_number(X):- number(X), X > 3, X < 7.

compute(X,X).

show_result(_X):-
	% ...
        true.

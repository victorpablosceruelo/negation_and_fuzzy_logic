:- module(calls_rt, [top/0], [assertions]).


:- entry top/0.

top:-
 	data1(D1),
 	compute(D1,Res1),
 	show_result(Res1),
	data2(D2),
	compute(D2,Res2),  % violates assertion for compute!
	show_result(Res2).

data1(4).

data2(8).

:- prop valid_number/1.

valid_number(X):- number(X), X > 3, X < 7.

:- calls compute(X,Y) : valid_number(X).

compute(X,X).

show_result(_X):-
	% ...
        true.

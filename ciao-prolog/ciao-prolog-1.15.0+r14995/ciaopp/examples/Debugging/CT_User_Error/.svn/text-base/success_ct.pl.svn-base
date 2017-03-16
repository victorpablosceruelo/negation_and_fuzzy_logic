:- module(success_ct,[top/0],[assertions,regtypes]).


:- entry top.

top:-
	data1(D1),
	compute(D1,Res1), % violates assertion for compute
	show_result(Res1).


data1(4).


:- success compute(X,Y):true => type1(X).

:- regtype type1/1.

type1(a).

compute(X,X).

show_result(_X):-
	% ...
        true.

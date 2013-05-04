:- module(calls_ct,[top/0],[assertions,regtypes]).

:- entry top/0.

top:-
 	data1(D1),
 	compute(D1,Res1),
 	show_result(Res1),
	data2(D2),
	compute(D2,Res2),  % violates assertion for compute!
	show_result(Res2).

data1(a).

data2(4).

:- calls compute(X,Y) : type1(X).

:- regtype type1/1.

type1(a).

compute(X,X).

show_result(_X):-
	% ...
        true.

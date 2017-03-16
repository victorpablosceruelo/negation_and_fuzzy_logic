:- module(check_pp, [top/0], [assertions,rtchecks]).

:- entry top.

top:-
	X is 1,
%	check(valid_number(X),pr_pp('end of predicate p')).
	check(valid_number(X)).


valid_number(X):- number(X), X > 3, X < 7.

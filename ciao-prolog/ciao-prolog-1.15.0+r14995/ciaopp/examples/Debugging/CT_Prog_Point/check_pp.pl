:- module(check_pp,[p/0],[assertions,regtypes,rtchecks]).

:- entry p.

p:-
	X is 1,
%	check(mytype(X),pr_pp('end of predicate p')).
	check(mytype(X)).

:- regtype mytype/1.
mytype(a).

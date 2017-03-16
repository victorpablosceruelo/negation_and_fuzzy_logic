:- module(_, _, []).

:- use_module(engine(hiord_rt)).

kk(X, Y) :-
	a, (c,d ; X:Y).

:- meta_predicate jj(pred(0), pred(0)).

jj(X, Y) :-
	a, (c,d ; X, Y).

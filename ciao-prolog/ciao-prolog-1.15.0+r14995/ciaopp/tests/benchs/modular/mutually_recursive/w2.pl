:- module(w2,[q/1],[ assertions,nativeprops]).

:- use_module(w1, [p/1]).


q([]).
q([X|Xs]) :-
	p(X),
	q(Xs).





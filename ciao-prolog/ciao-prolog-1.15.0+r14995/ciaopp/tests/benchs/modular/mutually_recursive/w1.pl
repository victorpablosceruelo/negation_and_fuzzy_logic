:- module(w1,[p/1],[ assertions,nativeprops]).

:- use_module(w2, [q/1]).


p([]).
p([X|Xs]) :-
	q(X),
	p(Xs).





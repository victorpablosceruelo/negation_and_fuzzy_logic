:- module(partition, [partition/4], [assertions]).

:- use_module(engine(arithmetic), [(<)/2, (>=)/2]).

:- entry partition(A,B,C,D) : list(num) * num * var * var.

partition([],_,[],[]).
partition([E|R],C,[E|Left1],Right):-
	E < C, !,
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
	E >= C,
	partition(R,C,Left,Right1).

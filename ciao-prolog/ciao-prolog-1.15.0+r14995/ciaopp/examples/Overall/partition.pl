
:- module( partition, [partition/4], [assertions] ).

partition([],_,[],[]).
partition([E|R],C,[E|Left1],Right):- E < C, !,
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):- E >= C,
	partition(R,C,Left,Right1).

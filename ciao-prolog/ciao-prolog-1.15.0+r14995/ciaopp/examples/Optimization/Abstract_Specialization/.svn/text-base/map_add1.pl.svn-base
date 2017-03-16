:- module(map_add1, [map_add1/2], []).

map_add1([],_).
map_add1([X|Xs],Y):-
	(nonvar(Y) -> 
	    X is Y + 1, 
	    map_add1(Xs,Y)
	;
	    Y is X - 1,
	    map_add1(Xs,Y)).

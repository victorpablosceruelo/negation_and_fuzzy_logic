:- module(bu_set, [add/2, collect/2]).

:- data elem/2.

add(K, V):-
	(
	    var(K)
	->
	    throw(instanciation_error(bag:add/2-1))
	;
	    elem(K, V)
	-> 
	    true
	;
	    assertz_fact(elem(K, V))
	).

collect(K, V):-
	(
	    var(K)
	->
	    throw(instanciation_error(bag:collect/2-1))
	;
	    collect_loop(K, V)
	).

collect_loop(K, L):-
	(
	    retract_fact(elem(K, X)) ->
	    L = [X|T],
	    collect_loop(K, T)
	;
	    L = []
	).
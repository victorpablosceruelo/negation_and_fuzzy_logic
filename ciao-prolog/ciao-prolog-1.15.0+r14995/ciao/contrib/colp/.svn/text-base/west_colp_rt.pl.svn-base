:- module(west_colp_rt, [in_stack/2, not_in_stack/2, getval/2, setval/2, mytrace/2], []).

:- use_module(library(global_vars), [getval/2, setval/2]).

:- use_module(library(write)).
:- use_module(library(format)).

setval(X, Y):- 
	global_vars:setval(X, Y).
getval(X, Y):-
	global_vars:getval(X, Y), 
	(Y=[], !; Y=[_|_]).

in_stack(G,[G|_]).
in_stack(G,[_|T]) :- in_stack(G,T).

not_in_stack(_G,[]).
not_in_stack(G,[H|T]) :- G \== H, not_in_stack(G,T).

'i++'(I):-
	global_vars:getval(i, I),
	(
	    I = 0 ->
	    global_vars:setval(i, 1)
	;
	    J is I + 1, 
	    global_vars:setval(i, J)
	).

:- meta_predicate(mytrace(?,:)).

mytrace(G, MG):-
	'i++'(I), 
	format("  ~d\tCall: ", [I]),
	write_term(G, [max_depth(10)]), 
	nl,
	call(MG), 
	format("  ~d\tExit: ", [I]),
	write_term(G, [max_depth(10)]), 
	nl.






:- module(colp_tr, [sentence/3], [assertions]).

:- use_module(library(colp(west_colp_tr)), [west_trace/1, west_sentence/3]).
:- use_module(library(colp(east_colp_tr)), [east_sentence/3]).
:- use_module(library(lists), [append/3]).

:- data implementation/1.
:- data saved_clause/2.
:- data trace/0.

:- pred sentence(Input, Outputs, Module) :
	ground * var * ground => ground * list * ground.

sentence(0, [], M):- 
	retractall_fact(implementation(_)),
	retractall_fact(saved_clause(M, _)),
	assertz_fact(implementation(west)),
	west_trace(off).


sentence((:- colp(O)), [], _M):-
	(
	    O = impl(I) ->
	    retractall_fact(implementation(_)),
	    assertz_fact(implementation(I))
	;
	    O = trace ->
	    west_trace(on)
	).

sentence(end_of_file, L, M):- 
	sentence2(0, L1, M),
	pass2(L2, M),  
	append(L1, L2, L).

sentence(A, [], M):-
	assertz_fact(saved_clause(M, A)).

pass2(L, M):-
	(
	    retract_fact(saved_clause(M, A)) ->
	    sentence2(A, L1, M),
	    append(L1, L2, L),
	    pass2(L2, M)
	; 
	    sentence2(end_of_file, L, M)
	).
	    
sentence2(A, L, M) :-
	(
	    implementation(west) ->
	    west_sentence(A, T, M)
	;
	    implementation(east) ->
	    east_sentence(A, T, M)
	;
	    implementation(I) ->
	    throw(error('not implementated'(I)))
	), 
	(
	    T = [] ->
	    L= []
	;
	    T = [_|_] ->
	    L = T
	;
	    L=[T]
	).

	    
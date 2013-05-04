:- module(corec_rt, ['$get_stack'/1, 
	             '$push_stack'/1,
		     '$pop_stack'/0,  
	             '==='/2
		    ], 
		    [foreign_interface]).


:- use_module(engine(internals), ['$global_vars_get'/2]).
:- use_module(library(odd)).

'$get_stack'(X):-
	'$global_vars_get'(12, stack('$'(L))), 
	member_(L, X).

'$push_stack'(C):-
	'$global_vars_get'(12, S),
	S = stack('$'(T)), !,
	setarg(1, S, '$'([C|T])).

'$pop_stack':-
	'$global_vars_get'(12, S),
	S = stack('$'([_|T])), !,
	setarg(1, S, '$'(T)).

member_([], _):-!, fail.
member_([X|_], X).
member_([_|T], X):- member_(T, X).


:- true pred '==='(term, term) + foreign_low(same_object_2).

:- use_foreign_source(same_object).
:- module(disequality_rt,
        [
	    '.<>.'/2,
	    disequality_backtracking/1
	], 
	[]).

:- include(library(disequality(disequality_syntax))).

:- use_module(library(lists)).
:- use_module(library(odd)).
:- use_module(library(terms_vars)).
:- use_module(engine(attributes)).

% For meta-calls
A .<>. B :- '$parseDISEQUALITY'(A,B).

'$parseDISEQUALITY'(A,B) :-
	ground(A), ground(B),
	(
	    A = B ->
	    fail
	;
	    true
	).

'$parseDISEQUALITY'(A,B) :-
	var(A), 
	ground(B),
	modify_attribute(A,B).

'$parseDISEQUALITY'(A,B) :-
	ground(A),
	var(B), 
	modify_attribute(A,B).

'$parseDISEQUALITY'(A,B) :-
	var(A), 
	var(B), 
	modify_attribute(A,B), 
	modify_attribute(B,A). %repeated call to member!

modify_attribute(Var,X) :-
	get_attribute_or_empty(Var,LVar),
	(
	    member_var(LVar,X) ->
	    true
	;
	    diseq_update_attribute(LVar,Var,X),
	    undo(disequality_backtracking(Var))
	).
	
get_attribute_or_empty(Var,L) :-
	get_attribute(Var,Attr), !,
	Attr = '$disequality'(L,Var).
get_attribute_or_empty(_,[]).

diseq_update_attribute([],V,X) :- !,
	attach_attribute(V,'$disequality'([X],V)).
diseq_update_attribute(L,V,X) :- !,
	update_attribute(V,'$disequality'([X|L],V)).

disequality_backtracking(Var) :-
	get_attribute(Var,'$disequality'([_|R],Var)),
	(
	    R = [] ->
	    detach_attribute(Var)
	;
	    update_attribute(Var,'$disequality'(R,Var))
	).
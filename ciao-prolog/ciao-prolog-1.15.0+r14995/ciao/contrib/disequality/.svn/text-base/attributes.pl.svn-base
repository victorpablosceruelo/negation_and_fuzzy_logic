:- use_module(library(write),[write/1]).
:- use_module(library(odd)).
:- use_module(library(lists)).
:- use_module(engine(attributes)).

:- multifile verify_attribute/2.
:- multifile combine_attributes/2.
:- multifile portray_attribute/2.

:- use_module(library(terms_vars)).

verify_attribute('$disequality'(L,Var), X) :-
	ground(X),
	(
	    member_var(L,X) ->
	    fail
	;
	    undo(update_attribute(Var,'$disequality'(L,Var))),
	    detach_attribute(Var),
	    Var = X
	).

combine_attributes('$disequality'(L1,V1), '$disequality'(L2,V2)) :-
	(
	    member_var(L1,V2) ->
	    fail
	;
	    undo(update_attribute(V2,'$disequality'(L2,V2))),
	    detach_attribute(V2),
	    V1 = V2,
	    insert_constraints(L2,V1,L1)
	).

insert_constraints([],_,_) :- !.
insert_constraints([H|R],V,L) :- 
	member_var(H,L), !,
	insert_constraints(R,V,L).
insert_constraints([H|R],V,L) :- 
	update_attribute(V,'$disequality'([H|L],V)),
	undo(disequality_backtracking(V)),
	insert_constraints(R,V,L).

portray_attribute('$disequality'(L,_),Var) :-
	write(Var), display(' different than '),
	display(L).


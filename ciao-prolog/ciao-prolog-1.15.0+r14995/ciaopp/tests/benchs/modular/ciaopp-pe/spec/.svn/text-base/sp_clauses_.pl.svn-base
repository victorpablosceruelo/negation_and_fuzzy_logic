:- module(sp_clauses_,
	[
	    init_unfold/0,
	    sp_clause/2,
	    add_all_clauses/4,
	    orig_clause/2,
	    orig_predicate_names/1,
	    collect_orig_clauses/2,
	    collect_one_orig_clause/2,
	    literal_for_orig_pred/1
	],
	[]).

:- use_package(assertions).

:- doc(title,"Module which handles specialized clauses").

:- doc(author, "Germ@'{a}n Puebla").

:- doc(module," This module contains the operations for handling
    clauses during partial evaluation.").

:- use_module('..'(p_unit(p_unit_)), 
 	[ program/2]).
:- use_module('..'(p_unit(clidlist_)), 
 	[inverse_rewrite_source_program/2, 
 	 rewrite_source_all_clauses/2]).
:- use_module(unfold_operations_, 
	[body2list/2,
	 list2body/2]).
:- use_module(library(terms), [copy_args/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(sort), [sort/2]).


:- data sp_clause/2.
:- data orig_clause/2.

init_unfold:-
	retractall_fact(sp_clause(_,_)),
	retractall_fact(orig_clause(_,_)),
	program(Cls,_Ds),
	inverse_rewrite_source_program(Cls,Cls1),
	assert_all_clauses(Cls1).


assert_all_clauses([]).
assert_all_clauses([clause(Head,Body)|Cls]):-
	body2list(Body,LBody),
	assertz_fact(orig_clause(Head,LBody)),
	assert_all_clauses(Cls).
	
:- doc(add_all_clauses(Clauses,NF), "@var{Clauses} are the
     new definition of the specialized version of new predicate
     @var{NF} after unfolding.").

add_all_clauses([],NF,A,[]):-!,
	functor(NHead,NF,A),
	Body = 'basiccontrol:fail',
	assertz_fact(sp_clause(NHead,Body)).
add_all_clauses(Clauses,NF,A,Newclauses):-
	add_all_clauses_(Clauses,NF,A,Newclauses).
add_all_clauses_([],_NF,_A,[]).
add_all_clauses_([clause(Head,LBody)|Clauses],NF,A,[Cl|NewClauses]):-
 	functor(NHead,NF,A),
 	copy_args(A,Head,NHead),
	list2body(LBody,Body),
	assertz_fact(sp_clause(NHead,Body)),
	Cl = clause(NHead,Body),
	add_all_clauses_(Clauses,NF,A,NewClauses).


:- doc(orig_predicate_names(Names), "@var{Names} is the list of
     predicates in the original program.").

orig_predicate_names(Names):-
	findall(Name/Arity,
	   (orig_clause(Head,_), functor(Head,Name,Arity)),
	    Names0),
	sort(Names0,Names).

collect_orig_clauses(L,UnfClauses):-
	findall(clause(L,Body), orig_clause(L, Body), UnfClauses).

collect_one_orig_clause(L,clause(L,Body)):-
	orig_clause(L,Body).

literal_for_orig_pred(L):-
	functor(L,F,A),
	functor(NL,F,A),
	current_fact(orig_clause(NL, _Body)).

:- doc(version_maintenance,dir('../version')).

:- doc(version(1*0+709,2004/10/08,17:13*50+'CEST'), "Added new
   (exported) predicate @pred{collect_orig_clauses}, necessary for
   implementing depth-first local unfolding.  (Elvira Albert)").

:- doc(version(1*0+608,2004/08/30,15:53*02+'CEST'), "Now code for
   handling clauses during partial evaluation lives here.  (German
   Puebla)").

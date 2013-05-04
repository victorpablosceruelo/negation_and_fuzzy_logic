:- module(relation_res,
	    [
		fail_body/1,
		fail_clause/1,
		recursive_clause/2,
		recursive_predicate/3,
		relation_analysis/10
	    ], [assertions, resources(inferres_decl)]).

%
%  relation.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for performing the relation size
%  analysis for the predicates in the program in topologically sorted order.
%

:- use_module(resources(algebraic_res(simpl_form_res)), [simplification/2]).
:- use_module(resources(init_res(symtable_res)),
	    [
		insert_symbol_field/4,
		find_symbol_field/4
	    ]).
:- use_module(resources(csp_res(csp_unfold_res)),    [unfoldable/5]).
:- use_module(resources(color_res(disequality_res)), [binary_disequality/4]).
:- use_module(resources(color_res(gcp_res)),         [gcp/4]).
:- use_module(resources(csp_res(constraint_res)),
	    [linear_arithmetic_constraints/3]).
:- use_module(resources(csp_res(csp__res)),              [csp/4]).
:- use_module(resources(init_res(initsystem_basic_res)), [clause_type/2]).
:- use_module(resources(top_res(utility_res)),
	    [addition/3, member/2]).
:- use_module(resources(solution_res(binding_res)),
	    [
		relation_head_input/10,
		body_binding/16,
		relation_head/6
	    ]).
:- use_module(resources(solution_res(solution__res)),
	    [
		no_of_cuts/2,
		collect_test_literals/8
	    ]).
:- use_module(resources(resources_basic)).
:- push_prolog_flag(unused_pred_warnings, no).
:- use_module(program(clidtypes), [clausebody/1]).
:- pop_prolog_flag(unused_pred_warnings).

%
%  Perform the relation size analysis for a strongly connected component.
%
relation_analysis([], _, _, _, _, _, _, _, _, []).
relation_analysis([Pred|CompList], Approx, BT, ST, Comp, [Size|SList],
	    [Adg|AList], [Gvars|GList], [Ldg|LList], [[Sol]|OList]) :-
	relation_predicate(Pred, Approx, BT, ST, Comp, Size, Adg, Gvars, Ldg,
	    Sol1),
	simplification(Sol1, Sol),
	insert_symbol_field(ST, Pred, relation, Sol),
	relation_analysis(CompList, Approx, BT, ST, Comp, SList, AList, GList,
	    LList, OList).

%
%  Perform the relation size analysis for a predicate.
%
relation_predicate(Pred, Approx, BT, ST, Comp, Size, Adg, Gvars, Ldg, Sol) :-
	find_symbol_field(ST, Pred, relation, Sol1),
	(
	    var(Sol1) ->
	    (
		recursive_predicate(Pred, ST, Comp) ->
		Sol = inf
	    ;
		relation_nonrec_pred(Pred, Approx, BT, ST, Comp, Size, Adg,
		    Gvars, Ldg, Sol)
	    )
	;
	    Sol = Sol1
	).

%
%  Perform the relation size analysis for a predicate.
%
relation_nonrec_pred(Pred, Approx, BT, ST, Comp, Size, Adg, Gvars, Ldg, Sol) :-
	find_symbol_field(ST, Pred, clause, ClauseKeys),
	find_symbol_field(ST, Pred, domain, Domain),
	(
	    (nonvar(Domain), unfoldable(Pred, BT, ST, Vars, UClauses)) ->
	    (
		binary_disequality(Pred, ST, UClauses, DomainSize) ->
		gcp(Vars, DomainSize, UClauses, Sol)
	    ;
		(
		    linear_arithmetic_constraints(Pred, ST, UClauses) ->
		    csp(Vars, Domain, UClauses, Sol)
		;
		    relation_clauses(ClauseKeys, Approx, BT, ST, Comp, Size,
			Adg, Gvars, Ldg, Sol)
		)
	    )
	;
	    relation_clauses(ClauseKeys, Approx, BT, ST, Comp, Size, Adg,
		Gvars, Ldg, Sol)
	).

:- pred relation_clauses/10 :: list(clause_key_t) * approx *
	list(bottom_entry) * list(symbol_entry) * term *
	list * list * list * list * term + (not_fails, is_det) #
"Perform the relation size analysis for the set of
clauses in a predicate.".
relation_clauses(ClauseKeys, _, _, _, _, _, _, _, _, 0) :-
	var(ClauseKeys),
	!.
relation_clauses([ClauseKey|CList], Approx, BT, ST, Comp, [Size|SList],
	    [Adg|AList], [Gvars|GList], [Ldg|LList], Sol) :-
	clause_key(ClauseKey, ClausePPKey, Key),
	relation_clause(ClausePPKey, Key, Approx, BT, ST, Comp, Size, Adg,
	    Gvars, Ldg, Sol1),
	relation_clauses(CList, Approx, BT, ST, Comp, SList, AList, GList,
	    LList, Sol2),
	addition(Sol1, Sol2, Sol).

:- pred relation_clause/11 :: clause_ppkey_t * atm * approx * list(bottom_entry
	)
	* list(symbol_entry) * term * term * term * term * term * term
	+ (not_fails, is_det) #
	"Perform the relation size analysis for a clause.".
relation_clause(Clause, Key, Approx, BT, ST, Comp, Size, Adg, Gvars, Ldg,
	    Sol) :-
	clause_type(Clause, Type),
	relation_clause_(Type, Clause, Key, Approx, BT, ST, Comp, Size, Adg,
	    Gvars, Ldg, Sol).
relation_clause_(rule, ClausePPKey, Key, Approx, BT, ST, Comp, Size, Adg,
	    Gvars, Ldg, Sol) :-
	clause_head_body(ClausePPKey, Head, Body),
	(
	    fail_body(Body) ->
	    Sol = 0
	;
	    relation_head_input(Head, Approx, BT, ST, ClausePPKey, Key, Adg,
		Gvars, Ldg, ISol),
	    (
		ISol == inf ->
		Sol = inf
	    ;
		no_of_cuts(Body, Cuts),
		collect_test_literals(Body, 1, ClausePPKey, Key, Approx, BT,
		    ST, Lits),
		(
		    body_binding(Body, 1, Approx, BT, ST, Comp, ClausePPKey,
			Key, Size, Adg, Gvars, Ldg, relation, Cuts, Lits, _),
		    relation_head(Head, ClausePPKey, Gvars, Ldg, Lits, Sol) ->
		    true ;
		    Sol = inf % Kludge to avoid fail - EMM
		)
	    )
	).
relation_clause_(fact, ClausePPKey, Key, Approx, BT, ST, _, _, Adg, Gvars,
	    Ldg, Sol) :-
	clause_head(ClausePPKey, Fact),
	relation_head_input(Fact, Approx, BT, ST, ClausePPKey, Key, Adg, Gvars,
	    Ldg, ISol),
	(
	    ISol == inf ->
	    Sol = inf
	;
	    relation_head(Fact, ClausePPKey, Gvars, Ldg, [], Sol)
	).

:- pred recursive_predicate/3 :: predname * list(symbol_entry) * list(predname)
	+ is_det # "Test if a predicate is recursive.".
recursive_predicate(Pred, ST, Component) :-
	find_symbol_field(ST, Pred, clause, ClauseKeys),
	recursive_clauses(ClauseKeys, Component).

recursive_clauses(ClauseKeys, _) :-
	var(ClauseKeys),
	!,
	fail.
recursive_clauses([ClauseKey|CList], Component) :-
	clause_key(ClauseKey, ClausePPKey, _Key),
	( recursive_clause(ClausePPKey, Component) ->
	    true ;
	    recursive_clauses(CList, Component) ).

:- pred recursive_clause/2 :: clause_ppkey_t * list(predname).
recursive_clause(ClausePPKey, Component) :-
	clause_type(ClausePPKey, Type),
	recursive_clause_(Type, ClausePPKey, Component).

recursive_clause_(rule, ClausePPKey, Component) :-
	clause_body(ClausePPKey, Body),
	recursive_body(Body, Component).

recursive_body((LitPPKey, Body), Component) :-
	!,
	lit_ppkey(LitPPKey, Lit, _PPKey),
	functor(Lit, F, A),
	( utility_res:member(Component, F/A) ->
	    true ;
	    recursive_body(Body, Component) ).
recursive_body(LitPPKey, Component) :-
	lit_ppkey(LitPPKey, Lit, _PPKey),
	functor(Lit, F, A),
	utility_res:member(Component, F/A).

fail_clause(Clause) :-
	clause_type(Clause, rule),
	clause_body(Clause, Body),
	fail_body(Body).

:- pred fail_body/1 : clausebody + is_det.

fail_body((LitPPKey, Body)) :-
	lit_ppkey(LitPPKey, Lit, _),
	!,
	( Lit == fail ->
	    true ;
	    fail_body(Body) ).
fail_body(LitPPKey) :-
	lit_ppkey(LitPPKey, Lit, _),
	Lit == fail.

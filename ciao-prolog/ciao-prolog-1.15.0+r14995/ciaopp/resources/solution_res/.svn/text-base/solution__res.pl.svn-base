:- module(solution__res,
	    [
		no_of_cuts/2,
		solution_analysis/11,
		collect_test_literals/8
	    ], [assertions, resources(inferres_decl)]).

%
%  solution.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for performing the solution size
%  analysis for the predicates in the program in topologically sorted order.
%

:- push_prolog_flag(unused_pred_warnings, no).
:- use_module(program(clidtypes), [clausebody/1]).
:- pop_prolog_flag(unused_pred_warnings).

:- use_module(resources(init_res(initsystem_basic_res)), [clause_type/2]).
:- use_module(resources(solution_res(relation_res)),     [fail_body/1]).
:- use_module(resources(csp_res(csp_unfold_res)),        [test_predicate/1]).
:- use_module(resources(init_res(symtable_res)),
	    [
		find_symbol_field/4,
		insert_symbol_field/4,
		literal_property/10
	    ]).
:- use_module(resources(solution_res(comp_diff_equ_res)),
	    [solve_complexity_equ/7]).
:- use_module(resources(size_res(size__res)), [remove_recursive_comps/4]).
:- use_module(resources(solution_res(binding_res)),
	    [
		solution_head_input/4,
		solution_head_input/9,
		body_binding_/17,
		set_vars_binding/3,
		solution_head_output/7
	    ]).
:- use_module(resources(resources_basic)).

:- pred solution_analysis/11 :: list(predname) * approx * bound
	* list(bottom_entry) * list(symbol_entry) * list(predname)
	* list * list * list * list * list + (not_fails, is_det) #
	"Perform the solution size analysis for a strongly connected component.".
solution_analysis(Comp, Approx, Bound, BT, ST, Comp, Size, Adg, Gvars, Ldg,
	    Sol) :-
	solution_analysis_(Comp, Approx, Bound, BT, ST, Comp, Size, Adg, Gvars,
	    Ldg, [], Sol).

:- pred solution_analysis_/12 :: list(predname) * approx * bound
	* list(bottom_entry) * list(symbol_entry) * list(predname)
	* list * list * list * list * list(comp_t) * list
	+ (not_fails, is_det).
solution_analysis_([], _, _, _, _, _, _, _, _, _, _, []).
solution_analysis_([Pred|CompList], Approx, Bound, BT, ST, Comp, [Size|SList],
	    [Adg|AList], [Gvars|GList], [Ldg|LList], RSol, [Sol|OList]) :-
	find_symbol_field(ST, Pred, det, Sol1),
	( var(Sol1) ->
	    ( find_symbol_field(ST, Pred, clause, ClauseKeys),
		solution_clauses(ClauseKeys, Approx, BT, ST, Comp, Size,
		    Adg, Gvars, Ldg, RSol, Sol2),
%write(Sol2),nl,
		solve_complexity_equ(Pred, Bound, ST, Comp, Sol2, Size, TSol),
%write(TSol),nl,
		Sol3 = [TSol] ) ;
	    Sol3 = Sol1 ),
	solution_analysis_(CompList, Approx, Bound, BT, ST, Comp, SList, AList,
	    GList, LList, [comp(Pred, Sol3)|RSol], OList),
	remove_recursive_comps(Sol3, ST, det, Sol),
	insert_symbol_field(ST, Pred, det, Sol).

:- pred solution_clauses/11 :: list(clause_key_t) * approx * list(bottom_entry)
	* list(symbol_entry) * list(predname) * list * list * list * list *
	list(comp_t) * list + (not_fails, is_det) #
"Perform the solution size analysis for the set of clauses in a predicate.".
solution_clauses(ClauseKeys, _, _, _, _, _, _, _, _, _, []) :-
	var(ClauseKeys),
	!.
solution_clauses([ClauseKey|CList], Approx, BT, ST, Comp, [Size|SList],
	    [Adg|AList], [Gvars|GList], [Ldg|LList], RSol, [Sol|OList]) :-
	clause_key(ClauseKey, ClausePPKey, Key),
	solution_clause(ClausePPKey, Key, Approx, BT, ST, Comp, Size, Adg,
	    Gvars, Ldg, RSol, Sol),
	solution_clauses(CList, Approx, BT, ST, Comp, SList, AList, GList,
	    LList, RSol, OList).

:- pred solution_clause/12 :: clause_ppkey_t * atm * approx *
	list(bottom_entry) * list(symbol_entry) * list(predname) * list *
	list * list * list * list(comp_t) * term + (not_fails, is_det) #
	"Perform the solution size analysis for a clause.".
solution_clause(Clause, Key, Approx, BT, ST, Comp, Size, Adg, Gvars, Ldg, RSol,
	    Sol) :-
	clause_type(Clause, Type),
	solution_clause_(Type, Clause, Key, Approx, BT, ST, Comp, Size, Adg,
	    Gvars, Ldg, RSol, Sol).
solution_clause_(rule, ClausePPKey, Key, Approx, BT, ST, Comp, Size, Adg,
	    Gvars, Ldg, RSol, Sol) :-
	clause_head_body(ClausePPKey, Head, Body),
	( fail_body(Body) ->
	    Sol = 0 ;
	    ( solution_head_input(Head, Adg, Ldg, HVars),
		no_of_cuts(Body, Cuts),
		collect_test_literals(Body, 1, ClausePPKey, Key, Approx, BT,
		    ST, Lits),
		set_vars_binding(HVars, 1, VarsBindings0),
		body_binding_(Body, 1, Approx, BT, ST, Comp, ClausePPKey, Key,
		    Size, Adg, Gvars, Ldg, det, Cuts, Lits, RSol,
		    VarsBindings0),
		solution_head_output(Head, ClausePPKey, Adg, Gvars, Ldg, Lits,
		    Sol)
	    ) ).
solution_clause_(fact, ClausePPKey, Key, Approx, BT, ST, _, _, Adg, Gvars, Ldg,
	    _, Sol) :-
	clause_head(ClausePPKey, Head),
	solution_head_input(Head, Approx, BT, ST, ClausePPKey, Key, Adg, Gvars,
	    Ldg),
	solution_head_output(Head, ClausePPKey, Adg, Gvars, Ldg, [], Sol).

%
:- pred no_of_cuts/2 :: clausebody * nnegint.
no_of_cuts((LitPPKey, Body), No) :-
	!,
	lit_ppkey(LitPPKey, Lit, _PPKey),
	no_of_cuts(Body, Nos),
	(Lit == (!) -> No is Nos + 1 ; No = Nos).
no_of_cuts(LitPPKey, No) :-
	lit_ppkey(LitPPKey, Lit, _PPKey),
	(Lit == (!) -> No = 1 ; No = 0).

%
collect_test_literals((LitPPKey, Body), LitNum, ClausePPKey, Key, Approx, BT,
	    ST, Lits) :-
	!,
	collect_test_literal(LitPPKey, LitNum, ClausePPKey, Key, Approx, BT,
	    ST, Lits, RestLits),
	LitNum1 is LitNum + 1,
	collect_test_literals(Body, LitNum1, ClausePPKey, Key, Approx, BT, ST,
	    RestLits).
collect_test_literals(LitPPKey, LitNum, ClausePPKey, Key, Approx, BT, ST,
	    Lits) :-
	collect_test_literal(LitPPKey, LitNum, ClausePPKey, Key, Approx, BT,
	    ST, Lits, []).

collect_test_literal(LitPPKey, LitNum, ClausePPKey, Key, Approx, BT, ST, Lits,
	    RestLits) :-
	lit_ppkey(LitPPKey, Lit, PPKey),
	literal_property(BT, ST, Lit, ClausePPKey, Key, PPKey, LitNum, mode,
	    Approx, Mode),
	( test_predicate(Mode) ->
	    ( literal_property(BT, ST, Lit, ClausePPKey, Key, PPKey, LitNum,
		    relation, Approx, Rel),
		( Rel \== inf ->
		    Lits = [test(Lit, Rel)|RestLits] ;
		    Lits = RestLits ) ) ;
	    Lits = RestLits ).

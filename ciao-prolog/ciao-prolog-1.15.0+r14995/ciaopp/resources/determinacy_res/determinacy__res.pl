:- module(determinacy__res, [determinacy_analysis/5],
	    [assertions, resources(inferres_decl)]).

%
%  determinacy.pl		Nai-Wei Lin			May 1992
%
%  This file contains the procedures for performing the determinacy analysis
%  for the predicates in the program in topologically sorted order.
%

:- use_module(resources(resources_basic)).
:- use_module(resources(init_res(symtable_res)),
	    [
		find_symbol_field/4,
		literal_property/10,
		insert_symbol_field/4
	    ]).
:- use_module(resources(init_res(initsystem_basic_res)), [clause_type/2]).
:- use_module(resources(top_res(utility_res)),
	    [
		opened_set_equivalent/2,
		member/2
	    ]).
:- use_module(resources(solution_res(solution__res)), [no_of_cuts/2]).
:- use_module(resources(init_res(builtin_res)),
	    [
		second_order_predicate/1,
		second_order_predicate_pred_arg/2,
		second_order_predicate_pred_num/3
	    ]).
:- use_module(resources(dependency_res(position_res)),
	    [gen_literal_iopos/5]).
:- use_module(resources(solution_res(binding_res)), [pos_var/3, term_var/2]).
:- use_module(resources(size_res(normalize__res)),  [find_recursive_comp/3]).
:- use_module(ciaopp(preprocess_flags),             [current_pp_flag/2]).

:- pred determinacy_analysis/5 :: list(predname) * approx * list(bottom_entry)
	* list(symbol_entry) * list + (not_fails, is_det) #
	"Perform the determinacy analysis for a strongly connected component.".
determinacy_analysis(Comp, Approx, BT, ST, Adg) :-
	initial_determinacy(Comp, Det),
	determinacy_analysis1(Comp, Approx, BT, ST, Adg, Det).

:- pred determinacy_analysis1/6 :: list(predname) * approx * list(bottom_entry)
	* list(symbol_entry) * list * list + (not_fails, is_det).
determinacy_analysis1(Comp, Approx, BT, ST, Adg, Det) :-
	determinacy_analysis2(Comp, Approx, BT, ST, Comp, Det, Adg, NDet),
	( Det == NDet ->
	    insert_determinacy(Det, ST) ;
	    determinacy_analysis1(Comp, Approx, BT, ST, Adg, NDet) ).

:- pred determinacy_analysis2/8 :: list(predname) * approx * list(bottom_entry)
	* list(symbol_entry) * list(predname) * list * list * list(comp_t)
	+ (not_fails, is_det).
determinacy_analysis2([], _, _, _, _, _, _, []).
determinacy_analysis2([Pred|Comps], Approx, BT, ST, Comp, CompDet, [Adg|AList],
	    [comp(Pred, Det)|Dets]) :-
	determinacy_predicate(Pred, Approx, BT, ST, Comp, CompDet, Adg, Det),
	determinacy_analysis2(Comps, Approx, BT, ST, Comp, CompDet, AList,
	    Dets).

:- pred determinacy_predicate/8 :: predname * approx * list(bottom_entry)
	* list(symbol_entry) * list(predname) * list * list * nnegint
	+ (not_fails, is_det) #
	"Perform the determinacy analysis for a predicate.".
determinacy_predicate(_Pred, _Approx, _BT, _ST, _Comp, _CompDet, _Adg, 1) :-
	current_pp_flag(prog_lang, java), !.
determinacy_predicate(Pred, Approx, BT, ST, Comp, CompDet, Adg, Det) :-
	find_symbol_field(ST, Pred, mutex, Mutex),
	( pairwise_mutual_exclusion(Mutex) ->
	    ( find_symbol_field(ST, Pred, clause, ClauseKeys),
		determinacy_clauses(ClauseKeys, Approx, BT, ST, Comp, CompDet,
		    Adg, Det)
	    )
	;
	    Det = 0 ).

:- pred determinacy_clauses/8 :: list(clause_key_t) * approx
	* list(bottom_entry) * list(symbol_entry) * list(predname) * list *
	list * nnegint + (not_fails, is_det) #
"Perform the determinacy analysis for the set of clauses in a predicate.".
determinacy_clauses(ClauseKeys, _, _, _, _, _, _, 1) :-
	var(ClauseKeys),
	!.
determinacy_clauses([ClauseKey|CList], Approx, BT, ST, Comp, CompDet,
	    [Adg|AList], Det) :-
	clause_key(ClauseKey, ClausePPKey, Key),
	determinacy_clause(ClausePPKey, Key, Approx, BT, ST, Comp, CompDet,
	    Adg, Det1),
	( Det1 == 1 ->
	    determinacy_clauses(CList, Approx, BT, ST, Comp, CompDet, AList,
		Det)
	; Det = 0 ).

%
%  Perform the determinacy analysis for a clause.
%
determinacy_clause(ClausePPKey, Key, Approx, BT, ST, Comp, CompDet, Adg,
	    Det) :-
	clause_type(ClausePPKey, Type),
	determinacy_clause_(Type, ClausePPKey, Key, Approx, BT, ST, Comp,
	    CompDet, Adg, Det).

determinacy_clause_(rule, ClausePPKey, Key, Approx, BT, ST, Comp, CompDet, Adg,
	    Det) :-
	clause_body(ClausePPKey, Body),
	no_of_cuts(Body, Cuts),
	determinacy_body(Body, 1, Approx, BT, ST, Comp, Cuts, CompDet, Adg,
	    ClausePPKey, Key, Det).
determinacy_clause_(fact, _, _, _, _, _, _, _, _, 1).

%
%  Perform the determinacy analysis for the body of a clause.
%
determinacy_body((LitPPKey, Body), Num, Approx, BT, ST, Comp, Cuts, CompDet,
	    Adg, ClausePPKey, Key, Det) :-
	!,
	lit_ppkey(LitPPKey, Lit, PPKey),
	Num1 is Num+1,
	( Lit == (!) ->
	    ( Cuts1 is Cuts -1,
		determinacy_body(Body, Num1, Approx, BT, ST, Comp, Cuts1,
		    CompDet, Adg, ClausePPKey, Key, Det) ) ;
	    ( Cuts > 0 ->
		determinacy_body(Body, Num1, Approx, BT, ST, Comp, Cuts,
		    CompDet, Adg, ClausePPKey, Key, Det) ;
		( determinacy_literal(Lit, PPKey, Num, Approx, BT, ST, Comp,
			CompDet, Adg, ClausePPKey, Key, Det1),
		    ( Det1 == 1 ->
			determinacy_body(Body, Num1, Approx, BT, ST, Comp,
			    Cuts, CompDet, Adg, ClausePPKey, Key, Det) ;
			Det = 0 ) ) ) ).
determinacy_body(LitPPKey, Num, Approx, BT, ST, Comp, _, CompDet, Adg,
	    ClausePPKey, Key, Det) :-
	lit_ppkey(LitPPKey, Lit, PPKey),
	( Lit == (!) ->
	    Det = 1 ;
	    determinacy_literal(Lit, PPKey, Num, Approx, BT, ST, Comp, CompDet,
		Adg, ClausePPKey, Key, Det) ).

%
%  Perform the determinacy analysis for a literal.
%
determinacy_literal(Lit, PPKey, LitNum, Approx, BT, ST, Comp, CompDet, Adg,
	    ClausePPKey, Key, Det) :-
	functor(Lit, F, A),
	( second_order_predicate(F/A) ->
%% handle setof predicate
	    ( second_order_predicate_pred_arg(Lit, NLit),
		functor(NLit, NF, NA),
		clause_body(ClausePPKey, Body),
		second_order_predicate_pred_num(Body, LitNum, Num),
		gen_literal_iopos(Adg, (NF/NA), Num, (-), Pos),
		pos_var(Pos, NLit, Vars),
		arg(1, Lit, Arg1),
		term_var(Arg1, Var1),
		( opened_set_equivalent(Var1, Vars) ->
		    Det = 1 ;
		    Det = 0 ) ) ;
%% handle other predicates
	    ( utility_res:member(Comp, (F/A)) ->
		find_recursive_comp(CompDet, (F/A), Det) ;
		( literal_property(BT, ST, Lit, ClausePPKey, Key, PPKey, LitNum,
			det, Approx, [Det1]),
		    ( (Det1 == 0 ; Det1 == 1) ->
			Det = 1 ;
			Det = 0 ) ) ) ).

%
%  Initialize the determinacy to `true' for the predicates in the component.
%

:- pred initial_determinacy/2 :: list(predname) * list(comp_t) + ( not_fails,
	    is_det ).
% rtcheck -- EMM
initial_determinacy([],          []).
initial_determinacy([Pred|Comp], [comp(Pred, 1)|Det]) :-
	initial_determinacy(Comp, Det).

%
%  Define the det field for the determinate predicates in the component.
%
insert_determinacy([],                     _).
insert_determinacy([comp(Pred, Det)|Dets], ST) :-
	( Det == 1 ->
	    ( find_symbol_field(ST, Pred, relation, Rel),
		( Rel == 0 ->
		    insert_symbol_field(ST, Pred, det, [0]) ;
		    insert_symbol_field(ST, Pred, det, [1]) ) ) ;
	    true ),
	insert_determinacy(Dets, ST).

%
%  Test if the clauses of a predicate is pairwise mutually exclusive.
%
pairwise_mutual_exclusion([]).
pairwise_mutual_exclusion([[_]|Mutex]) :-
	pairwise_mutual_exclusion(Mutex).

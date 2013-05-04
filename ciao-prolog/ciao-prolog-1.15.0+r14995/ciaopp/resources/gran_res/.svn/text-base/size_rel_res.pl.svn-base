:- module(size_rel_res, [input_arg_size_relation/7],
	    [assertions, resources(inferres_decl)]).

%
%  size_relations.pl			Pedro Lopez-Garcia		October, 1992
%  

%
%  This file contains the procedures to obtain the expresions of the sizes of 
%  the input positions in the body of a clause as a function of the input 
%  positions in the head.
%
%  A list of "iasize(Term/ArgNum ,Size)" is built.
%

:- use_module(resources(resources_basic)).
:- use_module(resources(algebraic_res(simpl_form_res)), [simplification/2]).
:- use_module(resources(dependency_res(adg_res)),       [find_adg_field/4]).
:- use_module(resources(dependency_res(position_res)),
	    [
		gen_clause_pos/2,
		pos_argnum/2,
		pos_litnum/2,
		pos_t/1
	    ]).
:- use_module(resources(gran_res(size_rel_basic_res)),
	    [generate_size_key/4]).
:- use_module(resources(gran_res(gran_table_res)),
	    [insert_gran_field/4]).
:- use_module(resources(init_res(symtable_res)),
	    [
		find_symbol_field/4,
		literal_property/10
	    ]).
:- use_module(resources(init_res(builtin_res)),
	    [second_order_predicate_pred_arg/2]).
:- use_module(resources(size_res(size__res)),
	    [
		explicit_output_size/10,
		implicit_output_size/10
	    ]).
:- use_module(resources(size_res(normalize__res)),
	    [
		normalize/15,
		init_normalize_queue/3
	    ]).
:- use_module(resources(size_res(clause_res)),
	    [
		ith_clause_literal/3,
		number_of_literals/3
	    ]).
:- use_module(resources(top_res(utility_res)), [ith_list_element/3]).


%
%  Perform the argument size analysis for a strongly connected component.
%

input_arg_size_relation([], _, _, _, _, _, _).
input_arg_size_relation([Pred|CompList], Approx, BT, ST, [Adg|AList],
	    [Gvars|GList], GT) :-
	find_symbol_field(ST, Pred, clause, ClauseKeys),
	input_arg_size_rel_clauses(ClauseKeys, Approx,
	    BT, ST, Adg, Gvars, Size),
	insert_gran_field(GT, Pred, sizes, Size),
	input_arg_size_relation(CompList, Approx, BT, ST, AList, GList, GT).

%
%  Perform the argument size analysis for the set of clauses in a predicate.
%

input_arg_size_rel_clauses(ClauseKeys, _, _, _, _, _, []) :-
	var(ClauseKeys),
	!.
input_arg_size_rel_clauses([ClauseKey|CList], Approx, BT, ST, [Adg|AList], [
		Gvars|GList],
	    [Size|SList]) :-
	clause_key(ClauseKey, ClausePPKey, Key),
	in_arg_size_rel_clause(ClausePPKey, Key, Approx, BT, ST, Adg, Gvars,
	    Size),
	input_arg_size_rel_clauses(CList, Approx, BT, ST, AList, GList,
	    SList).

%
%  Perform the argument size analysis for a clause.
%

in_arg_size_rel_clause(ClausePPKey, Key, Approx, BT, ST, Adg, Gvars, Size) :-
	gen_clause_pos(Adg, InPos),
	input_arg_size_rel_func(InPos, ClausePPKey, Key, Approx, BT, ST, Adg,
	    Gvars, Size).
%
%  Compute the size functions of the input positions in the body of a clause.
%

input_arg_size_rel_func([], _, _, _, _, _, _, _, []).
input_arg_size_rel_func([InPos|IPList], ClausePPKey, Key, Approx, BT, ST, Adg,
	    Gvars, [Entry|SList]) :-
	clause_term_measure_lit(Approx, BT, ST, ClausePPKey, Key, InPos, Term,
	    Measure, Lit),
	in_arg_size_rel_func_pos(InPos, Measure, Term, ClausePPKey, Key,
	    Approx, BT, ST, Adg, Gvars, Size, ExplSize),
	simplification(Size,     SimSize),
	simplification(ExplSize, SimExplSize),
	pos_argnum(InPos, ArgNum),
	pos_litnum(InPos, LitNum),
	generate_size_key(Lit, LitNum, ArgNum, Key),
	Entry = iasize(Key, se(SimSize, SimExplSize)),
	input_arg_size_rel_func(IPList, ClausePPKey, Key, Approx, BT, ST, Adg,
	    Gvars, SList).

:- pred clause_term_measure_lit/9 :: approx * list(bottom_entry)
	* list(symbol_entry) * clause_ppkey_t * atm * pos_t * term * measure_t
	* callable + (is_det, not_fails).
clause_term_measure_lit(Approx, BT, ST, ClausePPKey, Key, Pos, Term, Measure,
	    NewLit) :-
	pos_litnum(Pos, LitNum),
	( LitNum > 0 ->
	    ( clause_body(ClausePPKey, Body),
		number_of_literals(Body, 1, Num),
		( LitNum > Num ->
		    NewNum is LitNum-Num; % assume depth-one single pred
		    NewNum = LitNum ) ) ;
	    NewNum = LitNum ),
	ith_clause_literal(NewNum, ClausePPKey, LitPPKey),
	lit_ppkey(LitPPKey, Lit, PPKey),
	( (LitNum > 0, LitNum > Num) ->
	    second_order_predicate_pred_arg(Lit, NewLit) ;
	    NewLit = Lit ),
	pos_argnum(Pos, ArgNum),
	arg(ArgNum, NewLit, Term),
	literal_property(BT, ST, NewLit, ClausePPKey, Key, PPKey, LitNum,
	    measure, Approx, MeasureList),
	ith_list_element(ArgNum, MeasureList, Measure).

%
%  Compute the size function for an input position in the body of a clause . 
%  
in_arg_size_rel_func_pos(InPos, Measure, Term, ClausePPKey, Key, Approx,
	    BT, ST, Adg, Gvars, Size, ExplSize) :-
	(
	    var(Term) ->
	    implicit_output_size(Measure, Term, Adg, Approx, ClausePPKey, Key,
		BT, ST, Gvars, Size1)
	;
	    Size1 = bot
	),
	(
	    Size1 == bot ->
	    explicit_output_size(Measure, Approx, ClausePPKey, Key, BT, ST,
		Adg, Gvars, Term, ExplSize),
	    sr_normalize_size_function(ExplSize, InPos, Approx, BT, ST,
		ClausePPKey, Key, Adg, Gvars, Size)
	;
	    Size = Size1,
	    ExplSize = Size1
	).

:- pred sr_normalize_size_function/10 :: term * pos_t * approx *
	list(bottom_entry) * list(symbol_entry) * clause_ppkey_t * atm *
	term * term * term + (not_fails, is_det) # "Normalize the size
	function corresponding to an input position in the body of a
	clause.".
sr_normalize_size_function(Size, Pos, Approx, BT, ST, ClausePPKey, Key, Adg,
	    Gvars, NSize) :-
	gen_clause_pos(Adg, PosSet),
	find_adg_field(Adg, Pos, pred, PredPos),
	init_normalize_queue(PredPos, QHead, QTail),
	ith_clause_literal(0, ClausePPKey, LitPPKey),
	lit_ppkey(LitPPKey, Lit, _PPKey),
	functor(Lit, F, N),
	find_symbol_field(ST, F/N, size, ISize),
	normalize(Size, QHead, QTail, Approx, BT, ST, [], ClausePPKey,
	    Key, Adg, Gvars, PosSet, ISize, [], NSize).

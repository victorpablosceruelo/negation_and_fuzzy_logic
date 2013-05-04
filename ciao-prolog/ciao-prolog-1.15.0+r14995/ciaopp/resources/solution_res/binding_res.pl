:- module(binding_res,
	    [
		normalize_solution_function/14,
		pos_var/3,
		term_var/2,
		set_vars_binding/3,
		solution_head_output/7,
		body_binding/16,
		body_binding_/17,
		solution_head_input/4,
		solution_head_input/9,
		relation_head_input/10,
		relation_head/6
	    ], [assertions, resources(inferres_decl)]).

%
%  binding.pl			Nai-Wei Lin			January, 1992
%
%  This file contains the procedures for computing the binding pattern.
%  

:- use_module(library(lists), [length/2]).
:- push_prolog_flag(unused_pred_warnings, no).
:- use_module(program(clidtypes), [clausebody/1, bodykey/1]).
:- pop_prolog_flag(unused_pred_warnings).
:- use_module(resources(resources_basic)).
:- use_module(resources(dependency_res(ldg_res)),
	    [
		new_lit/2,
		insert_ldg_field/4,
		find_ldg_field/4
	    ]).
:- use_module(resources(dependency_res(position_res)),
	    [
		gen_literal_iopos/5,
		gen_literal_pos/3,
		gen_clause_pos/2,
		pos_litnum/2,
		pos_argnum/2
	    ]).
:- use_module(resources(top_res(utility_res)),
	    [
		minimum/3,
		multiply/3,
		opened_set_inclusion/2,
		opened_set_insertion/2,
		compound/1,
		close_list/1,
		opened_set_member/2
	    ]).
:- use_module(resources(init_res(builtin_res)),
	    [
		second_order_predicate/1,
		second_order_predicate_pred_arg/2,
		second_order_predicate_pred_num/3
	    ]).
:- use_module(resources(init_res(symtable_res)),
	    [literal_property/10, find_symbol_field/4]).
:- use_module(resources(init_res(initsystem_basic_res)), [clause_type/2]).
:- use_module(resources(size_res(normalize__res)),
	    [
		literal_output_comp/15,
		init_normalize_queue/3,
		normalize/15
	    ]).
:- use_module(resources(size_res(clause_res)),
	    [ith_clause_literal/3, number_of_literals/3]).
:- use_module(resources(algebraic_res(simpl_form_res)), [simplification/2]).
:- use_module(resources(solution_res(relation_res)),    [recursive_clause/2]).
:- use_module(resources(dependency_res(gvars_res)),
	    [find_gvars_field/4, insert_gvars_field/4]).

:- pred relation_head_input/10 :: callable * approx * list(bottom_entry)
	* list(symbol_entry) * clause_ppkey_t * atm * list * list * list * term
	+ is_det #
	"Compute the number of head input bindings for relation size analysis.".
relation_head_input(Head, Approx, BT, ST, ClausePPKey, Key, Adg, Gvars, Ldg,
	    Sol) :-
	new_lit(+, Lit),
	functor(Head, F, A),
	gen_literal_iopos(Adg, F/A, 0, (+), Pos),
	pos_var(Pos, Head, Vars),
	head_input_tuple_size(Vars, Approx, BT, ST, ClausePPKey, Key, Gvars,
	    Sol),
	insert_ldg_field(Ldg, Lit, relation, Sol),
	insert_vars_binding(Vars, Approx, BT, ST, ClausePPKey, Key, Gvars,
	    relation, Sol).
%insert_ldg_field(Ldg, Lit, redge, Mvars).

:- pred relation_head/6 :: callable * clause_ppkey_t * list * list * list *
	term + is_det #
	"Compute the number of head bindings for relation size analysis.".
relation_head(Head, ClausePPKey, Gvars, Ldg, TestLits, Binding) :-
	functor(Head, F, A),
	gen_literal_pos(F/A, 0, Pos),
	pos_var(Pos, Head, Vars),
	var_def_list(Vars, Gvars, Lvars),
	vars_binding(Lvars, Gvars, Ldg, redge, ClausePPKey, Binding1),
	filter_effect(TestLits, Vars, Binding1, Binding).

%
%  Compute the number of head input bindings for solution size analysis.
%
solution_head_input(Head, Adg, Ldg, Vars) :-
	new_lit(+, Lit),
	insert_ldg_field(Ldg, Lit, det, 1),
	functor(Head, F, A),
	gen_literal_iopos(Adg, F/A, 0, (+), Pos),
	pos_var(Pos, Head, Vars).
%insert_ldg_field(Ldg, Lit, sedge, Mvars).

solution_head_input(Head, Approx, BT, ST, ClausePPKey, Key, Adg, Gvars, Ldg) :-
	solution_head_input(Head, Adg, Ldg, Vars),
	insert_vars_binding(Vars, Approx, BT, ST, ClausePPKey, Key, Gvars, det,
	    1).

%
%  Compute the number of head output bindings for solution size analysis.
%
solution_head_output(Head, ClausePPKey, Adg, Gvars, Ldg, TestLits, Binding) :-
	functor(Head, F, A),
	gen_literal_iopos(Adg, F/A, 0, (-), Pos),
	pos_var(Pos, Head, Vars),
	var_def_list(Vars, Gvars, Lvars),
	vars_binding(Lvars, Gvars, Ldg, sedge, ClausePPKey, Binding1),
	filter_effect(TestLits, Vars, Binding1, Binding).

:- pred body_binding/16 :: clausebody * nnegint * approx * list(bottom_entry)
	* list(symbol_entry) * list(predname) * clause_ppkey_t * atm * list
	* list * list * list * symbol_field * nnegint * list(test_t)
	* list(comp_t) + (not_fails, is_det) #
	"Compute the number of body input and output bindings.".
body_binding(Body, LitNum, Approx, BT, ST, Comp, ClausePPKey, Key, Size, Adg,
	    Gvars, Ldg, Type, Cuts, TestLits, RSol) :-
	body_binding_(Body, LitNum, Approx, BT, ST, Comp, ClausePPKey,
	    Key, Size, Adg, Gvars, Ldg, Type, Cuts, TestLits, RSol, []).

:- pred body_binding_/17 :: clausebody * nnegint * approx * list(bottom_entry)
	* list(symbol_entry) * list(predname) * clause_ppkey_t * atm * list
	* list * list * list * symbol_field * nnegint * list(test_t)
	* list(comp_t) * list + (not_fails, is_det).
body_binding_(Body, LitNum, Approx, BT, ST, Comp, ClausePPKey, Key, Size, Adg,
	    Gvars, Ldg, Type, Cuts, TestLits, RSol, VarBindings0) :-
	body_binding__(Body, LitNum, Approx, BT, ST, Comp, ClausePPKey, Key,
	    Size, Adg, Gvars, Ldg, Type, Cuts, TestLits, RSol, VarBindings0,
	    VarsBindings),
	insert_vars_bindings(VarsBindings, Approx, BT, ST, ClausePPKey, Key,
	    Gvars, det).

insert_vars_bindings([], _, _, _, _, _, _, _).
insert_vars_bindings([vb(V, B)|VBs], Approx, BT, ST, ClausePPKey, Key, GVars,
	    Type) :-
	insert_vars_binding(V, Approx, BT, ST, ClausePPKey, Key, GVars, Type,
	    B),
	insert_vars_bindings(VBs, Approx, BT, ST, ClausePPKey, Key, GVars,
	    Type).

:- pred body_binding__/18 :: clausebody * nnegint * approx * list(bottom_entry)
	* list(symbol_entry) * list(predname) * clause_ppkey_t * atm * list
	* list * list * list * symbol_field * nnegint * list(test_t)
	* list(comp_t) * list * list + (not_fails, is_det).
body_binding__((LitPPKey, Body), LitNum, Approx, BT, ST, Comp, ClausePPKey,
	    Key, Size, Adg, Gvars, Ldg, Type, Cuts, TestLits, RSol,
	    VarsBindings0, VarsBindings) :-
	!,
	lit_ppkey(LitPPKey, Lit, PPKey),
	literal_binding(Lit, PPKey, LitNum, Approx, BT, ST, Comp, ClausePPKey,
	    Key, Size, Adg, Gvars, Ldg, Type, Cuts, TestLits, RSol, VarsBinding
	),
	add_vars_bindings(VarsBinding, VarsBindings0, VarsBindings1),
	(
	    LitPPKey == (!) ->
	    Cuts1 is Cuts -1
	;
	    Cuts1 = Cuts
	),
	LitNum1 is LitNum + 1,
	body_binding__(Body, LitNum1, Approx, BT, ST, Comp, ClausePPKey, Key,
	    Size, Adg, Gvars, Ldg, Type, Cuts1, TestLits, RSol, VarsBindings1,
	    VarsBindings).
body_binding__(LitPPKey, LitNum, Approx, BT, ST, Comp, ClausePPKey,
	    Key, Size, Adg, Gvars, Ldg, Type, Cuts, TestLits, RSol,
	    VarsBindings0, VarsBindings) :-
	lit_ppkey(LitPPKey, Lit, PPKey),
	literal_binding(Lit, PPKey, LitNum, Approx, BT, ST, Comp, ClausePPKey,
	    Key, Size, Adg, Gvars, Ldg, Type, Cuts, TestLits, RSol,
	    VarsBinding),
	add_vars_bindings(VarsBinding, VarsBindings0, VarsBindings).

set_vars_binding(V, B, VB) :-
	var(V),
	!,
	VB = [vb(V, B)].
set_vars_binding([],     _, []).
set_vars_binding([V|Vs], B, [vb(V, B)|VBs]) :-
	set_vars_binding(Vs, B, VBs).

add_vars_bindings([],     B, B).
add_vars_bindings([A|As], B, C) :-
	add_vars_binding(B, A, B0),
	add_vars_bindings(As, B0, C).

add_vars_binding(B, A, C) :-
	A = vb(VarA, BindingA),
	add_var_binding(B, A, VarA, BindingA, C).

add_var_binding([],     A, _,    _,        [A]).
add_var_binding([B|Bs], A, VarA, BindingA, C) :-
	B = vb(VarB, BindingB),
	(
	    VarA == VarB ->
	    C = [vb(VarA, BindingA + BindingB)|Bs]
	;
	    C = [B|Cs],
	    add_vars_binding(Bs, A, Cs)
	).

:- pred literal_binding/18 :: callable * bodykey * nnegint * approx *
	list(bottom_entry) * list(symbol_entry) * list(predname) *
	clause_ppkey_t * atm * term * term * term * term * term * nnegint *
	list * term * list + (not_fails, is_det) # "Compute the number
	of literal input and output bindings.".
literal_binding(Lit, PPKey, LitNum, Approx, BT, ST, Comp, ClausePPKey, Key,
	    Size, Adg, Gvars, Ldg, Type, Cuts, TestLits, RSol, VarsBinding) :-
	functor(Lit, F, A),
	(
	    second_order_predicate(F/A) ->
	    literal_binding_1(Lit, PPKey, LitNum, Approx, BT, ST, Comp,
		ClausePPKey, Key, Size, Adg, Gvars, Ldg, Type, Cuts, TestLits,
		RSol, VarsBinding)
	;
	    literal_binding_2(Lit, PPKey, LitNum, Approx, BT, ST, Comp,
		ClausePPKey, Key, Size, Adg, Gvars, Ldg, Type, Cuts, TestLits,
		RSol, VarsBinding)
	).

:- pred literal_binding_1/18 :: callable * bodykey * nnegint * approx *
	list(bottom_entry) * list(symbol_entry) * list(predname) *
	clause_ppkey_t * atm * term * term * term * term * term * nnegint *
	list * term * list + (not_fails, is_det).
literal_binding_1(Lit, PPKey, LitNum, Approx, BT, ST, Comp, ClausePPKey, Key,
	    Size, Adg, Gvars, Ldg, Type, Cuts, TestLits, RSol, VarsBinding) :-
	second_order_predicate_pred_arg(Lit, Lit1),
	clause_body(ClausePPKey, Body),
	second_order_predicate_pred_num(Body, LitNum, Num),
	literal_input_binding(Lit1, PPKey, Num, Approx, BT, ST, ClausePPKey,
	    Key, Adg, Gvars, Ldg, Type, TestLits, Binding, _),
	(
	    Type == relation ->
	    literal_output_binding_rel_1(Lit1, Num, Lit, LitNum, Approx, BT,
		ST, Comp, ClausePPKey, Size, Adg, Gvars, Ldg, RSol, Binding),
	    VarsBinding = []
	;
	    literal_output_binding_det_1(Lit1, Num, Lit, LitNum, BT, ST, Comp,
		ClausePPKey, Size, Adg, Gvars, Ldg, RSol, Cuts, Binding,
		VarsBinding)
	).

:- pred literal_binding_2/18 :: callable * bodykey * nnegint * approx *
	list(bottom_entry) * list(symbol_entry) * list(predname) *
	clause_ppkey_t * atm * term * term * term * term * term * nnegint *
	list * term * list + (not_fails, is_det).
literal_binding_2(Lit, PPKey, LitNum, Approx, BT, ST, Comp, ClausePPKey, Key,
	    Size, Adg, Gvars, Ldg, Type, Cuts, TestLits, RSol, VarsBinding) :-
	literal_input_binding(Lit, PPKey, LitNum, Approx, BT, ST, ClausePPKey,
	    Key, Adg, Gvars, Ldg, Type, TestLits, Binding, Subsume),
	(
	    Type == relation ->
	    literal_output_binding_rel(Lit, PPKey, LitNum, Approx, BT, ST,
		Comp, ClausePPKey, Key, Size, Adg, Gvars, Ldg, RSol, Binding,
		Subsume),
	    VarsBinding = []
	;
	    literal_output_binding_det(Lit, PPKey, LitNum, Approx, BT, ST,
		Comp, ClausePPKey, Key, Size, Adg, Gvars, Ldg, RSol, Cuts,
		Binding, Subsume, VarsBinding)
	).

%  Compute the number of literal input bindings.
%
literal_input_binding(Lit, PPKey, LitNum, Approx, BT, ST, ClausePPKey, Key,
	    Adg, Gvars, Ldg, Type, TestLits, Binding, SubsumeLit) :-
	functor(Lit, F, A),
	LitName = (F/A),
	gen_literal_iopos(Adg, LitName, LitNum, (+), Pos),
	pos_var(Pos, Lit, Vars),
	var_def_list(Vars, Gvars, Lvars),
	(
	    Type == relation ->
	    vars_binding(Lvars, Gvars, Ldg, redge, ClausePPKey, Binding2)
	;
	    vars_binding(Lvars, Gvars, Ldg, sedge, ClausePPKey, Binding2)
	),
	filter_effect(TestLits, Vars, Binding2, Binding1),
	literal_property(BT, ST, Lit, ClausePPKey, Key, PPKey, LitNum,
	    relation, Approx, Rel),
	minimum(Binding1, Rel, Binding),
	(
	    opt_cond1(Lvars, ClausePPKey, Vars, Subsume) ->
	    (
		opt_cond2(Subsume, Ldg, Binding1, Type) ->
		(
		    opt_cond3(Binding1, Binding) ->
		    SubsumeLit = Subsume
		;
		    true
		)
	    ;
		true
	    )
	;
	    true
	).

%
%  Compute the number of literal output bindings.
%
literal_output_binding_rel(Lit, PPKey, LitNum, Approx, BT, ST, _, ClausePPKey,
	    Key, Size, Adg, Gvars, Ldg, _, InBinding, SubsumeLit) :-
	functor(Lit, F, A),
	LitName = (F/A),
	literal_output_comp(LitName, Lit, ClausePPKey, Key, PPKey, LitNum, 1,
	    Approx, BT, ST, [], Adg, det, [], Sol),
	normalize_solution_function(Sol, LitName, LitNum, Approx,
	    BT, ST, [], ClausePPKey, Key, Adg, Gvars, Size, [], Sol1),
	multiply(InBinding, Sol1, Sol2),
	literal_property(BT, ST, Lit, ClausePPKey, Key, PPKey, LitNum,
	    relation, Approx, Rel),
	minimum(Sol2, Rel, Binding),
	gen_literal_iopos(Adg, LitName, LitNum, (-), Pos),
	pos_var(Pos, Lit, Vars),
	new_lit(LitNum, NLit),
	insert_ldg_field(Ldg, NLit, relation, Binding),
	insert_vars_binding(Vars, Approx, BT, ST, ClausePPKey, Key, Gvars,
	    relation, Binding),
	(
	    nonvar(SubsumeLit) ->
	    (
		opt_cond4(Sol2, Binding) ->
		(
		    find_ldg_field(Ldg, SubsumeLit, redge, SLits),
		    insert_ldg_field(Ldg, NLit, redge, [SubsumeLit|SLits])
		)
	    ;
		true
	    )
	;
	    true
	).

% PLG: anonymate variables to avoid warning messages (24-Mar-97). (not
% done
%% literal_output_binding_rel_1(Lit, LitNum, OLit, OLitNum, BT, ST, _, Clause, Size, Adg,
%% 		Gvars, Ldg, _, InBinding) :-
%% 	arg(3, OLit, Arg3),
%% 	term_var(Arg3, Var3),
%% 	insert_ldg_field(Ldg, NLit, relation, InBinding),
%% 	insert_vars_binding(Var3, BT, ST, Clause, Gvars, relation, InBinding).

:- push_prolog_flag(single_var_warnings, off).

literal_output_binding_rel_1(Lit, LitNum, OLit, OLitNum, Approx, BT, ST, _,
	    ClausePPKey, Size, Adg, Gvars, Ldg, _, InBinding) :-
	arg(3, OLit, Arg3),
	term_var(Arg3, Var3),
	insert_ldg_field(Ldg, NLit, relation, InBinding),
	insert_vars_binding(Var3, Approx, BT, ST, ClausePPKey, Key, Gvars,
	    relation, InBinding).

%
literal_output_binding_det_1(Lit, LitNum, OLit, OLitNum, BT, ST, Comp,
	    ClausePPKey, Size, Adg, Gvars, Ldg, RSol, Cuts, InBinding,
	    VarsBindings) :-
	arg(3, OLit, Arg3),
	term_var(Arg3, Var3),
	insert_ldg_field(Ldg, NLit, det, InBinding),
	set_vars_binding(Var3, InBinding, VarsBindings).

:- pop_prolog_flag(single_var_warnings).

literal_output_binding_det(Lit, PPKey, LitNum, Approx, BT, ST, Comp,
	    ClausePPKey, Key, Size, Adg, Gvars, Ldg, RSol, Cuts, InBinding,
	    SubsumeLit, VarsBinding) :-
	functor(Lit, F, A),
	LitName = (F/A),
	(
	    Cuts > 0 ->
	    Binding = 1
	;
	    (
		literal_output_comp(LitName, Lit, ClausePPKey, Key, PPKey,
		    LitNum, 1, Approx, BT, ST, Comp, Adg, det, RSol, Sol),
		normalize_solution_function(Sol, LitName, LitNum, Approx, BT,
		    ST, Comp, ClausePPKey, Key, Adg, Gvars, Size, RSol, Sol1),
		multiply(InBinding, Sol1, Sol2),
		literal_property(BT, ST, Lit, ClausePPKey, Key, PPKey, LitNum,
		    relation, Approx, Rel),
		minimum(Sol2, Rel, Binding)
	    )
	),
	gen_literal_iopos(Adg, LitName, LitNum, (-), Pos),
	pos_var(Pos, Lit, Vars),
	new_lit(LitNum, NLit),
	insert_ldg_field(Ldg, NLit, det, Binding),
	set_vars_binding(Vars, Binding, VarsBinding),
	(
	    nonvar(SubsumeLit) ->
	    (
		opt_cond4(Sol2, Binding) ->
		(
		    find_ldg_field(Ldg, SubsumeLit, sedge, SLits),
		    insert_ldg_field(Ldg, NLit, sedge, [SubsumeLit|SLits])
		)
	    ;
		true
	    )
	;
	    true
	).

%
%  Normalize a solution function of a literal.
%
normalize_solution_function(LitSol, LitName, LitNum, Approx, BT, ST, Comp,
	    ClausePPKey, Key, Adg, Gvars, Size, RSol, Sol) :-
	gen_clause_pos(Adg, PosSet),
	(
	    recursive_clause(ClausePPKey, Comp) ->
	    (
		ith_clause_literal(0, ClausePPKey, LitPPKey),
		lit_ppkey(LitPPKey, Lit, _PPKey),
		functor(Lit, F, N),
		find_symbol_field(ST, F/N, size, ISz)
	    )
	;
	    ISz = Size
	),
	gen_literal_iopos(Adg, LitName, LitNum, (+), Pos),
	init_normalize_queue(Pos, QHd, QTl),
	normalize(LitSol, QHd, QTl, Approx, BT, ST, [], ClausePPKey, Key, Adg,
	    Gvars, PosSet, ISz, RSol, Sol).

%
%  Estimate the number of input tuples for the head.
%
head_input_tuple_size(Vars, _Approx, _BT, _ST, _ClausePPKey, _, _Gvars, 1) :-
	var(Vars),
	!.
head_input_tuple_size(Vars, Approx, BT, ST, ClausePPKey, Key, Gvars, Size) :-
	nonvar(Vars),
	use_list(Vars, Gvars, UseList),
%	structure_isort(UseList, 1, SUseList),
	lit_relation_size(ClausePPKey, Key, Approx, BT, ST, LitList),
	head_active_lit(UseList, LitList, ClausePPKey, [], ActiveList),
	head_size(ActiveList, 2, Size).

%
%  Collect the set of uses for the input variables in the head.
%
use_list(Vars, _Gvars, []) :-
	var(Vars),
	!.
use_list(Vars, Gvars, [use(Len, UsePos)|UseList]) :-
	nonvar(Vars),
	Vars = [V|VList],
	find_gvars_field(Gvars, V, use, UsePos),
	length(UsePos, Len),
	use_list(VList, Gvars, UseList).

:- pred lit_relation_size/6 :: clause_ppkey_t * atm * approx *
	list(bottom_entry) * list(symbol_entry) * list # "Collect the
	relation size for the literals in a clause.  Only non-infinite
	relation literals are collected.".
lit_relation_size(ClausePPKey, Key, Approx, BT, ST, LitList) :-
	clause_type(ClausePPKey, Type),
	lit_relation_size_(Type, ClausePPKey, Key, Approx, BT, ST, LitList).

lit_relation_size_(rule, ClausePPKey, Key, Approx, BT, ST, LitList) :-
	clause_body(ClausePPKey, Body),
	body_relation_size(Body, 1, ClausePPKey, Key, Approx, BT, ST, LitList).
lit_relation_size_(fact, _, _, _, _, _, []).

:- pred body_relation_size/8 :: clausebody * nnegint * clause_ppkey_t * atm
	* approx * list(bottom_entry) * list(symbol_entry) * list.
body_relation_size((LitPPKey, Body), LitNum, ClausePPKey, Key, Approx, BT, ST,
	    LitList) :-
	!,
	body_relation_size_lit(LitPPKey, LitNum, ClausePPKey, Key, Approx, BT,
	    ST, LitList, LitLists),
	LitNum1 is LitNum + 1,
	body_relation_size(Body, LitNum1, ClausePPKey, Key, Approx, BT, ST,
	    LitLists).
body_relation_size(LitPPKey, LitNum, ClausePPKey, Key, Approx, BT, ST, LitList)
:- body_relation_size_lit(LitPPKey, LitNum, ClausePPKey, Key, Approx, BT, ST,
	    LitList, []).

body_relation_size_lit(LitPPKey, LitNum, ClausePPKey, Key, Approx, BT, ST,
	    LitList, LitLists) :-
	lit_ppkey(LitPPKey, Lit, PPKey),
	functor(Lit, F, N),
	(
	    second_order_predicate(F/N) ->
	    second_order_predicate_pred_arg(Lit, NLit)
	;
	    NLit = Lit
	),
	literal_property(BT, ST, NLit, ClausePPKey, Key, PPKey, LitNum,
	    relation, Approx, Rel),
	(
	    Rel == inf ->
	    LitList = LitLists
	;
	    LitList = [lit(LitNum, Rel)|LitLists]
	).

%
head_active_lit(_,  [],      _, _,          []) :- !. % fact
head_active_lit([], LitList, _, ActiveList, ActiveList) :-
	LitList \== [],
	!.
head_active_lit([use(_, [])|_], LitList, _, _, []) :-
	LitList \== [],
	!. % dangling var
head_active_lit([use(_, U)|UseList], LitList, ClausePPKey, AList,
	    ActiveList) :-
	LitList \== [],
	U \== [],
	least_lit(U, LitList, ClausePPKey, _, inf, Lit),
	(
	    var(Lit) ->
	    ActiveList = [] ; % all uses are in the head or infinite lit
	    (
		arg(1, Lit, LitNum),
		(
		    structure_member(AList, 1, LitNum) ->
		    AList1 = AList
		; % vars appear in same lit 
		    AList1 = [Lit|AList]
		),
		head_active_lit(UseList, LitList, ClausePPKey, AList1,
		    ActiveList)
	    )
	).

structure_member([S|_SList], I, M) :-
	arg(I, S, Arg),
	Arg == M,
	!.
structure_member([S|SList], I, M) :-
	arg(I, S, Arg),
	Arg \== M,
	structure_member(SList, I, M).

%
least_lit([],        _,       _,           Lit, _,   Lit).
least_lit([U|UList], LitList, ClausePPKey, Lit, Rel, MLit) :-
	pos_litnum(U, LitNum),
	(
	    LitNum > 0 ->
	    (
		clause_body(ClausePPKey, Body),
		number_of_literals(Body, 1, Num),
		NLitNum is LitNum-Num
	    )
	;
	    NLitNum = LitNum
	),
	(
	    structure_find(LitList, 1, NLitNum, NLit) ->
	    (
		arg(2, NLit, NRel),
		minimum(Rel, NRel, MRel),
		(
		    MRel == NRel ->
		    least_lit(UList, LitList, ClausePPKey, NLit, NRel, MLit)
		;
		    least_lit(UList, LitList, ClausePPKey, Lit, Rel, MLit)
		)
	    )
	;
	    ( /* LitNum == 0 or an infinite relation lit*/
		minimum(Rel, inf, NRel),
		least_lit(UList, LitList, ClausePPKey, Lit, NRel, MLit)
	    )
	).

%
structure_find([L|_LitList], I, LitNum, L) :-
	arg(I, L, Arg),
	Arg == LitNum,
	!.
structure_find([L|LitList], I, LitNum, Lit) :-
	arg(I, L, Arg),
	Arg \== LitNum,
	structure_find(LitList, I, LitNum, Lit).

%
%  Compute the number of input tuples for the head by multiplying the
%  relation sizes of the active literals.
%
head_size([], _, inf) :- !.
head_size(S,  I, Value) :-
	S \== [],
	structure_multiply(S, I, Value).

structure_multiply([],            _I, 1).
structure_multiply([S|Structure], I,  Value) :-
	arg(I, S, Val),
	structure_multiply(Structure, I, Values),
	multiply(Val, Values, Value).

%
%  Compute the number of bindings of a set of variables.
%
find_vars_binding(Vars, _, _, TBinding, TBinding) :-
	var(Vars),
	!.
find_vars_binding(Vars, Gvars, Type, TBinding, FBinding) :-
	nonvar(Vars),
	Vars = [Var|VList],
	find_gvars_field(Gvars, Var, Type, VBinding),
	multiply(TBinding, VBinding, TBinding1),
	find_vars_binding(VList, Gvars, Type, TBinding1, FBinding).

%
%  Compute the number of bindings for a set of variables.
%
insert_vars_binding(Vars, _, _, _, _, _, _, _, _) :-
	var(Vars),
	!.
insert_vars_binding(Vars, Approx, BT, ST, ClausePPKey, Key, Gvars, Type,
	    Default) :-
	nonvar(Vars),
	Vars = [Var|VList],
	insert_var_binding(Var, Approx, BT, ST, ClausePPKey, Key, Gvars, Type,
	    Default),
	insert_vars_binding(VList, Approx, BT, ST, ClausePPKey, Key, Gvars,
	    Type, Default).
/*
	( Default == Binding ->
	    Mvars = [ Var|MList ] ;
	    Mvars = MList ),
	*/

%
%  Insert the number of bindings of a variables into ground variable list.
%
insert_var_binding(Var, Approx, BT, ST, ClausePPKey, Key, Gvars, Type,
	    Default) :-
	find_gvars_field(Gvars, Var, use, Pos),
	min_use_binding(Pos, Approx, BT, ST, ClausePPKey, Key, Binding1),
	minimum(Binding1, Default, Binding),
	simplification(Binding, SBinding),
	insert_gvars_field(Gvars, Var, Type, SBinding).

%
%  Compute the minimum number of bindings of a variable among its use 
%  positions.
%
min_use_binding([],          _,      _,  _,  _,           _,   inf).
min_use_binding([Pos|PList], Approx, BT, ST, ClausePPKey, Key, Binding) :-
	pos_litnum(Pos, I),
	min_use_binding_litnum(I, PList, Approx, BT, ST, ClausePPKey, Key,
	    Binding).

min_use_binding_litnum(0, PList, Approx, BT, ST, ClausePPKey, Key, Binding) :-
	!,
	min_use_binding(PList, Approx, BT, ST, ClausePPKey, Key, Binding).
min_use_binding_litnum(I, PList, Approx, BT, ST, ClausePPKey, Key, Binding) :-
	I > 0,
	clause_body(ClausePPKey, Body),
	number_of_literals(Body, 1, Num),
	(
	    I > Num ->
	    NI is I-Num
	;
	    NI = I
	),
	ith_clause_literal(NI, ClausePPKey, NLitPPKey),
	lit_ppkey(NLitPPKey, NLit, PPKey),
	(
	    I > Num ->
	    second_order_predicate_pred_arg(NLit, Lit)
	;
	    Lit = NLit
	),
	literal_property(BT, ST, Lit, ClausePPKey, Key, PPKey, NI, relation,
	    Approx, Rel),
	min_use_binding(PList, Approx, BT, ST, ClausePPKey, Key, Binding1),
	minimum(Rel, Binding1, Binding).

%
%  Find the set of variables occurring at a set of positions.
%
pos_var([],          _Lit, _Vars).
pos_var([Pos|PList], Lit,  Vars) :-
	pos_argnum(Pos, ArgNum),
	arg(ArgNum, Lit, Arg),
	term_var(Arg, Vars),
	pos_var(PList, Lit, Vars).

%
%  Find the set of variables occurring in a term.
%
term_var(Term, _) :-
	atomic(Term),
	!.
term_var(Term, Vars) :-
	var(Term),
	!,
	opened_set_insertion(Vars, Term).
term_var(Term, Vars) :-
	compound(Term),
	functor(Term, _, N),
	term_var_(N, Term, Vars).

term_var_(0, _Term, _Vars) :-
	!.
term_var_(N, Term, Vars) :-
	N > 0,
	arg(N, Term, Arg),
	term_var(Arg, Vars),
	N1 is N -1,
	term_var_(N1, Term, Vars).

%
%  Find the defining literal number of a variable.
%
var_def_lit(Gvars, Var, LitNum) :-
	find_gvars_field(Gvars, Var, def, Def),
	Def = [Pos|_],
	pos_litnum(Pos, LitNum).

%
%  Establish variables defining literals list by dividing the set of variables
%  based on the defining literals and sorting them in decreasing order.
%
var_def_list(Vars, Gvars, SVarList) :-
	var_d_list(Vars, Gvars, VarList),
	msort(VarList, SVarList).

var_d_list(Vars, _, VarList) :-
	var(Vars),
	!,
	close_list(VarList).
var_d_list(Vars, Gvars, VarList) :-
	nonvar(Vars),
	Vars = [Var|VList],
	var_def_lit(Gvars, Var, LitNum),
	insert_var_division(VarList, Var, LitNum),
	var_d_list(VList, Gvars, VarList).

%
%  Sort a list of element in decreasing order.
%
msort([],  []).
msort([X], [X]).
msort(LL,  R) :- LL = [_, _|_],
	divide(LL, L1, L2),
	msort(L1, R1), msort(L2, R2),
	merge(R1, R2, R).

%
%  Divide a list into two lists.
%
divide([],         [],      []).
divide([X],        [X],     []).
divide([X1, X2|L], [X1|L1], [X2|L2]) :- divide(L, L1, L2).

%
%  Merge two lists into a list in decreasing order.
%
merge([],      [],      []).
merge(L,       [],      L) :- L = [_|_].
merge([],      L,       L) :- L = [_|_].
merge([X1|L1], [X2|L2], [X1|L]) :-
	X1 = cv(LitNum1, _),
	X2 = cv(LitNum2, _),
	LitNum1 >= LitNum2,
	merge(L1, [X2|L2], L).
merge([X1|L1], [X2|L2], [X2|L]) :-
	X1 = cv(LitNum1, _),
	X2 = cv(LitNum2, _),
	LitNum1 < LitNum2,
	merge([X1|L1], L2, L).

%
%  Insert a variable into the variables defining literals list.
%
insert_var_division(Vars, Var, LitNum) :-
	var(Vars),
	!,
	Vars = [cv(LitNum, [Var|_])|_].
insert_var_division(Vars, Var, LitNum) :-
	nonvar(Vars),
	Vars = [cv(LitNum1, List)|VList],
	(
	    LitNum == LitNum1 ->
	    opened_set_insertion(List, Var)
	;
	    insert_var_division(VList, Var, LitNum)
	).

%
%  Compute the number of bindings of a set of variables which is in the form
%  of variables defining literals list.
%
vars_binding([],            _,     _,   _,    _,           1).
vars_binding([HdLit|TlLit], Gvars, Ldg, Type, ClausePPKey, Binding) :-
	HdLit = cv(_, HdVars),
	var(HdVars),
	!,
	vars_binding(TlLit, Gvars, Ldg, Type, ClausePPKey, Binding).
vars_binding([HdLit|TlLit], Gvars, Ldg, Type, ClausePPKey, Binding) :-
	HdLit = cv(LitNum, HdVars),
	nonvar(HdVars),
	(
	    LitNum =:= 0 ->
	    new_lit(+, Lit)
	;
	    new_lit(LitNum, Lit)
	),
	optimize_var_lits(LitNum, HdVars, TlLit, Ldg, Type, ClausePPKey, NTlLit
	),
	(
	    Type == redge ->
	    (
		find_vars_binding(HdVars, Gvars, relation, 1, Binding1),
		find_ldg_field(Ldg, Lit, relation, Binding2)
	    )
	;
	    (
		find_vars_binding(HdVars, Gvars, det, 1, Binding1),
		find_ldg_field(Ldg, Lit, det, Binding2)
	    )
	),
	minimum(Binding1, Binding2, Binding3),
	vars_binding(NTlLit, Gvars, Ldg, Type, ClausePPKey, Binding4),
	multiply(Binding3, Binding4, Binding).

opt_cond1([cv(LitNum, _)|Lvars], ClausePPKey, IVars, Subsume) :-
	ith_clause_literal(LitNum, ClausePPKey, LitPPKey),
	lit_ppkey(LitPPKey, Lit, _PPKey),
	term_var(Lit, LitVars),
	(
	    opened_set_inclusion(IVars, LitVars) ->
	    new_lit(LitNum, Subsume)
	;
	    opt_cond1(Lvars, ClausePPKey, IVars, Subsume)
	).

opt_cond2(Lit, Ldg, Instance, Type) :-
	find_ldg_field(Ldg, Lit, Type, OBinding),
	Instance == OBinding.

opt_cond3(Instance, IBinding) :- Instance == IBinding.

opt_cond4(Instance, OBinding) :- Instance == OBinding.

optimize_var_lits(_, HdVars, _, _, _, _, _) :-
	var(HdVars),
	!.
optimize_var_lits(HdLit, HdVars, TlLit, Ldg, Type, ClausePPKey, NTlLit) :-
	new_lit(HdLit, Lit),
	find_ldg_field(Ldg, Lit, Type, SubsumeLits),
	remove_subsumed_lits(TlLit, SubsumeLits, HdVars, TTlLit),
	find_ldg_field(Ldg, Lit, pred, PredLits),
	ith_clause_literal(HdLit, ClausePPKey, LiteralPPKey),
	lit_ppkey(LiteralPPKey, Literal, _PPKey),
	term_var(Literal, LitVars),
	merge_same_lit_vars(TTlLit, PredLits, LitVars, HdVars, NTlLit).

remove_subsumed_lits([], _, _, []).
remove_subsumed_lits([cv(LitNum, LitVars)|TlLit], SubsumeLits, HdVars,
	    NTlLit) :-
	new_lit(LitNum, Lit),
	( opened_set_member(SubsumeLits, Lit) ->
	    merge_subsumed_vars(LitVars, HdVars),
	    NTlLit = NTlLits
	;
	    NTlLit = [cv(LitNum, LitVars)|NTlLits]
	),
	remove_subsumed_lits(TlLit, SubsumeLits, HdVars, NTlLits).

merge_subsumed_vars(LitVars, _HdVars) :-
	var(LitVars),
	!.
merge_subsumed_vars(LitVars, HdVars) :-
	nonvar(LitVars),
	LitVars = [V|Vars],
	opened_set_insertion(HdVars, V),
	merge_subsumed_vars(Vars, HdVars).

merge_same_lit_vars([], _, _, _, []).
merge_same_lit_vars([cv(LitNum, LitVars)|TlLit], PredLits, CVars, HdVars,
	    NTlLit) :-
	new_lit(LitNum, Lit),
	(
	    opened_set_member(PredLits, Lit) ->
	    (
		same_lit_vars(LitVars, CVars, HdVars, NLitVars),
		(
		    var(NLitVars) ->
		    NTlLit = NTlLits
		;
		    NTlLit = [cv(LitNum, NLitVars)|NTlLits]
		)
	    )
	;
	    NTlLit = [cv(LitNum, LitVars)|NTlLits]
	),
	merge_same_lit_vars(TlLit, PredLits, CVars, HdVars, NTlLits).

same_lit_vars(LitVars,  _,     _,      _) :- var(LitVars), !.
same_lit_vars([V|Vars], CVars, HdVars, NLitVars) :-
	(
	    opened_set_member(CVars, V) ->
	    opened_set_insertion(HdVars, V)
	;
	    opened_set_insertion(NLitVars, V)
	),
	same_lit_vars(Vars, CVars, HdVars, NLitVars).

%
filter_effect([],                        _Vars, Binding,  Binding).
filter_effect([test(Lit, Rel)|TestLits], Vars,  Binding1, Binding) :-
	term_var(Lit, Var1), % Lit is test literal
	(
	    opened_set_inclusion(Vars, Var1) ->
	    minimum(Rel, Binding1, Binding2)
	;
	    Binding2 = Binding1
	),
	filter_effect(TestLits, Vars, Binding2, Binding).

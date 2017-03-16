:- module(binding,
	[
	    normalize_solution_function/12,
	    pos_var/3,
	    term_var/2,
	    set_vars_binding/3,
	    solution_head_output/7,
	    body_binding/14,
	    body_binding/15,
	    body_binding/16,
	    solution_head_input/4,
	    solution_head_input/7,
	    relation_head_input/8,
	    relation_head/6
	], [assertions]).

%
%  binding.pl			Nai-Wei Lin			January, 1992
%
%  This file contains the procedures for computing the binding pattern.
%  

:- use_module(library(lists), [length/2]).

:- use_module(infercost(dependency(ldg)), 
	[
	    new_lit/2,
	    insert_ldg_field/4,
	    find_ldg_field/4
	]).
:- use_module(infercost(dependency(position)), 
	[
	    gen_literal_iopos/5,
	    gen_literal_pos/3,
	    gen_clause_pos/2,
	    pos_litnum/2,
	    pos_argnum/2
	]).
:- use_module(infercost(top(utility)), 
	[
	    nonsequence/1,
	    minimum/3,
	    multiply/3,
	    opened_set_inclusion/2,
	    opened_set_insertion/2,
	    compound/1,
	    close_list/1,
	    opened_set_member/2
	]).
:- use_module(infercost(init(builtin)), 
	[
	    second_order_predicate/1,
	    second_order_predicate_pred_arg/2,
	    second_order_predicate_pred_num/3
	]).
:- use_module(infercost(init(symtable)), 
	[
	    literal_property/5,
	    find_symbol_field/4
	]).
:- use_module(infercost(init(initsystem_basic)), [clause_type/2]).
:- use_module(infercost(size(normalize_)), 
	[
	    literal_output_comp/11,
	    init_normalize_queue/3,
	    normalize/13
	]).
:- use_module(infercost(size(clause)), 
	[
	     ith_clause_literal/3,
	     number_of_literals/3
	]).
:- use_module(infercost(algebraic(simpl_form)), [simplification/2]).
:- use_module(infercost(solution(relation)), [recursive_clause/2]).
:- use_module(infercost(dependency(gvars)), 
	[
	    find_gvars_field/4,
	    insert_gvars_field/4
	]).

%
%  Compute the number of head input bindings for relation size analysis.
%
relation_head_input(Head, BT, ST, Clause, Adg, Gvars, Ldg, Sol) :-
	new_lit(+, Lit),
	functor(Head, F, A),
	gen_literal_iopos(Adg, F/A, 0, (+), Pos),
	pos_var(Pos, Head, Vars),
	head_input_tuple_size(Vars, BT, ST, Clause, Gvars, Sol),
	insert_ldg_field(Ldg, Lit, relation, Sol),
	insert_vars_binding(Vars, BT, ST, Clause, Gvars, relation, Sol).
	%insert_ldg_field(Ldg, Lit, redge, Mvars).

%
%  Compute the number of head bindings for relation size analysis.
%
relation_head(Head, Clause, Gvars, Ldg, TestLits, Binding) :-
	functor(Head, F, A),
	gen_literal_pos(F/A, 0, Pos),
	pos_var(Pos, Head, Vars),
	var_def_list(Vars, Gvars, Lvars),
	vars_binding(Lvars, Gvars, Ldg, redge, Clause, Binding1),
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

solution_head_input(Head, BT, ST, Clause, Adg, Gvars, Ldg) :-
	solution_head_input(Head, Adg, Ldg, Vars),
	insert_vars_binding(Vars, BT, ST, Clause, Gvars, det, 1).

%
%  Compute the number of head output bindings for solution size analysis.
%
solution_head_output(Head, Clause, Adg, Gvars, Ldg, TestLits, Binding) :-
	functor(Head, F, A),
	gen_literal_iopos(Adg, F/A, 0, (-), Pos),
	pos_var(Pos, Head, Vars),
	var_def_list(Vars, Gvars, Lvars),
	vars_binding(Lvars, Gvars, Ldg, sedge, Clause, Binding1),
	filter_effect(TestLits, Vars, Binding1, Binding).

%
%  Compute the number of body input and output bindings.
%

body_binding(LitNum, Lit, BT, ST, Comp, Clause, Size, Adg, Gvars, Ldg, Type,
	    Cuts, TestLits, RSol, VarBindings0) :-
	body_binding(LitNum, Lit, BT, ST, Comp, Clause, Size, Adg, Gvars, Ldg,
	    Type, Cuts, TestLits, RSol, VarBindings0, VarsBindings),
	insert_vars_bindings(VarsBindings, BT, ST, Clause, Gvars, det).

body_binding(LitNum, Lit, BT, ST, Comp, Clause, Size, Adg, Gvars, Ldg, Type,
	    Cuts, TestLits, RSol) :-
	body_binding(LitNum, Lit, BT, ST, Comp, Clause, Size, Adg, Gvars, Ldg,
	    Type, Cuts, TestLits, RSol, []).

insert_vars_bindings([],            _BT, _ST, _Clause, _GVars, _Type).
insert_vars_bindings([vb(V, B)|VBs], BT,  ST,  Clause,  GVars,  Type) :-
	insert_vars_binding(V, BT, ST, Clause, GVars, Type, B),
	insert_vars_bindings(VBs, BT, ST, Clause, GVars, Type).

body_binding(LitNum, Lit, BT, ST, Comp, Clause, Size, Adg, Gvars, Ldg, Type,
	    Cuts, TestLits, RSol, VarsBindings0, VarsBindings) :-
	nonsequence(Lit),
	!,
	literal_binding(Lit, LitNum, BT, ST, Comp, Clause, Size, Adg, Gvars,
	    Ldg, Type, Cuts, TestLits, RSol, VarsBinding),
	add_vars_bindings(VarsBinding, VarsBindings0, VarsBindings).
body_binding(LitNum, (Lit, Body), BT, ST, Comp, Clause, Size, Adg, Gvars, Ldg,
	    Type, Cuts, TestLits, RSol, VarsBindings0, VarsBindings) :-
	literal_binding(Lit, LitNum, BT, ST, Comp, Clause, Size, Adg, Gvars,
	    Ldg, Type, Cuts, TestLits, RSol, VarsBinding),
	add_vars_bindings(VarsBinding, VarsBindings0, VarsBindings1),
	(
	    Lit == (!) ->
	    Cuts1 is Cuts-1
	;
	    Cuts1 = Cuts
	),
	LitNum1 is LitNum + 1,
	body_binding(LitNum1, Body, BT, ST, Comp, Clause, Size, Adg, Gvars,
	    Ldg, Type, Cuts1, TestLits, RSol, VarsBindings1, VarsBindings).

set_vars_binding(V, B, VB) :-
	var(V),
	!,
	VB = [vb(V, B)].
set_vars_binding([], _, []).
set_vars_binding([V|Vs], B, [vb(V, B)|VBs]) :-
	set_vars_binding(Vs, B, VBs).

add_vars_bindings([], B, B).
add_vars_bindings([A|As], B, C) :-
	add_vars_binding(B, A, B0),
	add_vars_bindings(As, B0, C).

add_vars_binding(B, A, C) :-
	A = vb(VarA, BindingA),
	add_var_binding(B, A, VarA, BindingA, C).

add_var_binding([], A, _, _, [A]).
add_var_binding([B|Bs], A, VarA, BindingA, C) :-
	B = vb(VarB, BindingB),
	(
	    VarA == VarB ->
	    C = [vb(VarA, BindingA + BindingB) | Bs]
	;
	    C = [B | Cs],
	    add_vars_binding(Bs, A, Cs)
	).

%
%  Compute the number of literal input and output bindings.
%
literal_binding(Lit, LitNum, BT, ST, Comp, Clause, Size, Adg, Gvars, Ldg,
	    Type, Cuts, TestLits, RSol, VarsBinding) :-
	functor(Lit, F, A),
	(
	    second_order_predicate(F/A) ->
	    literal_binding_1(Lit, LitNum, BT, ST, Comp, Clause, Size, Adg,
	        Gvars, Ldg, Type, Cuts, TestLits, RSol, VarsBinding)
	;
	    literal_binding_2(Lit, LitNum, BT, ST, Comp, Clause, Size, Adg,
	        Gvars, Ldg, Type, Cuts, TestLits, RSol, VarsBinding)
	).

literal_binding_1(Lit, LitNum, BT, ST, Comp, Clause, Size, Adg, Gvars, Ldg,
	    Type, Cuts, TestLits, RSol, VarsBinding) :-
	second_order_predicate_pred_arg(Lit, Lit1),
	arg(2, Clause, Body),
	second_order_predicate_pred_num(Body, LitNum, Num),
	literal_input_binding(Lit1, Num, BT, ST, Clause, Adg, Gvars, Ldg, Type,
	    TestLits, Binding, _),
	(
	    Type == relation ->
	    literal_output_binding_rel_1(Lit1, Num, Lit, LitNum, BT, ST, Comp,
	        Clause, Size, Adg, Gvars, Ldg, RSol, Binding),
	    VarsBinding = []
	;
	    literal_output_binding_det_1(Lit1, Num, Lit, LitNum, BT, ST, Comp,
	        Clause, Size, Adg, Gvars, Ldg, RSol, Cuts, Binding,
		VarsBinding)
	).

literal_binding_2(Lit, LitNum, BT, ST, Comp, Clause, Size, Adg, Gvars, Ldg,
	    Type, Cuts, TestLits, RSol, VarsBinding) :-
	literal_input_binding(Lit, LitNum, BT, ST, Clause, Adg, Gvars, Ldg,
	    Type, TestLits, Binding, Subsume),
	(
	    Type == relation ->
	    literal_output_binding_rel(Lit, LitNum, BT, ST, Comp, Clause,
	        Size, Adg, Gvars, Ldg, RSol, Binding, Subsume),
	    VarsBinding = []
	;
	    literal_output_binding_det(Lit, LitNum, BT, ST, Comp, Clause,
	        Size, Adg, Gvars, Ldg, RSol, Cuts, Binding, Subsume,
		VarsBinding)
	).

%  Compute the number of literal input bindings.
%
literal_input_binding(Lit, LitNum, BT, ST, Clause, Adg, Gvars, Ldg, Type,
	    TestLits, Binding, SubsumeLit) :-
	functor(Lit, F, A),
	LitName = (F/A),
	gen_literal_iopos(Adg, LitName, LitNum, (+), Pos),
	pos_var(Pos, Lit, Vars),
	var_def_list(Vars, Gvars, Lvars),
	(
	    Type == relation ->
	    vars_binding(Lvars, Gvars, Ldg, redge, Clause, Binding2)
	;
	    vars_binding(Lvars, Gvars, Ldg, sedge, Clause, Binding2)
	),
	filter_effect(TestLits, Vars, Binding2, Binding1),
	literal_property(BT, ST, LitName, relation, Rel),
	minimum(Binding1, Rel, Binding),
	(
	    opt_cond1(Lvars, Clause, Vars, Subsume) ->
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
literal_output_binding_rel(Lit, LitNum, BT, ST, _, Clause, Size, Adg, Gvars,
	    Ldg, _, InBinding, SubsumeLit) :-
	functor(Lit, F, A),
	LitName = (F/A),
	literal_output_comp(LitName, Lit, LitNum, 1, BT, ST, [], Adg, det, [],
	    Sol),
	%write(Sol), nl,
	normalize_solution_function(Sol, LitName, LitNum, BT, ST, [],
	    Clause, Adg, Gvars, Size, [], Sol1),
	%write(Sol1), nl,
	multiply(InBinding, Sol1, Sol2),
	%write(Sol2), nl,
	literal_property(BT, ST, LitName, relation, Rel),
	%write(Rel), nl,
	minimum(Sol2, Rel, Binding),
	%write(Binding), nl,
	gen_literal_iopos(Adg, LitName, LitNum, (-), Pos),
	pos_var(Pos, Lit, Vars),
	new_lit(LitNum, NLit),
	%find_ldg_field(Ldg, NLit, succ, Succ),
	%min_lit_binding(Succ, Clause, BT, ST, Adg, Vars, Binding, MinBinding),
	insert_ldg_field(Ldg, NLit, relation, Binding),
	insert_vars_binding(Vars, BT, ST, Clause, Gvars, relation, Binding),
	%insert_binding_coverage(Mvars, Gvars, Adg, Ldg, LitName, LitNum,
	%	Clause, relation),
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
/*
min_lit_binding(Succ, _Clause, _BT, _ST, _Adg, _Vars, MinBinding,
	    MinBinding) :-
	var(Succ),
	!.
min_lit_binding(Succ,  Clause,  BT,  ST,  Adg,  Vars, Binding,
	    MinBinding) :-
	nonvar(Succ),
	Succ = [Lit|Succs],
	new_lit(LitNum, Lit),
	(
	    integer(LitNum) ->
	    (
		ith_clause_literal(LitNum, Clause, Lit1),
		functor(Lit1, F1, A1),
		gen_literal_iopos(Adg, F1/A1, LitNum, (+), Pos),
		pos_var(Pos, Lit1, Var1),
		(
		    opened_set_inclusion(Vars, Var1) ->
		    (
			literal_property(BT, ST, F1/A1, relation, Rel),
			minimum(Binding, Rel, Binding1)
		    )
		;
		    Binding1 = Binding)
	    )
	;
	    Binding1 = Binding
	),
	simplification(Binding1, NBinding),
	min_lit_binding(Succs, Clause, BT, ST, Adg, Vars, NBinding,
	    MinBinding).
*/
% PLG: anonymate variables to avoid warning messages (24-Mar-97). (not
% done
 %% literal_output_binding_rel_1(Lit, LitNum, OLit, OLitNum, BT, ST, _, Clause, Size, Adg,
 %% 		Gvars, Ldg, _, InBinding) :-
 %% 	arg(3, OLit, Arg3),
 %% 	term_var(Arg3, Var3),
 %% 	insert_ldg_field(Ldg, NLit, relation, InBinding),
 %% 	insert_vars_binding(Var3, BT, ST, Clause, Gvars, relation, InBinding).

:- push_prolog_flag(single_var_warnings, off).

literal_output_binding_rel_1(Lit, LitNum, OLit, OLitNum, BT, ST, _, Clause,
	    Size, Adg, Gvars, Ldg, _, InBinding) :-
	arg(3, OLit, Arg3),
	term_var(Arg3, Var3),
	insert_ldg_field(Ldg, NLit, relation, InBinding),
	insert_vars_binding(Var3, BT, ST, Clause, Gvars, relation, InBinding).

 /*
	functor(Lit, F, A),
	LitName = (F/A),
	literal_output_comp(LitName, Lit, LitNum, 1, BT, ST, [], Adg, det,
	    [], Sol),
	%write(Sol), nl,
	normalize_solution_function(Sol, LitName, LitNum, BT, ST, [],
	    Clause, Adg, Gvars, Size, [], Sol1),
	literal_property(BT, ST, LitName, det, [Sol1]),
	%write(Sol1), nl,
	multiply(InBinding, Sol1, Sol2),
	%write(Sol2), nl,
	literal_property(BT, ST, LitName, relation, Rel),
	minimum(Sol2, Rel, Binding),
	%write(Binding), nl,
	%
	gen_literal_iopos(Adg, LitName, LitNum, (-), Pos),
	pos_var(Pos, Lit, Vars),
	arg(1, OLit, Arg1),
	term_var(Arg1, Var1),
	arg(3, OLit, Arg3),
	term_var(Arg3, Var3),
	new_lit(OLitNum, NLit),
	(
	    opened_set_equivalent(Var1, Vars) ->
	    (
		insert_ldg_field(Ldg, NLit, relation, InBinding),
		insert_vars_binding(Var3, BT, ST, Clause, Gvars, relation,
		    InBinding)
	    )
	;
	    (
		insert_ldg_field(Ldg, NLit, relation, Binding),
		insert_vars_binding(Var3, BT, ST, Clause, Gvars, relation,
		    Binding)
	    )
	).
*/

%
literal_output_binding_det_1(Lit, LitNum, OLit, OLitNum, BT, ST, Comp, Clause,
	    Size, Adg, Gvars, Ldg, RSol, Cuts, InBinding, VarsBindings) :-
	arg(3, OLit, Arg3),
	term_var(Arg3, Var3),
	insert_ldg_field(Ldg, NLit, det, InBinding),
	set_vars_binding(Var3, InBinding, VarsBindings).

:- pop_prolog_flag(single_var_warnings).

/*
	functor(Lit, F, A),
	LitName = (F/A),
	(
	    Cuts > 0 ->
	    Binding = 1
	;
	    (
		literal_output_comp(LitName, Lit, LitNum, 1, BT, ST, Comp,
		    Adg, det, RSol, Sol),
		 %write(Sol), nl,
		 normalize_solution_function(Sol, LitName, LitNum, BT, ST,
		     Comp, Clause, Adg, Gvars, Size, RSol, Sol1),
		 %write(Sol1), nl,
		 multiply(InBinding, Sol1, Sol2),
		 literal_property(BT, ST, LitName, relation, Rel),
		 minimum(Sol2, Rel, Binding)
	    )
	),
	%
	gen_literal_iopos(Adg, LitName, LitNum, (-), Pos),
	pos_var(Pos, Lit, Vars),
	arg(1, OLit, Arg1),
	term_var(Arg1, Var1),
	arg(3, OLit, Arg3),
	term_var(Arg3, Var3),
	new_lit(OLitNum, NLit),
	(
	    opened_set_equivalent(Var1, Vars) ->
	    (
		insert_ldg_field(Ldg, NLit, det, 1),
		insert_vars_binding(Var3, BT, ST, Clause, Gvars, det, 1)
	    )
	;
	    (
		insert_ldg_field(Ldg, NLit, det, Binding),
		insert_vars_binding(Var3, BT, ST, Clause, Gvars, det, Binding)
	    )
	).
*/

%
literal_output_binding_det(Lit, LitNum, BT, ST, Comp, Clause, Size, Adg, Gvars,
	    Ldg, RSol, Cuts, InBinding, SubsumeLit, VarsBinding) :-
	functor(Lit, F, A),
	LitName = (F/A),
	(
	    Cuts > 0 ->
	    Binding = 1
	;
	    (
		literal_output_comp(LitName, Lit, LitNum, 1, BT, ST, Comp, Adg,
		    det, RSol,Sol),
		 %write(Sol), nl,
		normalize_solution_function(Sol, LitName, LitNum, BT, ST, Comp,
		    Clause, Adg, Gvars, Size, RSol, Sol1),
		 %write(Sol1), nl,
		 multiply(InBinding, Sol1, Sol2),
		 %write(Sol2), nl,
		 literal_property(BT, ST, LitName, relation, Rel),
		 %write(Rel), nl,
		 minimum(Sol2, Rel, Binding)
	    )
	),
	%write(Binding), nl,
	gen_literal_iopos(Adg, LitName, LitNum, (-), Pos),
	pos_var(Pos, Lit, Vars),
	new_lit(LitNum, NLit),
	%find_ldg_field(Ldg, NLit, succ, Succ),
	%min_lit_binding(Succ, Clause, BT, ST, Adg, Vars, Binding, MinBinding),
	insert_ldg_field(Ldg, NLit, det, Binding),
	set_vars_binding(Vars, Binding, VarsBinding),
	%insert_binding_coverage(Mvars, Gvars, Adg, Ldg, LitName, LitNum,
	%	Clause, det),
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
normalize_solution_function(LitSol, LitName, LitNum, BT, ST, Comp, Clause, Adg,
	    Gvars, Size, RSol, Sol) :-
	gen_clause_pos(Adg, PosSet),
	(
	    recursive_clause(Clause, Comp) ->
	    (
		ith_clause_literal(0, Clause, Lit),
		functor(Lit, F, N),
		find_symbol_field(ST, F/N, size, ISz)
	    )
	;
	    ISz = Size
	),
	gen_literal_iopos(Adg, LitName, LitNum, (+), Pos),
	init_normalize_queue(Pos, QHd, QTl),
	normalize(LitSol, QHd, QTl, BT, ST, [], Clause, Adg, Gvars, PosSet,
	    ISz, RSol, Sol).

%
%  Estimate the number of input tuples for the head.
%
head_input_tuple_size(Vars, _BT, _ST, _Clause, _Gvars, 1) :-
	var(Vars),
	!.
head_input_tuple_size(Vars,  BT,  ST,  Clause,  Gvars, Size) :-
	nonvar(Vars),
	use_list(Vars, Gvars, UseList),
%	structure_isort(UseList, 1, SUseList),
	lit_relation_size(Clause, BT, ST, LitList),
	head_active_lit(UseList, LitList, Clause, [], ActiveList),
	head_size(ActiveList, 2, Size).

%
%  Collect the set of uses for the input variables in the head.
%
use_list(Vars, _Gvars, []) :-
	var(Vars),
	!.
use_list(Vars,  Gvars, [use(Len, UsePos)|UseList]) :-
	nonvar(Vars),
	Vars = [V|VList],
	find_gvars_field(Gvars, V, use, UsePos),
	length(UsePos, Len),
	use_list(VList, Gvars, UseList).
/*
%
%  Perform a quick sort in increasing order on a list of structure by using
%  the ith argument as the key.
%
structure_isort([],         _I, []).
structure_isort([U|UseList], I, SUseList) :-
	structure_split(UseList, U, I, Small, Large),
	structure_isort(Small, I, SList),
	structure_isort(Large, I, LList),
	append(SList, [U|LList], SUseList).

structure_split([],       _U, _I, [],    []).
structure_split([V|UList], U,  I, Small, Large) :-
	arg(I, V, VKey),
	arg(I, U, UKey),
	(
	    VKey > UKey ->
	    (
		Large = [V|NLarge],
		Small = NSmall
	    )
	;
	    (
		Large = NLarge,
		Small = [V|NSmall]
	    )
	),
	structure_split(UList, U, I, NSmall, NLarge).
*/
%
%  Collect the relation size for the literals in a clause.
%  Only non-infinite relation literals are collected.
%

:- push_prolog_flag(multi_arity_warnings, off).

lit_relation_size(Clause, BT, ST, LitList) :-
	clause_type(Clause, Type),
	lit_relation_size(Type, Clause, BT, ST, LitList).

lit_relation_size(2, (_ :- Body), BT, ST, LitList) :-
	body_relation_size(Body, 1, BT, ST, LitList).
lit_relation_size(3, _, _, _, []).

body_relation_size(Lit, LitNum, BT, ST, LitList) :-
	nonsequence(Lit),
	!,
	functor(Lit, F, N),
	(
	    second_order_predicate(F/N) ->
	    second_order_predicate_pred_arg(Lit, NLit)
	;
	    NLit = Lit
	),
	functor(NLit, NF, NN),
	literal_property(BT, ST, NF/NN, relation, Rel),
	(
	    Rel == inf ->
	    LitList = []
	;
	    LitList = [lit(LitNum, Rel)]
	).
body_relation_size((Lit, Body), LitNum, BT, ST, LitList) :-
	functor(Lit, F, N),
	(
	    second_order_predicate(F/N) ->
	    second_order_predicate_pred_arg(Lit, NLit)
	;
	    NLit = Lit
	),
	functor(NLit, NF, NN),
	literal_property(BT, ST, NF/NN, relation, Rel),
	(
	    Rel == inf ->
	    LitList = LitLists
	;
	    LitList = [lit(LitNum, Rel)|LitLists]
	),
	LitNum1 is LitNum + 1,
	body_relation_size(Body, LitNum1, BT, ST, LitLists).

%
head_active_lit(_,                   [],      _Clause, _AList, []) :-
	!. % fact
head_active_lit([],                  LitList, _Clause,  ActiveList,
	    ActiveList) :-
	LitList \== [],
	!.
head_active_lit([use(_, [])|_],      LitList, _Clause, _AList, []) :-
	LitList \== [],
	!. % dangling var
head_active_lit([use(_, U)|UseList], LitList,  Clause,  AList, ActiveList) :-
	LitList \== [],
	U \== [],
	least_lit(U, LitList, Clause, _, inf, Lit),
	(
	    var(Lit) ->
	    ActiveList = [];  % all uses are in the head or infinite lit
	    (
		arg(1, Lit, LitNum),
		(
		    structure_member(AList, 1, LitNum) ->
		    AList1 = AList
		;		% vars appear in same lit 
		    AList1 = [Lit|AList]
		),
		head_active_lit(UseList, LitList, Clause, AList1, ActiveList)
	    )
	).

/*
head_active_lit([use(_, U)|UseList], LitList, AList, ActiveList) :-
	LitList \== [],
	U \== [],
	(
	    structure_list_member(U, 1, AList, 1) ->
	    head_active_lit(UseList, LitList, AList, ActiveList);
	    (
		least_lit(U, LitList, _, inf, Lit),
		(
		    var(Lit) ->
		    ActiveList = []
		;  % all uses are in the head 
		    (
			reduce_use_list(UseList, Lit, UseLists),
			ALists = [Lit|AList],
			head_active_lit(UseLists, LitList, ALists, ActiveList)
		    )
		)
	    )
	).

%
structure_list_member([S|SList], I, Structure, J) :-
	arg(I, S, Arg),
	(
	    structure_member(Structure, J, Arg) ->
	    true
	;
	    structure_list_member(SList, I, Structure, J)
	).
*/

structure_member([S|_SList], I, M) :-
	arg(I, S, Arg),
	Arg == M,
	!.
structure_member([S| SList], I, M) :-
	arg(I, S, Arg),
	Arg \== M,
	structure_member(SList, I, M).

%
least_lit([],       _LitList, _Clause, Lit, _Rel,  Lit).
least_lit([U|UList], LitList,  Clause, Lit,  Rel, MLit) :-
	pos_litnum(U, LitNum),
	(
	    LitNum > 0 ->
	    (
		arg(2, Clause, Body),
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
		    least_lit(UList, LitList, Clause, NLit, NRel, MLit)
		;
		    least_lit(UList, LitList, Clause, Lit, Rel, MLit)
		)
	    )
	;
	    (/* LitNum == 0 or an infinite relation lit*/
	        minimum(Rel, inf, NRel),
		least_lit(UList, LitList, Clause, Lit, NRel, MLit)
	    )
	).

%
structure_find([L|_LitList], I, LitNum, L) :-
	arg(I, L, Arg),
	Arg == LitNum,
	!.
structure_find([L| LitList], I, LitNum, Lit) :-
	arg(I, L, Arg),
	Arg \== LitNum,
	structure_find(LitList, I, LitNum, Lit).
/*
%
reduce_use_list(UseList, lit(LitNum, _), UseLists) :-
	reduce_use_list1(UseList, LitNum, UseLists).

reduce_use_list1([],         _Lit, []).
reduce_use_list1([U|UseList], Lit, NUseList) :-
	reduce_use_list1(UseList, Lit, UseLists),
	arg(2, U, Us),
	(
	    structure_member(Us, 1, Lit) ->
	    NUseList = UseLists
	;
	    NUseList = [U|UseLists]
	).
*/
%
%  Compute the number of input tuples for the head by multiplying the
%  relation sizes of the active literals.
%
head_size([], _I, inf) :-
	!.
head_size(S,   I, Value) :-
	S \== [],
	structure_multiply(S, I, Value).

structure_multiply([],           _I, 1).
structure_multiply([S|Structure], I, Value) :-
	arg(I, S, Val),
	structure_multiply(Structure, I, Values),
	multiply(Val, Values, Value).

%
%  Compute the number of bindings of a set of variables.
%
find_vars_binding(Vars, _Gvars, _Type, TBinding, TBinding) :-
	var(Vars),
	!.
find_vars_binding(Vars,  Gvars,  Type, TBinding, FBinding) :-
	nonvar(Vars),
	Vars = [Var|VList],
	find_gvars_field(Gvars, Var, Type, VBinding),
	multiply(TBinding, VBinding, TBinding1),
	find_vars_binding(VList, Gvars, Type, TBinding1, FBinding).

%
%  Compute the number of bindings for a set of variables.
%
insert_vars_binding(Vars, _BT, _ST, _Clause, _Gvars, _Type, _Default) :-
	var(Vars),
	!.
insert_vars_binding(Vars,  BT,  ST,  Clause,  Gvars,  Type,  Default) :-
	nonvar(Vars),
	Vars = [Var|VList],
	insert_var_binding(Var, BT, ST, Clause, Gvars, Type, Default),
	insert_vars_binding(VList, BT, ST, Clause, Gvars, Type, Default).
/*
	(Default == Binding ->
		Mvars = [Var|MList];
		Mvars = MList),
*/

%
%  Insert the number of bindings of a variables into ground variable list.
%
insert_var_binding(Var, BT, ST, Clause, Gvars, Type, Default) :-
	find_gvars_field(Gvars, Var, use, Pos),
	min_use_binding(Pos, BT, ST, Clause, Binding1),
	minimum(Binding1, Default, Binding),
	simplification(Binding, SBinding),
	insert_gvars_field(Gvars, Var, Type, SBinding).

%
%  Compute the minimum number of bindings of a variable among its use 
%  positions.
%
min_use_binding([],         _BT, _ST, _Clause, inf).
min_use_binding([Pos|PList], BT,  ST,  Clause, Binding) :-
	pos_litnum(Pos, I),
	min_use_binding_litnum(I, PList, BT, ST, Clause, Binding).

min_use_binding_litnum(0, PList, BT, ST, Clause, Binding) :-
	!,
	min_use_binding(PList, BT, ST, Clause, Binding).
min_use_binding_litnum(I, PList, BT,  ST,  Clause, Binding) :-
	I > 0,
	arg(2, Clause, Body),
	number_of_literals(Body, 1, Num),
	(
	    I > Num ->
	    NI is I-Num
	;
	    NI = I
	),
	ith_clause_literal(NI, Clause, NLit),
	(
	    I > Num ->
	    second_order_predicate_pred_arg(NLit, Lit)
	;
	    Lit = NLit
	),
	functor(Lit, F, N),
	literal_property(BT, ST, F/N, relation, Rel),
	min_use_binding(PList, BT, ST, Clause, Binding1),
	minimum(Rel, Binding1, Binding).

%
%  Find the set of variables occurring at a set of positions.
%
pos_var([],         _Lit, _Vars).
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
	term_var(N, Term, Vars).

term_var(0, _Term, _Vars) :-
	!.
term_var(N,  Term,  Vars) :-
	N > 0,
	arg(N, Term, Arg),
	term_var(Arg, Vars),
	N1 is N-1,
	term_var(N1, Term, Vars).

:- pop_prolog_flag(multi_arity_warnings).
/*
%
%  Insert a set of variables into a list.
%
insert_vars_list([],         _List).
insert_vars_list([Var|VList], List) :-
	insert_var_list(List, Var),
	insert_vars_list(VList, List).

%
%  Insert a variable into a list.
%
insert_var_list(List,      Var) :-
	var(List),
	!,
	List = [Var|_].
insert_var_list([V|_List], Var) :-
	V == Var,
	!.
insert_var_list([V| List], Var) :-
	V \== Var,
	insert_var_list(List, Var).

%
%  Establish the relations of binding coverage.
%
insert_binding_coverage(Vars, Gvars, Adg, Ldg, LitName, LitNum, Clause,
	    Type) :-
	gen_literal_iopos(Adg, LitName, LitNum, (+), Pos),
	pos_var(Pos, Clause, IVars),
	extend_saturated_var(IVars, Gvars, Ldg, Type, CVars),
	insert_covered_vars(Vars, Gvars, CVars, Type).

%
%  Extand the binding coverage by considering saturated variables sets.
%
extend_saturated_var(Vars, _Gvars, _Ldg, _Type, _Cvars) :-
	var(Vars),
	!.
extend_saturated_var(Vars,  Gvars,  Ldg,  Type,  Cvars) :-
	nonvar(Vars),
	Vars = [Var|VList],
	var_def_lit(Gvars, Var, LitNum),
	(
	    LitNum == 0 ->
	    new_lit(+, Lit)
	;
	    new_lit(LitNum, Lit)
	),
	(
	    Type == relation ->
	    find_ldg_field(Ldg, Lit, redge, Svars)
	;
	    find_ldg_field(Ldg, Lit, sedge, Svars)
	),
	opened_set_union(Svars, Cvars),
	extend_saturated_var(VList, Gvars, Ldg, Type, Cvars).
*/
%
%  Find the defining literal number of a variable.
%
var_def_lit(Gvars, Var, LitNum) :-
	find_gvars_field(Gvars, Var, def, Def),
	Def = [Pos|_],
	pos_litnum(Pos, LitNum).

/*
%
%  Insert the binding coverages on the set of output variables.
%
insert_covered_vars([], _, _, _).
insert_covered_vars([Var|VList], Gvars, CVars, Type) :-
	insert_coverage_edge(Gvars, Var, Type, CVars),
	insert_covered_vars(VList, Gvars, CVars, Type).

%
%  Insert the binding coverages on an output variables.
%
insert_coverage_edge(Gvars, Var, Type, CVars) :-
	insert_coverage_edge_(Type, Gvars, Var, CVars).

insert_coverage_edge_(relation, Gvars, Var, CVars) :-
	insert_gvars_field(Gvars, Var, redge, CVars).
insert_coverage_edge_(det,      Gvars, Var, CVars) :-
	insert_gvars_field(Gvars, Var, sedge, CVars).
*/

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
msort([], []).
msort([X], [X]).
msort(LL, R) :- LL = [_, _|_],
	divide(LL, L1, L2),
	msort(L1, R1),  msort(L2, R2),
	merge(R1, R2, R).

%
%  Divide a list into two lists.
%
divide([], [], []).
divide([X], [X], []).
divide([X1, X2|L], [X1|L1], [X2|L2]) :- divide(L, L1, L2).

%
%  Merge two lists into a list in decreasing order.
%
merge([], [], []).
merge(L, [], L) :- L = [_|_].
merge([], L, L) :- L = [_|_].
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
vars_binding([],           _Gvars, _Ldg, _Type, _Clause, 1).
vars_binding([HdLit|TlLit], Gvars,  Ldg,  Type,  Clause, Binding) :-
	HdLit = cv(_, HdVars),
	var(HdVars),
	!,
	vars_binding(TlLit, Gvars, Ldg, Type, Clause, Binding).
vars_binding([HdLit|TlLit], Gvars,  Ldg,  Type,  Clause, Binding) :-
	HdLit = cv(LitNum, HdVars),
	nonvar(HdVars),
	(
	    LitNum =:= 0 ->
	    new_lit(+, Lit)
	;
	    new_lit(LitNum, Lit)
	),
	optimize_var_lits(LitNum, HdVars, TlLit, Ldg, Type, Clause, NTlLit),
	%remove_covered_lits(HdVars, TlLit, Gvars, Type, NTlLit),
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
	%write(Binding1), nl,
	%write(Binding2), nl,
	minimum(Binding1, Binding2, Binding3),
	%write(Binding3), nl,
	vars_binding(NTlLit, Gvars, Ldg, Type, Clause, Binding4),
	multiply(Binding3, Binding4, Binding).

/*
%
%  Remove the set of covered variables from the tail of the variables
%  defining literals list.
%
remove_covered_lits(HdVars, TlLit,                _Gvars, _Type,
	    TlLit) :-
	var(HdVars),
	!.
remove_covered_lits(HdVars, [],                   _Gvars, _Type,
	    []) :-
	nonvar(HdVars),
	!.
remove_covered_lits(HdVars, [cv(LNum, Lit)|LList], Gvars,  Type,
	    [cv(LNum, NLit)|NList]) :-
	nonvar(HdVars),
	remove_covered_lit(HdVars, Lit, Gvars, Type, NLit),
	remove_covered_lits(HdVars, LList, Gvars, Type, NList).

%
%  Remove the set of covered variables from a literal in the variables
%  defining literals list.
%
remove_covered_lit(_HdVars, Lit, _Gvars, _Type, Lit) :-
	var(Lit),
	!.
remove_covered_lit( HdVars, Lit,  Gvars,  Type, NLit) :-
	nonvar(Lit),
	Lit = [Var|VList],
	(
	    covering_vars(Gvars, HdVars, Var, Type) ->
	    NLit = NVList
	;
	    NLit = [Var|NVList]
	),
	remove_covered_lit(HdVars, VList, Gvars, Type, NVList).

%
%  Test if a set of variables is covering a variable in binding coverage.
%
covering_vars(_Gvars, HdVars, _Var, _Type) :-
	var(HdVars),
	!,
	fail.
covering_vars( Gvars, HdVars,  Var,  Type) :-
	nonvar(HdVars),
	HdVars = [Hvar|HList],
	(
	    covering_var(Gvars, Hvar, Var, Type) ->
	    true
	;
	    covering_vars(Gvars, HList, Var, Type)
	).

%
%  Test if a variable is covering another variable in binding coverage.
%
covering_var(Gvars, Hvar, Var, Type) :-
	find_gvars_field(Gvars, Hvar, Type, Edges),
	init_queue(Qhd, Qtl),
	set_put_queue(Qtl, Edges, Ntl),
	reachable_var(Gvars, Qhd, Ntl, Var, Type).

%
%  Test if a variable is reachable from a queue of variables in binding 
%  coverage.
%
reachable_var(_Gvars, Qhd, Qtl, _Var, _Type) :-
	empty_queue(Qhd, Qtl),
	!,
	fail.
reachable_var( Gvars, Qhd, Qtl,  Var,  Type) :-
	nonempty_queue(Qhd, Qtl),
	get_queue(Qhd, V, Nhd),
	(
	    V == Var ->
	    true
	;
	    (
		find_gvars_field(Gvars, V, Type, Edges),
		set_put_queue(Qtl, Edges, Ntl),
		reachable_var(Gvars, Nhd, Ntl, Var, Type)
	    )
	).
*/

%
opt_cond1([cv(LitNum, _)|Lvars], Clause, IVars, Subsume) :-
	ith_clause_literal(LitNum, Clause, Lit),
	term_var(Lit, LitVars),
	(
	    opened_set_inclusion(IVars, LitVars) ->
	    new_lit(LitNum, Subsume)
	;
	    opt_cond1(Lvars, Clause, IVars, Subsume)
	).

opt_cond2(Lit, Ldg, Instance, Type) :-
	find_ldg_field(Ldg, Lit, Type, OBinding),
	Instance == OBinding.

opt_cond3(Instance, IBinding) :- Instance == IBinding.

opt_cond4(Instance, OBinding) :- Instance == OBinding.

optimize_var_lits(_HdLit, HdVars, _TlLit, _Ldg, _Type, _Clause, _NTlLit) :-
	var(HdVars),
	!.
optimize_var_lits( HdLit, HdVars,  TlLit,  Ldg,  Type,  Clause,  NTlLit) :-
	nonvar(HdVars),
	new_lit(HdLit, Lit),
	find_ldg_field(Ldg, Lit, Type, SubsumeLits),
	remove_subsumed_lits(TlLit, SubsumeLits, HdVars, TTlLit),
	find_ldg_field(Ldg, Lit, pred, PredLits),
	ith_clause_literal(HdLit, Clause, Literal),
	term_var(Literal, LitVars),
	merge_same_lit_vars(TTlLit, PredLits, LitVars, HdVars, NTlLit).

remove_subsumed_lits([],                         _SubsumeLits, _HdVars,
	    []).
remove_subsumed_lits([cv(LitNum, LitVars)|TlLit], SubsumeLits,  HdVars,
	    NTlLit) :-
	new_lit(LitNum, Lit),
	(
	    opened_set_member(SubsumeLits, Lit) ->
	    (
		merge_subsumed_vars(LitVars, HdVars),
		NTlLit = NTlLits
	    )
	;
	    NTlLit = [cv(LitNum, LitVars)|NTlLits]
	),
	remove_subsumed_lits(TlLit, SubsumeLits, HdVars, NTlLits).

merge_subsumed_vars(LitVars, _HdVars) :-
	var(LitVars),
	!.
merge_subsumed_vars(LitVars,  HdVars) :-
	nonvar(LitVars),
	LitVars = [V|Vars],
	opened_set_insertion(HdVars, V),
	merge_subsumed_vars(Vars, HdVars).

merge_same_lit_vars([],                         _PredLits, _CVars, _HdVars,
	    []).
merge_same_lit_vars([cv(LitNum, LitVars)|TlLit], PredLits,  CVars,  HdVars,
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

same_lit_vars(LitVars, _CVars, _HdVars, _NLitVars) :-
	var(LitVars),
	!.
same_lit_vars(LitVars,  CVars,  HdVars,  NLitVars) :-
	nonvar(LitVars),
	LitVars = [V|Vars],
	(
	    opened_set_member(CVars, V) ->
	    opened_set_insertion(HdVars, V)
	;
	    opened_set_insertion(NLitVars, V)
	),
	same_lit_vars(Vars, CVars, HdVars, NLitVars).

%
filter_effect([],                       _Vars, Binding,  Binding).
filter_effect([test(Lit, Rel)|TestLits], Vars, Binding1, Binding) :-
	term_var(Lit, Var1), 		% Lit is test literal
	(
	    opened_set_inclusion(Vars, Var1) ->
	    minimum(Rel, Binding1, Binding2)
	;
	    Binding2 = Binding1
	),
	filter_effect(TestLits, Vars, Binding2, Binding).

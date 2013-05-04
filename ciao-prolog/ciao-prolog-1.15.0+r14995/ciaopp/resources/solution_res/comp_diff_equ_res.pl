:- module(comp_diff_equ_res,
	    [
		solve_complexity_equ/7,
		solve_time_complexity_equ/7, % EMM
		solve_one_index_comp_diff_equ/11
	    ], [assertions, resources(inferres_decl)]).

:- doc(author, "Nai-Wei Lin").

:- doc(module, "This file contains the programs for solving the
	complexity difference equations.").

:- use_module(library(hiordlib),                        [map/3]).
:- use_module(library(llists),                          [transpose/2]).
:- use_module(ciaopp(preprocess_flags),                 [current_pp_flag/2]).
:- use_module(resources(algebraic_res(algebraic__res)), [add_expr/3]).
:- use_module(resources(init_res(symtable_res)),        [find_symbol_field/4]).
:- use_module(resources(top_res(utility_res)),
	    [
		ith_list_element/3,
		member/2,
		minimum/3
	    ]).
:- use_module(resources(size_res(normalize__res)),        [substitute/4]).
:- use_module(resources(algebraic_res(general_form_res)), [general_form/2]).
:- use_module(resources(algebraic_res(normal_form_res)),  [normal_form/2]).
:- use_module(resources(algebraic_res(simpl_form_res)),   [simplification/2]).
:- use_module(resources(size_res(size_diff_equ_res)),
	    [
		input_sizes/3,
		construct_comp_equs/4,
		classify_equs/3,
		abnormal_equs/2,
		indirect_recursion/2,
		indirect_recursion_expr/2,
		solve_comp_non_diff_equs/3,
		exprs_one_index_reducible/6,
		solve_two_indices_diff_equs/5,
		boundary_condition_one_index_reducible/4,
		boundary_condition_two_indices_reducible/4,
		replace_values_in_equs/4,
		exprs_two_indices_reducible/5,
		adjust_pos/2,
		reduce_two_indices_exprs/6,
		boundary_conds/2,
		min_unifiable_term_size/6,
		real_pos/4,
		min_unifiable_term_size1/5
	    ]).
:- use_module(resources(size_res(size__res)), [up_low_approximation_expr/4]).
:- use_module(resources(diff_equation_res(diff_equ_res))).
:- use_module(resources(diff_equation_res(product_res))).
:- use_module(resources(resources_basic)).

%
%  Solve the complexity equation for a predicate.
%  Size is the size functions for the clauses of the predicate;
%  while SIZE is the computed size functions for the predicate.
%
% Commented out by PLG (25-Mar-97)
%% solve_complexity_equ(Pred,ST,_,Comp,Size,Sol) :-
%% 	find_symbol_field(ST,Pred,size,SIZE),
%% 	find_symbol_field(ST,Pred,(mode),Mode),
%% 	(inconsistent_sizes(Mode,SIZE) ->
%% 		Sol1 = inf;	% size functions for the pred undefined 
%% 		(construct_clause_complexity_equs(Pred,ST,Size,Comp,ClauseEqu),
%% 		 (mutual_exclusive_complexity(Pred,ST,ClauseEqu,ClassEqu) ->
%% 			solve_comp_equs(ST,Pred,ClassEqu,Sol1);
%% 			Sol1 = inf))),	% sizes in cluster inconsistent 
%% 	simplification(Sol1,Sol).

% Added by EMM

:- comp solve_time_complexity_equ/7 + not_fails.
solve_time_complexity_equ(Complexity, Pred, Bound, ST, Comp, Size, Sol) :-
	solve_resource_complexity_equ(Complexity, Pred, Bound, ST, Comp,
	    Size, Sol).

solve_resource_complexity_equ(Complexities, Pred, Bound, ST, Comp,
	    Size, Sol) :-
	transpose(Complexities, ComplexitiesTrans),
	solve_resource_complexity_equ_trans(ComplexitiesTrans, Pred, Bound, ST,
	    Comp, Size, Sol).

solve_resource_complexity_equ_trans([], _Pred, _Bound, _ST, _Comp, _Size, []).
solve_resource_complexity_equ_trans([Complexity|Complexities], Pred, Bound,
	    ST, Comp, Size, [Sol|Sols]) :-
	solve_complexity_equ(Pred, Bound, ST, Comp, Complexity, Size, Sol),
	solve_resource_complexity_equ_trans(Complexities, Pred, Bound, ST,
	    Comp, Size, Sols).
% End Added by EMM

% Added by PLG (25-Mar-97)

:- comp solve_complexity_equ/7 + not_fails. % rtcheck -- EMM

solve_complexity_equ(Pred, Bound, ST, _, Complexity, Size, Sol) :-
	find_symbol_field(ST, Pred, size,   SIZE),
	find_symbol_field(ST, Pred, (mode), Mode),
	(
	    inconsistent_sizes(Mode, SIZE) ->
	    Sol1 = inf
	;
	    up_low_construct_clause_complexity_equs(Pred, Bound, ST, Size,
		Complexity, Sol1)
	),
	simplification(Sol1, Sol).

%% up_low_construct_clause_complexity_equs(Pred,ST,Size,Complexity,Sol1)
%% 
%% Pred: predicate/arity.
%% ST: symbol table.
%% Size: size relations corresponding to clauses. A list per
%% predicate, each item is another list (one per clause).  
%%       Eg. [[0,0],[$(0,1),q(2,2,$(0,1)-1)+1]]
%% q(2,2,$(0,1)-1) means the size of the second (output) argument of
%% q/2 for an input of size $(0,1)-1.
%% The first argument is the arity, the second is the output position
%% to which the size refers to. This syntax is interpreted slightly
%% different in the context of time-(complexity) analysis.
%% Complexity: Eg. [1,q(2,1,$(0,1)-1)+1]
%% q(2,1,$(0,1)-1) means the cost (time) of the predicate q/2 for an
%% input of size $(0,1)-1. The first argument is the predicate arity, the second
%% is irrelevant, and is always 1.
%% Sol1: solution $(0,1)+1.
%% 
%% The previous data correspond to the following predicate.
%% 
%% :- entry(p(X,Y), [ground(X),list(X),var(Y)]).
%% 
%% p([],[]).
%% p([X|Y], [X1|Y1]):- X1 is X + 1, p(Y, Y1). 
%% p([X|Y], [X1|Y1]):- X1 is X + 7, q(Y, Y1). 
%% 
%% q([],[]). 
%% q([X|Y], [X1|Y1]):- X1 is X + 2, q(Y, Y1).


up_low_construct_clause_complexity_equs(Pred, Bound, ST, Size, Complexity,
	    Sol1) :-
	construct_clause_complexity_equs(Pred, ST, Size, Complexity,
	    ClauseEqu),
	(
	    Bound == lower ->
	    find_symbol_field(ST, Pred, measure, _Measure),
	    solve_comp_equs(ST, Pred, ClauseEqu, Bound, Sol1)
	;
	    Bound == upper,
	    (
		mutual_exclusive_complexity(Pred, ST, ClauseEqu, ClassEqu)
	    -> solve_comp_equs(ST, Pred, ClassEqu, Bound, Sol1) ;
		Sol1 = inf % sizes in cluster inconsistent 
	    )
	).
% End added
%
%  Test if the sizes are inconsistent.
%
inconsistent_sizes(Mode, SIZE) :-
	inconsistent_sizes(Mode, SIZE, 1).

:- push_prolog_flag(multi_arity_warnings, off).

inconsistent_sizes([],       [],         0).
inconsistent_sizes([+|Mode], [bot|Size], _) :-
	inconsistent_sizes(Mode, Size, 0).
inconsistent_sizes([+|_],    [S|_],    _) :- S \== bot, !, fail.
inconsistent_sizes([-|Mode], [_|Size], I) :- inconsistent_sizes(Mode,
	    Size, I).

:- pop_prolog_flag(multi_arity_warnings).

%
%  Construct the complexity equation for each clause from the input size 
%  and complexity function for the clause.
%
construct_clause_complexity_equs(Pred, ST, Size, Comp, Equ) :-
	find_symbol_field(ST, Pred, (mode), Mode),
	input_sizes(Size, Mode, ISize),
	construct_comp_equs(ISize, Comp, 1, Equ).

%
%  Compose the complexity equation for each mutually exclusive cluster
%  from the complexity equations of its clauses.
%
mutual_exclusive_complexity(_Pred, _ST, ClauseEqu, ClauseEqu) :-
	current_pp_flag(prog_lang, java), !.
mutual_exclusive_complexity(Pred, ST, ClauseEqu, ClassEqu) :-
	find_symbol_field(ST, Pred, mutex, Mutex),
	map(Mutex, mutual_exclusive_comp1(ClauseEqu), ClassEqu).

% Extra checks on the consistent input sizes are also performed.
% The form of the input size for all clauses are the same.
mutual_exclusive_comp1([], _, equ(_, _, Zero)) :-
	normal_form(0, Zero).
mutual_exclusive_comp1([ClauseNum|CList], ClauseComp, equ(ClauseNum, ISize,
		Comp)) :-
	ith_list_element(ClauseNum, ClauseComp, CompEqu1),
	CompEqu1 = equ(ClauseNum, ISize, Comp1),
	mutual_exclusive_comp1(CList, ClauseComp, CompEqu2),
	CompEqu2 = equ(_, ISize, Comp2),
	add_expr(Comp1, Comp2, Comp).

%
%  Solve the complexity equations corresponding to the mutually exclusive
%  clusters of a predicate.
%
solve_comp_equs(ST, Pred, Equ, Bound, Sol) :-
	classify_equs(Equ, DEqu, BEqu),
%write(DEqu),nl,
%write(BEqu),nl,
	solve_comp_equs1(ST, Pred, DEqu, BEqu, Bound, NSol),
	general_form(NSol, Sol).

solve_comp_equs1(ST, Pred, DE, BE, Bound, Sol) :-
	abnormal_equs(BE, S),
%write(S),nl,
	(
	    general_form(S, 0) ->
	    solve_comp_equs2(DE, BE, Bound, ST, Pred, Sol)
	;
	    bound_bottom(Bound, Sol0),
	    normal_form(Sol0, Sol)
	).
% Commented out by PLG
% Sol = inf).

solve_comp_equs2(DE, [], Bound, ST, Pred, Sol) :-
	DE \== [],
	!,
	(
	    indirect_recursion(DE, Pred) ->
	    solve_comp_non_diff_equs(Bound, DE, Sol)
	;
	    solve_comp_diff_equs(DE, [], Bound, ST, Pred, Sol)
	).
solve_comp_equs2([], BE, Bound, _ST, _Pred, Sol) :-
	BE \== [],
	!,
	solve_comp_non_diff_equs(Bound, BE, Sol).
solve_comp_equs2(DE, BE, Bound, ST, Pred, Sol) :-
	DE \== [],
	BE \== [],
	(
	    indirect_recursion(DE, Pred) ->
	    (
		solve_comp_non_diff_equs(Bound, DE, S1),
		solve_comp_non_diff_equs(Bound, BE, S2),
% Commented out by PLG (25-Mar-97) 
% max_expr(S1,S2,Sol)
% Added by PLG (25-Mar-97)
		up_low_approximation_expr(Bound, S1, S2, Sol)
	    )
	;
	    solve_comp_diff_equs(DE, BE, Bound, ST, Pred, Sol)
	).

%
solve_comp_diff_equs(DE, BE, Bound, ST, Pred, Sol) :-
	exprs_one_index_reducible(DE, Pred, 1, IS, RDE, Pos),
	(
	    BE == [] ->
	    implicit_boundary_condition(Bound, ST, Pred, Pos, RBE)
	; % implicit failure
	    boundary_condition_one_index_reducible(BE, IS, Pos, RBE)
	),
%write(RBE),nl,
% Added by PLG 
	replace_values_in_equs(RDE, RBE, Pred, NewRDE),
	solve_one_index_comp_diff_equs(NewRDE, RBE, Bound, ST, Pred, Pos, Sol),
	!.
% End added.
% Commented out by PLG:
% solve_one_index_comp_diff_equs(RDE,RBE,ST,Pred,Pos,Sol),!.
solve_comp_diff_equs(DE, BE, Bound, _, Pred, Sol) :-
	exprs_two_indices_reducible(DE, Pred, 1, IS, Pos),
	adjust_pos(Pos, NPos),
	reduce_two_indices_exprs(DE, Pred, 1, IS, NPos, NDE),
	NPos = [Pos1, Pos2],
	POS1 is Pos1 -2,
	POS2 is Pos2 -2,
	boundary_condition_two_indices_reducible(BE, IS, [POS1, POS2], NBE
	),
	solve_two_indices_diff_equs(NDE, NBE, Pred, Bound, Sol),
	!.
% Commented out PLG
% solve_comp_diff_equs(_,_,_,_,inf).
% Added by PLG. Modified by EMM
solve_comp_diff_equs(_, _, Bound, _, _, Sol) :-
	bound_bottom(Bound, Sol0),
	normal_form(Sol0, Sol).
%End added.

%
%
% Commented out by PLG (25-Mar-97)
%% solve_one_index_comp_diff_equs([],_,_,_,_,Zero) :- normal_form(0,Zero).
%% solve_one_index_comp_diff_equs([equ(_,Var,OC)|DE],BE,ST,Pred,Pos,Sol) :-
%% 	%diff_equ_type(OC,Var,Pred,An1,An2,Bn,Type),
%% 	%general_form(OC,R),
%% 	%write(R),nl,
%% 	%write(Var),nl,
%% 	(indirect_recursion_expr(OC,Pred) ->
%% 		Sol1 = OC;
%% 		solve_typed_diff_equ(comp,OC,BE,Var,ST,Pred,Pos,Sol1)),
%% 	%solve_one_index_comp_diff_equ(Type,BE,Var,An1,An2,Bn,ST,Pred,Pos,Sol1),
%% 	%general_form(Sol1,GSol),
%% 	%write(GSol),nl,
%% 	solve_one_index_comp_diff_equs(DE,BE,ST,Pred,Pos,Sol2),
%% 	max_expr(Sol1,Sol2,Sol).
% Added by PLG (25-Mar-97)

solve_one_index_comp_diff_equs([DEqu], BE, Bound, ST, Pred, Pos, Sol) :- !,
	solve_one_index_one_comp_diff_equation(DEqu, BE, Bound, ST, Pred, Pos,
	    Sol).
solve_one_index_comp_diff_equs([DEqu|DE], BE, Bound, ST, Pred, Pos, Sol) :-
	solve_one_index_one_comp_diff_equation(DEqu, BE, Bound, ST, Pred, Pos,
	    Sol1),
	solve_one_index_comp_diff_equs(DE, BE, Bound, ST, Pred, Pos, Sol2),
	up_low_approximation_expr(Bound, Sol1, Sol2, Sol).

solve_one_index_one_comp_diff_equation(equ(_, Var, OC), BE, Bound, ST, Pred,
	    Pos, Sol1) :-
	( indirect_recursion_expr(OC, Pred) ->
	    Sol1 = OC
	;
	    solve_typed_diff_equ_comp(OC, BE, Var, Bound, ST, Pred, Pos, Sol1)
	).

% PLG
% solve_typed_diff_equ_comp(+DE,+BE,+Var,+Bound,+ST,+Pred,+Pos,-Sol):
% Solve a difference equation DE with boundary equations BE.
% +DE: a normal form expression.
% +BE: a list of equation. An equation is of type equ(N, ISize, Expre),
% where N is a natural number that indicates the clause to which the 
% equation refers to, ISize is the input size (it can be a natural
% number or a variable, and Expre is a normal form expression.
% Var is the (input) variable to which the difference equation depends on.
% It corresponds to the argument number Pos.
% ST symbol table.
% Pred: predicate/arity to which the equation refers to.

solve_typed_diff_equ_comp(DE, BE, Var, Bound, ST, Pred, Pos, Sol) :-
% Added by PLG (14-May-97)
% for debugging PLG
% debug
% write('Comp diff. equ:'),
% general_form(DE, GDE), nl, write(GDE), nl,
% write('Comp bound. equ:'),
% write_equs_general_form(BE),
% write('Reducible to Var: '), write(Var), nl,         
% End added
	( product_diff_equ(DE, Pred, NDE) ->
	    diff_equ_type(NDE, Var, Pred, A1n, A2n, Bn, Dtype),
	    log_base_equs(BE, NBE),
	    solve_one_index_comp_diff_equ(Dtype, NBE, Var, A1n, A2n, Bn,
		Bound, ST, Pred, Pos, Sol1),
	    exp_solution(Sol1, Sol)
% Added by PLG (14-May-97)
% debug
% write('Comp diff. Prod. solution:'),
% general_form(Sol, GSol), nl, write(GSol), nl
% End added
	;
	    diff_equ_type(DE, Var, Pred, A1n, A2n, Bn, Dtype),
	    solve_one_index_comp_diff_equ(Dtype, BE, Var, A1n, A2n, Bn,
		Bound, ST, Pred, Pos, Sol)
% Added by PLG (14-May-97)
% debug
% write('Comp diff. solution:'),
% general_form(Sol, GSol), nl, write(GSol), nl
% End added
	).


% End added

solve_one_index_comp_diff_equ(second_order, BE, Var, An1, An2, Bn, _, _, _, _,
	    Sol) :-
	!,
	boundary_conds(BE, Ival),
	solve_diff_equ(second_order, Var, An1, An2, Bn, Ival, Sol).
solve_one_index_comp_diff_equ(Type, BE, Var, An1, An2, Bn, Bound, ST, Pred,
	    Pos, Sol) :-
	solve_one_index_comp_diff_equ1(BE, Type, Var, An1, An2, Bn, Bound, ST,
	    Pred, Pos, Sol).

% Commented out by PLG (25-Mar-97)
%% solve_one_index_comp_diff_equ1([],_,_,_,_,_,_,_,_,Zero) :- normal_form(0,Zero).
%% solve_one_index_comp_diff_equ1([E|BE],Type,Var,An1,An2,Bn,ST,Pred,Pos,Sol) :-
%% 	E = equ(_,Index,Val),
%% 	(integer(Index) ->
%% 		(boundary_conds([E],Ival),
%% 		 %write(Ival),nl,
%% 		 solve_diff_equ(Type,Var,An1,An2,Bn,Ival,Sol1));
%% 		(comp_boundary_cond(E,ST,Pred,Pos,Ival),
%% 		 %write(Ival),nl,
%% 		 solve_diff_equ(Type,Var,An1,An2,Bn,Ival,TS),
%% 		 %write(TS),nl,
%% 		 max_expr(TS,Val,Sol1))),
%% 	%general_form(Sol1,GSol),
%% 	%write(GSol),nl,
%% 	solve_one_index_comp_diff_equ1(BE,Type,Var,An1,An2,Bn,ST,Pred,Pos,Sol2),
%% 	max_expr(Sol1,Sol2,Sol).

% Added by PLG (25-Mar-97)

solve_one_index_comp_diff_equ1([E], Type, Var, An1, An2, Bn, Bound, ST,
	    Pred, Pos, Sol) :- !,
	solve_one_index_one_comp_diff_equation1(E, Type, Var, An1, An2, Bn,
	    Bound, ST, Pred, Pos, Sol).
solve_one_index_comp_diff_equ1([E|BE], Type, Var, An1, An2, Bn, Bound, ST,
	    Pred, Pos, Sol) :-
	solve_one_index_one_comp_diff_equation1(E, Type, Var, An1, An2, Bn,
	    Bound, ST, Pred, Pos, Sol1),
	solve_one_index_comp_diff_equ1(BE, Type, Var, An1, An2, Bn, Bound, ST,
	    Pred, Pos, Sol2),
	up_low_approximation_expr(Bound, Sol1, Sol2, Sol).

solve_one_index_one_comp_diff_equation1(E, Type, Var, An1, An2, Bn, Bound, ST,
	    Pred, Pos, Sol1) :-
	E = equ(_, Index, Val),
	( integer(Index) ->
	    boundary_conds([E], Ival),
	    solve_diff_equ(Type, Var, An1, An2, Bn, Ival, Sol1)
	;
	    comp_boundary_cond(E, Bound, ST, Pred, Pos, Ival),
	    solve_diff_equ(Type, Var, An1, An2, Bn, Ival, TS),
	    up_low_approximation_expr(Bound, TS, Val, Sol1)
	).


%
%  Compute the boundary condition for a complexity difference equation.
%
comp_boundary_cond(equ(Num, Var, Val), Bound, ST, Pred, Pos, [val(Iind,
		    Ival)]) :-
	min_unifiable_term_sizes(Bound, ST, Pred, Num, Pos, MinSize),
	( integer(MinSize) ->
	    normal_form(MinSize, Iind),
	    general_form(Val, Nval),
	    substitute(Nval, Var, MinSize, Sval),
	    normal_form(Sval, Ival)
	;
	    Iind = bot,
	    Ival = bot
	).

min_unifiable_term_sizes(Bound, ST, Pred, Num, Pos, MinSize) :-
	find_symbol_field(ST, Pred, mutex, Mutex),
	mutex_cluster(Mutex, Num, Cluster),
	( min_unifiable_term_sizes1(Cluster, Bound, ST, Pred, Pos, _, MS) ->
	    MinSize = MS
	; MinSize = bot
	).

mutex_cluster([],        _,   []).
mutex_cluster([M|Mutex], Num, Cluster) :-
	( utility_res:member(M, Num) ->
	    Cluster = M;
	    mutex_cluster(Mutex, Num, Cluster) ).

min_unifiable_term_sizes1([], _, _, _, _, MinSize, MinSize) :- integer(
	    MinSize).
min_unifiable_term_sizes1([Num|Cluster], Bound, ST, Pred, Pos, MinSize,
	    FMinSize) :-
	min_unifiable_term_size(Bound, ST, Pred, Num, Pos, MinSize1),
	( var(MinSize) ->
	    integer(MinSize1),
	    min_unifiable_term_sizes1(Cluster, Bound, ST, Pred, Pos,
		MinSize1, FMinSize)
	;
	    integer(MinSize1),
	    MinSize =:= MinSize1,
	    min_unifiable_term_sizes1(Cluster, Bound, ST, Pred, Pos,
		MinSize1, FMinSize)
	).

implicit_boundary_condition(Bound, ST, Pred, Pos, [equ(_, Size, Zero)]) :-
	find_symbol_field(ST, Pred, measure, Measure),
	find_symbol_field(ST, Pred, (mode),  Mode),
	real_pos(Mode, Pos, 1, RealPos),
	ith_list_element(RealPos, Measure, M),
	find_symbol_field(ST, Pred, clause, ClauseKeys),
	min_unifiable_implicit_term_size(ClauseKeys, Bound, M, RealPos,
	    MinSize),
%write(MinSize),nl,
	Size is MinSize -1,
	normal_form(0, Zero).

min_unifiable_implicit_term_size(ClauseKeys, _Bound, _, _, inf) :-
	var(ClauseKeys),
	!.
min_unifiable_implicit_term_size([ClauseKey|Cs], Bound, M, Pos, MinSize) :-
	clause_key(ClauseKey, ClausePPKey, _Key),
	min_unifiable_term_size1(M, Bound, ClausePPKey, Pos, MS1),
	min_unifiable_implicit_term_size(Cs, Bound, M, Pos, MS2),
% Warning: check this!
	minimum(MS1, MS2, MinSize).

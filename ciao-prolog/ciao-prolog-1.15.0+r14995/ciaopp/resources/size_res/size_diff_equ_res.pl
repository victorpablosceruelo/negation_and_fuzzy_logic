:- module(size_diff_equ_res,
	    [
		input_sizes/3,
		construct_comp_equs/4,
		classify_equs/3,
		abnormal_equs/2,
		indirect_recursion/2,
		indirect_recursion_expr/2,
		solve_comp_non_diff_equs/3,
		exprs_one_index_reducible/6,
		solve_one_index_size_diff_equ/11,
		solve_two_indices_diff_equs/5,
		solve_size_equs/6,
		boundary_condition_one_index_reducible/4,
		boundary_condition_two_indices_reducible/4,
		replace_values_in_equs/4,
		exprs_two_indices_reducible/5,
		adjust_pos/2,
		reduce_two_indices_exprs/6,
		boundary_conds/2,
		min_unifiable_term_size/6,
		real_pos/4,
		min_unifiable_term_size1/5,
		solve_output_dummy_sizes/3
	    ], [assertions, resources(inferres_decl)]).

%
%  size_diff_equ.pl		Nai-Wei Lin			February, 1992
%
%  This file contains the programs for solving the size difference equations.
%

:- use_module(library(lists), [length/2]).
:- use_module(resources(size_res(size__res)),
	    [
		up_low_approximation_expr/4,
		min_approximation/3,
		up_low_approximation_minmax/4,
		minimum_list/2
	    ]).
:- use_module(resources(init_res(symtable_res)),
	    [find_symbol_field/4]).
:- use_module(resources(algebraic_res(general_form_res)), [general_form/2]).
:- use_module(resources(algebraic_res(normal_form_res)),  [normal_form/2]).
:- use_module(resources(algebraic_res(simpl_form_res)),   [simplification/2]).
:- use_module(resources(algebraic_res(normal_form_basic_res)),
	    [userfunc/1, variable/1]).
:- use_module(resources(algebraic_res(maxmin_res)), [max_expr/3]).
:- use_module(resources(diff_equation_res(diff_equ_res))).
:- use_module(resources(diff_equation_res(product_res))).
:- use_module(resources(diff_equation_res(first_order_res)), [solve_fode/5]).
:- use_module(resources(top_res(utility_res)),
	    [
		ith_list_element/3,
		union/3,
		member/2,
		maximum/3,
		minimum/3,
		compound/1,
		add/3
	    ]).
:- use_module(resources(size_res(normalize__res)),       [substitute/4]).
:- use_module(resources(init_res(initsystem_basic_res)), [clause_type/2]).
:- use_module(resources(determinacy_res(mutual_exclusion_res)),
	    [comparison_op/1]).
:- use_module(resources(solution_res(relation_res)),
	    [
		recursive_predicate/3,
		recursive_clause/2
	    ]).
:- use_module(resources(resources_basic)).

%
%  Solve size equations.
%

:- test solve_size_equs(Pred, Bound, ST, Component, Size, Sol) :
	(
	    Pred = 'reverse:concat'/3,
	    Bound = upper,
	    ST = [st('reverse:reverse'/2,
		    ['reverse:reverse'([], []) :'reverse:reverse/2/1',
			( 'reverse:reverse'([_150|_151], _149)
			    :- 'reverse:reverse'(_151, _160),
			    'reverse:concat'(_160, [_150], _149) )
			:'reverse:reverse/2/2'|_82758
		    ],
		    [+, -],
		    [length, length],
		    _81728, _81729, _81730, _81731, _81732, _81733),
		st('reverse:concat'/3,
		    ['reverse:concat'([], _132, _132) :'reverse:concat/3/1',
			( 'reverse:concat'([_97|_98], _95, [_97|_100]
			    )
			    :- 'reverse:concat'(_98, _95, _100) )
			:'reverse:concat/3/2'|_84657
		    ],
		    [+, +, -],
		    [length, length, length],
		    [[1], [2]],
		    _82882, _82883, _82884, _82885, _82886)|_83113],
	    Component = ['reverse:concat'/3],
	    Size = [[0, $(0, 2), $(0, 2)], [$(0, 1), $(0, 2),
		    'reverse:concat'(3, 3, $(0, 1) -1, $(0, 2)) +1]
		|_147306]
	) =>
	(
	    Sol = [$(0, 1), $(0, 2), $(0, 2) + $(0, 1)]
	) # "Test equation solver for size.".

:- pred solve_size_equs/6 + not_fails.

solve_size_equs(_F/A, Bound, _ST, _Component, [], Sol) :-
	!,
	bound_bottom(Bound, Bot),
	bot_sizes(A, Bot, [], Sol).
solve_size_equs(Pred, Bound, ST, Component, Size, Sol) :-
	!,
	find_symbol_field(ST, Pred, (mode), Mode),
	input_sizes(Size, Mode, ISize),
	(
	    consistent_input_sizes(Pred, ST, Component, ISize, Isize) ->
	    (
		solve_size_complexity_equs(Pred, Bound, ST, Mode, Size, ISize,
		    Osize),
		combine_size(Mode, Isize, Osize, Sol))
	; % by JNL
	    ( recursive_predicate(Pred, ST, Component) ->
		dummy_size(Mode, 1, Sol)
	    ;
		dummy_size(Mode, 1, Bound, Size, Sol)
	    )
	).

bot_sizes(0, _,   Acc, Acc).
bot_sizes(N, Bot, Acc, Rs) :-
	N1 is N -1,
	NAcc = [Bot|Acc],
	bot_sizes(N1, Bot, NAcc, Rs).

%
%  Solve size complexity equations.
%
solve_size_complexity_equs(Pred, Bound, ST, Mode, Size, ISize, Sol) :-
	construct_size_comp_equs(Mode, 1, Size, ISize, Equs, OIndex),
	solve_size_comp_equs(Equs, Bound, ST, Pred, OIndex, Sol).

%
%  Construct the complexity equations for each output position and
%  a list of indices for output positions.
%
construct_size_comp_equs([],       _, _,    _,     [],   []).
construct_size_comp_equs([+|Mode], I, Size, ISize, Equs, OIndex) :-
	I1 is I+1,
	construct_size_comp_equs(Mode, I1, Size, ISize, Equs, OIndex).
construct_size_comp_equs([-|Mode], I, Size, ISize, [E|Equs], [I|OIndex]) :-
	output_sizes(Size, I, OSize),
	construct_comp_equs(ISize, OSize, 1, E),
	I1 is I+1,
	construct_size_comp_equs(Mode, I1, Size, ISize, Equs, OIndex).

%
%  Solve the set of size complexity equations.
%
solve_size_comp_equs([],       _Bound, _,  _,    [],         []).
solve_size_comp_equs([E|Equs], Bound,  ST, Pred, [I|OIndex], [S|Sols]) :-
	classify_equs(E, DE, BE),
	solve_size_comp_equs1(DE, BE, Bound, ST, Pred, I, NS),
	general_form(NS, S),
	solve_size_comp_equs(Equs, Bound, ST, Pred, OIndex, Sols).

solve_size_comp_equs1(DE, BE, Bound, ST, Pred, I, Sol) :-
	abnormal_equs(BE, S),
%write(S),nl,
	( general_form(S, 0) ->
	    solve_size_comp_equs2(DE, BE, Bound, ST, Pred, I, Sol) ;
% no abnormals
	    Sol = S ).


%
%  Test if any abnormal size equation exists.
%

abnormal_equs([],                Zero) :- normal_form(0, Zero).
abnormal_equs([equ(_, _, O)|BE], Sol) :-
	abnormal_equ(O, S1),
	abnormal_equs(BE, S2),
	max_expr(S1, S2, Sol).


abnormal_equ(bot, bot) :- !.
abnormal_equ(inf, inf) :- !.
abnormal_equ(_,   Zero) :- normal_form(0, Zero).

%
%  Solve the set of size complexity equations corresponding one output position.
%
solve_size_comp_equs2(DE, [], Bound, _, Pred, _, Sol) :- % no base cases
	DE \== [],
	!,
	(
	    indirect_recursion(DE, Pred) ->
	    solve_comp_non_diff_equs(Bound, DE, Sol)
	;
	    Sol = bot
	).
solve_size_comp_equs2([], BE, Bound, _, _, _, Sol) :-
% no difference equations
	BE \== [],
	!,
	solve_comp_non_diff_equs(Bound, BE, Sol).
solve_size_comp_equs2(DE, BE, Bound, ST, Pred, I, Sol) :-
% both diff equs and base cases
	DE \== [],
	BE \== [],
	(
	    indirect_recursion(DE, Pred) ->
	    (
		solve_comp_non_diff_equs(Bound, DE, S1),
		solve_comp_non_diff_equs(Bound, BE, S2),
% Added by PLG (22-Mar-97)
% Warning: check this!
		up_low_approximation_expr(Bound, S1, S2, Sol)
% End added
% Commented by PLG (22-Mar-97)
% max_expr(S1,S2,Sol)
	    )
	;
	    solve_size_comp_diff_equs(DE, BE, Bound, ST, Pred, I, Sol)
	).

% Added by PLG

replace_values_in_equs([], _, _, []) :-
	!.
replace_values_in_equs([equ(A, B, E)|Eqs], BE, Pred, [equ(A, B, RE)|NewEqs]) :-
	general_form(E, GE), !,
	replace_value_one_equ(GE, BE, Pred, NewE),
	normal_form(NewE, RE), !,
% nl, write(A), write(' '), write(B),
% write(' '), write(GE), nl,
	replace_values_in_equs(Eqs, BE, Pred, NewEqs).

%% Not used PLG
%% trans_equs_to_general_form([], []).
%% trans_equs_to_general_form([equ(A, B, E)|Eqs], [equ(A, B, GE)|NewEqs]):-
%%         general_form(E, GE), 
%%         trans_equs_to_general_form(Eqs, NewEqs).

replace_value_one_equ(DE, _, _, DE) :-
	number(DE), !.
replace_value_one_equ(DE, BE, Pred / _, RDE) :-
	nonvar(DE),
	functor(DE, Pred, A), !,
	(
	    replaced_value(DE, BE, A, Val) ->
	    RDE = Val
	;
	    RDE = DE
	).
replace_value_one_equ(DE, BE, Pred, RDE) :-
	nonvar(DE),
	functor(DE,  F, A), A > 0, !,
	functor(RDE, F, A),
	comp_replace_value_one_equ(A, DE, BE, Pred, RDE).
replace_value_one_equ(DE, _, _, DE).

comp_replace_value_one_equ(0, _DE, _BE, _Pred, _RDE).
comp_replace_value_one_equ(N, DE,  BE,  Pred,  RDE) :-
	N > 0,
	arg(N, DE, Arg),
	replace_value_one_equ(Arg, BE, Pred, RArg),
	arg(N, RDE, RArg),
	N1 is N-1,
	comp_replace_value_one_equ(N1, DE, BE, Pred, RDE).

replaced_value(DE, BE, A, Val) :-
	A > 0,
	arg(1, DE, Arg),
	number(Arg),
	select_boundary_cond(BE, Arg, Val).

select_boundary_cond([equ(_, B, E)|_], Arg, Val) :-
	Arg == B, !, general_form(E, Val).
select_boundary_cond([_|Eqs], Arg, Val) :-
	select_boundary_cond(Eqs, Arg, Val).

% End added.
%
%

solve_size_comp_diff_equs(Equ, BEqus, Bound, ST, Pred, I, Sol) :-
	exprs_one_index_reducible(Equ, Pred, I, IS, ROC, Pos),
	boundary_condition_one_index_reducible(BEqus, IS, Pos, RBC),
% Added by PLG
	replace_values_in_equs(ROC, RBC, Pred, NewROC),
	solve_one_index_size_diff_equs(Bound, NewROC, RBC, ST, Pred, Pos, Sol),
	!.
% End added.
% Commented out by PL.
% solve_one_index_size_diff_equs(ROC,RBC,ST,Pred,Pos,Sol),!.
solve_size_comp_diff_equs(Equ, BEqus, Bound, _, Pred, I, Sol) :-
	exprs_two_indices_reducible(Equ, Pred, I, IS, Pos),
	adjust_pos(Pos, NPos),
	reduce_two_indices_exprs(Equ, Pred, I, IS, NPos, NEqu),
	NPos = [Pos1, Pos2],
	POS1 is Pos1-2,
	POS2 is Pos2-2,
	boundary_condition_two_indices_reducible(BEqus, IS, [POS1, POS2], BCs),
	solve_two_indices_diff_equs(NEqu, BCs, Pred, Bound, Sol),
	!.
% Commented out PLG
% solve_size_comp_diff_equs(_,_,_,_,_,inf).
% Added by PLG
solve_size_comp_diff_equs(_, _, Bound, _, _, _, Sol) :-
	bound_bottom(Bound, Sol0),
	normal_form(Sol0, Sol).

%End added.
%% Commented out by PLG (25-Mar-97)
%% solve_one_index_size_diff_equs([],_,_,_,_,Zero) :- normal_form(0,Zero).
%% solve_one_index_size_diff_equs([equ(_,Var,OC)|OCs],BEqus,ST,Pred,Pos,Sol) :-
%% 	%diff_equ_type(OC,Var,Pred,An1,An2,Bn,Type),
%% 	%general_form(OC,R),
%% 	%write(R),nl,
%% 	%write(Var),nl,
%% 	(indirect_recursion_expr(OC,Pred) ->
%% 		S = OC;
%% 		solve_typed_diff_equ(size,OC,BEqus,Var,ST,Pred,Pos,S)),
%% 	%general_form(S,GS),
%% 	%write(GS),nl,
%% 	%solve_one_index_size_diff_equ(Type,BEqus,Var,An1,An2,Bn,ST,Pred,Pos,S),
%% 	solve_one_index_size_diff_equs(OCs,BEqus,ST,Pred,Pos,Sols),
%%     max_expr(Sols,S,Sol).

% Added by PLG (25-Mar-97)
solve_one_index_size_diff_equs(lower, DEqus, BEqus, ST, Pred, Pos, Sol) :-
	lower_solve_one_index_size_diff_equs(DEqus, BEqus, lower, ST, Pred,
	    Pos, Sol).
solve_one_index_size_diff_equs(upper, DEqus, BEqus, ST, Pred, Pos, Sol) :-
	upper_solve_one_index_size_diff_equs(DEqus, BEqus, upper, ST, Pred,
	    Pos, Sol).

lower_solve_one_index_size_diff_equs([Equ], BEqus, Bound, ST, Pred, Pos,
	    Sol) :-
	!,
	lower_solve_one_index_size_diff_equation(Equ, BEqus, Bound, ST, Pred,
	    Pos, Sol).
lower_solve_one_index_size_diff_equs([Equ|OCs], BEqus, Bound, ST, Pred,
	    Pos, Sol) :-
	lower_solve_one_index_size_diff_equation(Equ, BEqus, Bound, ST, Pred,
	    Pos, S),
	lower_solve_one_index_size_diff_equs(OCs, BEqus, Bound, ST, Pred, Pos,
	    Sols),
	min_approximation(Sols, S, Sol).

lower_solve_one_index_size_diff_equation(Equ, BEqus, Bound, ST, Pred, Pos,
	    Sol) :-
	Equ = equ(_, Var, OC),
% general_form(OC,R),
% write(R),nl,
% write(Var),nl,
	(
	    indirect_recursion_expr(OC, Pred) ->
	    Sol = OC
	;
	    solve_typed_diff_equ_size(OC, BEqus, Var, Bound, ST, Pred, Pos,
		Sol)
	).

upper_solve_one_index_size_diff_equs([], _, _, _, _, _, Zero) :- normal_form(
	    0, Zero).
upper_solve_one_index_size_diff_equs([equ(_, Var, OC)|OCs], BEqus,
	    Bound, ST, Pred, Pos, Sol) :-
%diff_equ_type(OC,Var,Pred,An1,An2,Bn,Type),
%general_form(OC,R),
%write(R),nl,
%write(Var),nl,
	(
	    indirect_recursion_expr(OC, Pred) ->
	    S = OC
	;
	    solve_typed_diff_equ_size(OC, BEqus, Var, Bound, ST, Pred, Pos, S)
	),
%general_form(S,GS),
%write(GS),nl,
%solve_one_index_size_diff_equ(Type,BEqus,Var,An1,An2,Bn,ST,Pred,Pos,S),
	upper_solve_one_index_size_diff_equs(OCs, BEqus, Bound, ST, Pred, Pos,
	    Sols),
	max_expr(Sols, S, Sol).

% End added
% PLG
% solve_typed_diff_equ_size(+DE,+BE,+Var,+Bound,+ST,+Pred,+Pos,-Sol):
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

solve_typed_diff_equ_size(DE, BE, Var, Bound, ST, Pred, Pos, Sol) :-
% Added by PLG (25-Mar-97)
% for debugging PLG
% debug
% write('Size diff. equ:'),
% general_form(DE, GDE), nl, write(GDE), nl,
% write('Pos: '), write(Pos), nl, 
% write('Size bound. equ:'),
% write_equs_general_form(BE),
% debug
% write('Reducible to Var: '), write(Var), nl, 
% End added
	( product_diff_equ(DE, Pred, NDE) ->
	    ( diff_equ_type(NDE, Var, Pred, A1n, A2n, Bn, Dtype),
		log_base_equs(BE, NBE),
		solve_one_index_size_diff_equ(Dtype, NBE, Var, A1n, A2n, Bn,
		    Bound, ST, Pred, Pos, Sol1),
		exp_solution(Sol1, Sol)
% Added by PLG (14-May-97)
% debug
% write('Size diff. Prod. solution:'),
% general_form(Sol, GSol), nl, write(GSol), nl
% End added
	    ) ;
% Hook here the determination of a new type of
% difference equation
	    ( diff_equ_type(DE, Var, Pred, A1n, A2n, Bn, Dtype),
		solve_one_index_size_diff_equ(Dtype, BE, Var, A1n, A2n, Bn,
		    Bound, ST, Pred, Pos, Sol)
% Added by PLG (14-May-97)
% debug:
% write('Size diff. solution:'),
% general_form(Sol, GSol), nl, write(GSol), nl
% End added
	    ) ).

% End added

% Warning: I think the two following predicate is dead code PLG
% (25-Mar-97) %
solve_one_index_size_diff_equ(second_order, BE, Var, An1, An2, Bn, _, _, _, _,
	    Sol) :-
	!,
	boundary_conds(BE, Ival), %write(Ival),nl,
	solve_diff_equ(second_order, Var, An1, An2, Bn, Ival, Sol).
solve_one_index_size_diff_equ(Type, BE, Var, An1, An2, Bn, Bound, ST, Pred,
	    Pos, Sol) :-
	solve_one_index_size_diff_equ1(BE, Type, Var, An1, An2, Bn, Bound, ST,
	    Pred, Pos, Sol).


%% Commented out by PLG (25-Mar-97)
%% solve_one_index_size_diff_equ1([],_,_,_,_,_,_,_,_,Zero) :-
%% 	normal_form(0,Zero).
%% solve_one_index_size_diff_equ1([E|BE],Type,Var,An1,An2,Bn,ST,Pred,Pos,Sol) :-
%% 	E = equ(_,Index,Val),
%% 	(integer(Index) ->
%% 		(boundary_conds([E],Ival),
%% 		 %write(Ival),nl,
%% 		 solve_diff_equ(Type,Var,An1,An2,Bn,Ival,S));
%% 		(size_boundary_cond(E,ST,Pred,Pos,Ival),
%% 		 %write(Ival),nl,
%% 		 solve_diff_equ(Type,Var,An1,An2,Bn,Ival,TS),
%%                  % Commented by PLG (22-Mar-97)
%% 		 % max_expr(TS,Val,S)
%%                  % Added by PLG (22-Mar-97)
%%                  up_low_approximation_expr(TS,Val,S)
%%                  % End added
%%                  )),
%% 	%general_form(S,GS),
%% 	%write(GS),nl,
%% 	solve_one_index_size_diff_equ1(BE,Type,Var,An1,An2,Bn,ST,Pred,Pos,Sols),
%%         % Added by PLG (22-Mar-97)
%%         up_low_approximation_expr(Sols,S,Sol).
%%         % End added
%%         % Commented by PLG (22-Mar-97)
%%         % max_expr(Sols,S,Sol).

% Added by PLG (25-Mar-97)
%
solve_one_index_size_diff_equ1([E], Type, Var, An1, An2, Bn, Bound, ST,
	    Pred, Pos, Sol) :- !,
	solve_one_index_size_diff_equation1(E, Type, Var, An1, An2, Bn, Bound,
	    ST, Pred, Pos, Sol).
solve_one_index_size_diff_equ1([E|BE], Type, Var, An1, An2, Bn, Bound, ST,
	    Pred, Pos, Sol) :-
	solve_one_index_size_diff_equation1(E, Type, Var, An1, An2, Bn, Bound,
	    ST, Pred, Pos, S),
%general_form(S,GS),
%write(GS),nl,
	solve_one_index_size_diff_equ1(BE, Type, Var, An1, An2, Bn, Bound, ST,
	    Pred, Pos, Sols),
% Added by PLG (22-Mar-97)
	up_low_approximation_expr(Bound, Sols, S, Sol).
% End added
% Commented by PLG (22-Mar-97)
% max_expr(Sols,S,Sol).

%% solve_one_index_size_diff_equation1(+E,+Type,+Var,+An1,+An2,+Bn,+Bound,+ST,+Pred,+Pos,-Sol)
%% Solve the difference equation given by An1,+An2,+Bn with the
%% boundary condition given by E.

solve_one_index_size_diff_equation1(E, Type, Var, An1, An2, Bn, Bound, ST,
	    Pred, Pos, Sol) :-
	E = equ(_, Index, Val),
	( integer(Index) ->
	    ( boundary_conds([E], Ival),
%write(Ival),nl,
		solve_diff_equ(Type, Var, An1, An2, Bn, Ival, Sol) ) ;
	    ( size_boundary_cond(E, Bound, ST, Pred, Pos, Ival),
%write(Ival),nl,
		solve_diff_equ(Type, Var, An1, An2, Bn, Ival, TS),
% warning: check!
% Commented by PLG (22-Mar-97)
% max_expr(TS,Val,S)
% Added by PLG (22-Mar-97)
		up_low_approximation_expr(Bound, TS, Val, Sol) ) ).
% End added


%
%% % Commented out by PLG (25-Mar-97)
%% solve_two_indices_diff_equs([],_,_,Zero) :- normal_form(0,Zero).
%% solve_two_indices_diff_equs([equ(_,Vars,OC)|Equ],BCs,Pred,Sol) :-
%% 	solve_two_indices_diff_equ(OC,BCs,Vars,Pred,Sol1),
%% 	solve_two_indices_diff_equs(Equ,BCs,Pred,Sol2),
%%         % Added by PLG (22-Mar-97)
%%         up_low_approximation_expr(Sol1,Sol2,Sol).
%%         % End added
%%         % Commented by PLG (22-Mar-97)
%% 	% max_expr(Sol1,Sol2,Sol).

% Added by PLG (25-Mar-97)
solve_two_indices_diff_equs([equ(_, Vars, OC)], BCs, Pred, Bound, Sol) :-
	!,
	solve_two_indices_diff_equ(OC, BCs, Vars, Pred, Bound, Sol).
solve_two_indices_diff_equs([equ(_, Vars, OC)|Equ], BCs, Pred, Bound,
	    Sol) :-
	solve_two_indices_diff_equ(OC, BCs, Vars, Pred, Bound, Sol1),
	solve_two_indices_diff_equs(Equ, BCs, Pred, Bound, Sol2),
	up_low_approximation_expr(Bound, Sol1, Sol2, Sol).
% End added

solve_two_indices_diff_equ(Equ, BCs, Vars, Pred, Bound, Sol) :-
	two_indices_diff_equ_type(Equ, Vars, Pred, An, Bn, Type),
	solve_two_indices_diff_equ(Type, BCs, Vars, An, Bn, Bound, Sol), !.
solve_two_indices_diff_equ(_, _, _, _, _, inf).
%% Should not be 0 if Bound=lower ????? EMM

:- push_prolog_flag(multi_arity_warnings, off).

solve_two_indices_diff_equ(first_index, BCs, Vars, An, Bn, _Bound, Sol) :-
	two_indices_boundary_cond_type(BCs, Vars, _, Ivalue, _, BType),
	Vars = [Var1, _],
	( (BType == exclusive ; BType == both) ->
	    solve_fode(Var1, Ivalue, An, Bn, Sol) ;
	    Sol = inf ).
solve_two_indices_diff_equ(second_index, BCs, Vars, An, Bn, _Bound, Sol) :-
	two_indices_boundary_cond_type(BCs, Vars, _, _, Ivalue, BType),
	Vars = [_, Var2],
	( (BType == exclusive ; BType == both) ->
	    solve_fode(Var2, Ivalue, An, Bn, Sol) ;
	    Sol = inf ).
solve_two_indices_diff_equ(both_indices, BCs, Vars, An, Bn, Bound, Sol) :-
	two_indices_boundary_cond_type(BCs, Vars, Ivalue, Ivalue1, Ivalue2,
	    BType),
	Vars = [Var1, Var2],
	( BType == inclusive ->
	    solve_fode(Var1, Ivalue, An, Bn, Sol) ;
	    ( (BType == exclusive ; BType == both) ->
		( solve_fode(Var1, Ivalue1, An, Bn, Sol1),
		    solve_fode(Var2, Ivalue2, An, Bn, Sol2),
% Commented by PLG (22-Mar-97)
% max_expr(Sol1,Sol2,Sol)
% Added by PLG (22-Mar-97)
		    up_low_approximation_expr(Bound, Sol1, Sol2, Sol)
% End added
		) ;
		Sol = inf ) ).

two_indices_diff_equ_type(Equ, [Var1, Var2], F / _, An, Bn, Type) :-
	Equ = expr([term([DVar], An)], Bn),
	userfunc(DVar),
	functor(DVar, F, 2),
	arg(1, DVar, Arg1),
	arg(2, DVar, Arg2),
	two_indices_equ_type(Arg1, Arg2, Var1, Var2, Type).

two_indices_equ_type(Arg1, Arg2, Var1, Var2, first_index) :-
	normal_form(Var1-1, Arg1),
	normal_form(Var2,   Arg2).
two_indices_equ_type(Arg1, Arg2, Var1, Var2, second_index) :-
	normal_form(Var1,   Arg1),
	normal_form(Var2-1, Arg2).
two_indices_equ_type(Arg1, Arg2, Var1, Var2, both_indices) :-
	normal_form(Var1-1, Arg1),
	normal_form(Var2-1, Arg2).

two_indices_boundary_cond_type([equ(_, [0, 0], Expr)], _, [val(Zero, Expr)],
	    _, _, inclusive) :- normal_form(0, Zero).
two_indices_boundary_cond_type([equ(_, [0, Var2], Expr1),
		equ(_, [Var1, 0], Expr2)], [Var1, Var2], _, [val(Zero, Expr1)],
	    [val(Zero, Expr2)], exclusive) :-
	normal_form(0, Zero).
two_indices_boundary_cond_type([equ(_, [Var1, 0], Expr2),
		equ(_, [0, Var2], Expr1)], [Var1, Var2], _, [val(Zero, Expr1)],
	    [val(Zero, Expr2)], exclusive) :-
	normal_form(0, Zero).
two_indices_boundary_cond_type([equ(_, [0, 0], Expr),
		equ(_, [0, Var2], Expr1), equ(_, [Var1, 0], Expr2)],
	    [Var1, Var2], [val(Zero, Expr)], [val(Zero, Expr1)],
	    [val(Zero, Expr2)], both) :-
	normal_form(0, Zero).
two_indices_boundary_cond_type([equ(_, [0, 0], Expr),
		equ(_, [Var1, 0], Expr2), equ(_, [0, Var2], Expr1)],
	    [Var1, Var2], [val(Zero, Expr)], [val(Zero, Expr1)],
	    [val(Zero, Expr2)], both) :-
	normal_form(0, Zero).
two_indices_boundary_cond_type([equ(_, [0, Var2], Expr1),
		equ(_, [0, 0], Expr), equ(_, [Var1, 0], Expr2)],
	    [Var1, Var2], [val(Zero, Expr)], [val(Zero, Expr1)],
	    [val(Zero, Expr2)], both) :-
	normal_form(0, Zero).
two_indices_boundary_cond_type([equ(_, [0, Var2], Expr1),
		equ(_, [Var1, 0], Expr2), equ(_, [0, 0], Expr)],
	    [Var1, Var2], [val(Zero, Expr)], [val(Zero, Expr1)],
	    [val(Zero, Expr2)], both) :-
	normal_form(0, Zero).
two_indices_boundary_cond_type([equ(_, [Var1, 0], Expr2),
		equ(_, [0, 0], Expr), equ(_, [0, Var2], Expr1)],
	    [Var1, Var2], [val(Zero, Expr)], [val(Zero, Expr1)],
	    [val(Zero, Expr2)], both) :-
	normal_form(0, Zero).
two_indices_boundary_cond_type([equ(_, [Var1, 0], Expr2),
		equ(_, [0, Var2], Expr1), equ(_, [0, 0], Expr)],
	    [Var1, Var2], [val(Zero, Expr)],
	    [val(Zero, Expr1)], [val(Zero, Expr2)], both) :-
	normal_form(0, Zero).
two_indices_boundary_cond_type(_, _, _, _, _, no_match).

%
%  Reduce a difference equation to an one-index difference equation.
%
exprs_one_index_reducible([],                     _,    _, _,
	    [],                       _).
exprs_one_index_reducible([equ(Num, IS, OC)|Equ], Pred, I, IS,
	    [equ(Num, Var, ROC)|OCs], Pos) :-
	expr_one_index_reducible(OC, Pred, I, IS, ROC, Pos),
	ith_list_element(Pos, IS, Var),
	exprs_one_index_reducible(Equ, Pred, I, IS, OCs, Pos).

expr_one_index_reducible(expr(T, F), Pred, I, ISize, expr(NT, F), Pos) :-
	term_one_index_reducible(T, Pred, I, ISize, 0, NT, Pos1),
	Pos is Pos1-2.

term_one_index_reducible([], _, _, _, 0,    [], _) :- fail.
term_one_index_reducible([], _, _, _, Flag, [], _) :-
	Flag > 0.
term_one_index_reducible([term(P, F)|T], Pred, I, ISize, Flag,
	    [term(NP, F)|NT], Pos) :-
	primary_one_index_reducible(P, Pred, I, ISize, Flag1, NP, Pos),
	Flag2 is Flag1 + Flag,
	term_one_index_reducible(T, Pred, I, ISize, Flag2, NT, Pos).

primary_one_index_reducible([],     _,   _, _,     0,    [],     _).
primary_one_index_reducible([P|Ps], F/A, I, ISize, Flag, [R|Rs], Pos) :-
% 	direct recursion
	userfunc(P), functor(P, F, N), arg(1, P, Arity),
	normal_form(A, Arity),
	arg(2, P, Index),
	normal_form(I, Index),
	one_arg_diff(3, N, P, ISize, 0, RArg, Pos),
	functor(R, F, 1), arg(1, R, RArg),
	primary_one_index_reducible(Ps, F/A, I, ISize, Flag1, Rs, Pos),
	Flag is Flag1+1.
primary_one_index_reducible([P|Ps], F/A, I, ISize, Flag,
	    [P|Rs], Pos) :-
% 	indirect recursion
	userfunc(P), functor(P, F1, _),
	F1 \== F,
	primary_one_index_reducible(Ps, F/A, I, ISize, Flag, Rs, Pos).
primary_one_index_reducible([P|Ps], F/A, I, ISize, Flag,
	    [P|Rs], Pos) :-
% 	indirect recursion
	userfunc(P), functor(P, F, _), arg(1, P, Arity),
	general_form(Arity, GArity),
	GArity \== A,
	primary_one_index_reducible(Ps, F/A, I, ISize, Flag, Rs, Pos).
primary_one_index_reducible([P|Ps], F/A, I, ISize, Flag,
	    [R|Rs], Pos) :-
% 	direct recursion
	P = sum(Index, Lower, Upper, Expr),
	Expr = expr([term([P1], FS)], CS),
	userfunc(P1), functor(P1, F, N), arg(1, P1, Arity),
	normal_form(A, Arity),
	arg(2, P1, OPos),
	normal_form(I, OPos),
	one_arg_diff(3, N, P1, ISize, 0, RArg, Pos),
	functor(R1, F, 1), arg(1, R1, RArg),
	RExpr = expr([term([R1], FS)], CS),
	R = sum(Index, Lower, Upper, RExpr),
	primary_one_index_reducible(Ps, F/A, I, ISize, Flag1, Rs, Pos),
	Flag is Flag1+1.

%
%  Test if the number of different arguments is 1.
%
one_arg_diff(M, N, _, [],        1,     _,    _) :- M > N.
one_arg_diff(M, N, F, [S|ISize], Count, RArg, I) :- M =< N,
	arg(M, F, Arg), normal_form(S, Arg), !,
	M1 is M+1,
	one_arg_diff(M1, N, F, ISize, Count, RArg, I).
one_arg_diff(M, N, F, [S|ISize], 0, Arg, M) :- M =< N,
	arg(M, F, Arg), normal_form(S, S1),
	S1 \== Arg, variable(S),
	M1 is M+1,
	one_arg_diff(M1, N, F, ISize, 1, Arg, M).

%
%  Reduce a difference equation to an two-indices difference equation.
%
exprs_two_indices_reducible(Equ, Pred, I, IS, Pos) :-
	exprs_two_indices_reducible1(Equ, Pred, I, IS, Pos),
	length(Pos, Len), Len =:= 2.

exprs_two_indices_reducible1([],                   _,    _, _,  []).
exprs_two_indices_reducible1([equ(_, IS, OC)|Equ], Pred, I, IS, Pos) :-
	expr_two_indices_reducible(OC, Pred, I, IS, Pos1),
	exprs_two_indices_reducible1(Equ, Pred, I, IS, Pos2),
	union(Pos1, Pos2, Pos).

expr_two_indices_reducible(expr(T, _), Pred, I, ISize, Pos) :-
	term_two_indices_reducible(T, Pred, I, ISize, 0, Pos),
	length(Pos, Len), Len =< 2.

term_two_indices_reducible([], _, _, _, 0, _) :-
	fail.
term_two_indices_reducible([], _, _, _, Flag, []) :-
	Flag > 0.
term_two_indices_reducible([term(P, _)|T], Pred, I, ISize, Flag,
	    Pos) :-
	primary_two_indices_reducible(P, Pred, I, ISize, Flag1, Pos1),
	Flag2 is Flag1 + Flag,
	term_two_indices_reducible(T, Pred, I, ISize, Flag2, Pos2),
	union(Pos1, Pos2, Pos).

primary_two_indices_reducible([P], F/A, I, ISize, 1, Pos) :-
% 	direct recursion
	userfunc(P), functor(P, F, N), arg(1, P, Arity),
	normal_form(A, Arity), arg(2, P, Index),
	normal_form(I, Index),
	two_arg_diff(3, N, P, ISize, 0, Count, [], Pos), Count > 0.
primary_two_indices_reducible([P], F / _, _, _, 0, []) :-
% 	indirect recursion
	userfunc(P), functor(P, F1, _), F1 \== F.
primary_two_indices_reducible([P], F/A, _, _, 0, []) :-
% 	indirect recursion
	userfunc(P), functor(P, F, _), arg(1, P, Arity),
	general_form(Arity, GArity), GArity \== A.
primary_two_indices_reducible([P], F/A, I, ISize, 1, Pos) :-
% 	direct recursion
	P = sum(_, _, _, Expr),
	Expr = expr([term([P1], _)], _),
	userfunc(P1), functor(P1, F, N), arg(1, P1, Arity),
	normal_form(A, Arity), arg(2, P1, OPos),
	normal_form(I, OPos),
	two_arg_diff(3, N, P1, ISize, 0, Count, [], Pos), Count > 0.

%
%  Test if the number of different arguments is at most 2.
%
two_arg_diff(M, N, _, [], Count, Count, Pos, Pos) :- M > N,
	Count =< 2.
two_arg_diff(M, N, F, [S|ISize], Count, FCount, Pos, FPos) :- M =< N,
	arg(M, F, Arg), normal_form(S, Arg), !,
	M1 is M+1,
	two_arg_diff(M1, N, F, ISize, Count, FCount, Pos, FPos).
two_arg_diff(M, N, F, [S|ISize], 0, Count, Pos, FPos) :- M =< N,
	arg(M, F, Arg), normal_form(S, S1),
	S1 \== Arg, variable(S),
	M1 is M+1,
	two_arg_diff(M1, N, F, ISize, 1, Count, [M|Pos], FPos).
two_arg_diff(M, N, F, [S|ISize], 1, Count, Pos, FPos) :- M =< N,
	arg(M, F, Arg), normal_form(S, S1),
	S1 \== Arg, variable(S),
	M1 is M+1,
	two_arg_diff(M1, N, F, ISize, 2, Count, [M|Pos], FPos).

adjust_pos([Pos1, Pos2], [Pos1, Pos2]) :- Pos1 < Pos2, !.
adjust_pos([Pos1, Pos2], [Pos2, Pos1]).

reduce_two_indices_exprs(Equ, Pred, I, IS, Pos, NEqu) :-
	Pos = [Pos1, Pos2],
	POS1 is Pos1-2,
	POS2 is Pos2-2,
	ith_list_element(POS1, IS, Var1),
	ith_list_element(POS2, IS, Var2),
	reduce_two_indices_exprs(Equ, Pred, I, Var1, Var2, Pos, NEqu).

reduce_two_indices_exprs([],                    _,    _, _,    _,
	    _,   []).
reduce_two_indices_exprs([equ(Num, _, OC)|Equ], Pred, I, Var1, Var2,
	    Pos, [equ(Num, [Var1, Var2], ROC)|OCs]) :-
	reduce_two_indices_expr(OC, Pred, I, Pos, ROC),
	reduce_two_indices_exprs(Equ, Pred, I, Var1, Var2, Pos, OCs).

reduce_two_indices_expr(expr(T, F), Pred, I, Pos, expr(ROC, F)) :-
	reduce_two_indices_term(T, Pred, I, Pos, ROC).

:- pop_prolog_flag(multi_arity_warnings).

reduce_two_indices_term([],             _,    _, _,   []).
reduce_two_indices_term([term(P, F)|T], Pred, I, Pos, [term(NP, F)|NT]) :-
	reduce_two_indices_primary(P, Pred, I, Pos, NP),
	reduce_two_indices_term(T, Pred, I, Pos, NT).

reduce_two_indices_primary([P], F/A, I, Pos, [R]) :- % direct recursion
	userfunc(P), functor(P, F, _), arg(1, P, Arity),
	normal_form(A, Arity),
	arg(2, P, Index),
	normal_form(I, Index),
	Pos = [Pos1, Pos2], functor(R, F, 2),
	arg(Pos1, P, Arg1), arg(1, R, Arg1),
	arg(Pos2, P, Arg2), arg(2, R, Arg2).
reduce_two_indices_primary([P], F / _, _, _, [P]) :- % indirect recursion
	userfunc(P), functor(P, F1, _),
	F1 \== F.
reduce_two_indices_primary([P], F/A, _, _, [P]) :- % indirect recursion
	userfunc(P), functor(P, F, _), arg(1, P, Arity),
	general_form(Arity, GArity),
	GArity \== A.
reduce_two_indices_primary([P], F/A, I, Pos, [R]) :- % direct recursion
	P = sum(Index, Lower, Upper, Expr),
	Expr = expr([term([P1], FS)], CS),
	userfunc(P1), functor(P1, F, _), arg(1, P1, Arity),
	normal_form(A, Arity),
	arg(2, P1, OPos),
	normal_form(I, OPos),
	Pos = [Pos1, Pos2], functor(R, F, 2),
	arg(Pos1, P1, Arg1), arg(1, R1, Arg1),
	arg(Pos2, P1, Arg2), arg(2, R1, Arg2),
	RExpr = expr([term([R1], FS)], CS),
	R = sum(Index, Lower, Upper, RExpr).

%
%  Test if boundary conditions are one index reducible. If it is,
%  the reduced conditions are returned.
%

%% Comments added by PLG:
%% boundary_condition_one_index_reducible(+EquList,+IS,+Pos,-ReducedEquList)
%%    +EquList: list of boundary equations [equ(N,S,C)|Equ], where
%%       N is the equation number (clause number), 
%%       S list of actual input sizes of the clause,
%%       C actual size of the output argument, in the case we are computing
%%         sizes, or time complexity. It is in normal form.
%%    +IS: list of input sizes.
%%    +Pos: Position of the input argument to which the boundary
%%         condition is to be reduced to.
%%    -ReducedEquList: list of reduced boundary conditions, in which only
%%       the variable corresponding to the argument position Pos appears. 


boundary_condition_one_index_reducible([],                 _,  _,
	    []).
boundary_condition_one_index_reducible([equ(N, S, C)|Equ], IS, Pos,
	    [equ(N, I, C)|RBC]) :-
	bc_one_index_reducible(S, IS, Pos, 1),
	ith_list_element(Pos, S, I),
	boundary_condition_one_index_reducible(Equ, IS, Pos, RBC).

bc_one_index_reducible([],     [],     _,   _).
bc_one_index_reducible([S|S1], [S|S2], Pos, I) :-
	Pos \== I, I1 is I+1,
	bc_one_index_reducible(S1, S2, Pos, I1).
bc_one_index_reducible([_|S1], [_|S2], Pos, Pos) :-
	I1 is Pos+1,
	bc_one_index_reducible(S1, S2, Pos, I1).

boundary_condition_two_indices_reducible([],                 _,  _,
	    []).
boundary_condition_two_indices_reducible([equ(N, S, C)|Equ], IS, Pos,
	    [equ(N, [I1, I2], C)|RBC]) :-
	bc_two_indices_reducible(S, IS, Pos, 1),
	Pos = [Pos1, Pos2],
	ith_list_element(Pos1, S, I1),
	ith_list_element(Pos2, S, I2),
	boundary_condition_two_indices_reducible(Equ, IS, Pos, RBC).

bc_two_indices_reducible([],      [],      _,   _).
bc_two_indices_reducible([H1|S1], [H2|S2], Pos, I) :-
	( utility_res:member(Pos, I) -> true
	; H1 == H2
	),
	I1 is I+1,
	bc_two_indices_reducible(S1, S2, Pos, I1).

% Create a list with the boundary values (conditions).
% Question: is it assumed the NIndex is always an integer? PLG
boundary_conds([],                        []).
boundary_conds([equ(_, Index, Ival)|Equ], [val(NIndex, Ival)|Ivalue]) :-
	normal_form(Index, NIndex),
	boundary_conds(Equ, Ivalue).

%
%  Compute the boundary condition for a size difference equation.
%  Main task is to estimate the minimum unifiable input term size.
%
size_boundary_cond(equ(Num, Var, Val), Bound, ST, Pred, Pos,
	    [val(Iind, Ival)]) :-
	min_unifiable_term_size(Bound, ST, Pred, Num, Pos, MinSize),
	( integer(MinSize) ->
	    normal_form(MinSize, Iind),
	    general_form(Val, Nval),
	    substitute(Nval, Var, MinSize, Sval),
	    normal_form(Sval, Ival)
	;
	    (Iind = bot, Ival = bot)
	).

%
%  Find out the minimum unifiable size of a term in clause CNum at position Pos.
%
min_unifiable_term_size(Bound, ST, Pred, CNum, Pos, MinSize) :-
	find_symbol_field(ST, Pred, measure, Measure),
	find_symbol_field(ST, Pred, (mode),  Mode),
	real_pos(Mode, Pos, 1, RealPos),
	ith_list_element(RealPos, Measure, M),
	find_symbol_field(ST, Pred, clause, ClauseKeys),
	ith_list_element(CNum, ClauseKeys, ClauseKey),
	clause_key(ClauseKey, ClausePPKey, _Key),
	min_unifiable_term_size1(M, Bound, ClausePPKey, RealPos, MinSize).

%
real_pos([+|_],    1,            Pos, Pos).
real_pos([+|Mode], PrincipalPos, I,   Pos) :-
	PrincipalPos > 1,
	PrincipalPos1 is PrincipalPos-1,
	I1 is I+1,
	real_pos(Mode, PrincipalPos1, I1, Pos).
real_pos([-|Mode], PrincipalPos, I, Pos) :-
	I1 is I+1,
	real_pos(Mode, PrincipalPos, I1, Pos).

%
:- pred min_unifiable_term_size1/5 :: measure_t * bound * clause_ppkey_t
	* nnegint * term.
min_unifiable_term_size1(int, _Bound, ClausePPKey, PrincipalPos, Size) :-
	clause_type(ClausePPKey, Type),
	clause_head_body(ClausePPKey, Head, Body),
	arg(PrincipalPos, Head, Term),
	( var(Term) ->
	    ( Type == rule -> min_unifiable_int(Body, Term, Size)
	    ; Size = 0 ) ; % may unify with any integer
	    Size = bot ).

min_unifiable_int((LitPPKey, Body), Var, Size) :-
	!,
	min_unifiable_int_lit(LitPPKey, Var, S),
	min_unifiable_int(Body, Var, Sizes),
% 	Warning: ckeck this! %chk1
% 	Commented by PLG (22-Mar-97)
	maximum(S, Sizes, Size).
% 	Added by PLG (22-Mar-97)
% 	up_low_approximation(S,Sizes,Size).
% 	End added
min_unifiable_int(LitPPKey, Var, Size) :-
	min_unifiable_int_lit(LitPPKey, Var, Size).

min_unifiable_int_lit(LitPPKey, Var, S) :-
	lit_ppkey(LitPPKey, Lit, _PPKey),
	functor(Lit, F, N),
	( comparison_op(F/N) ->
	    min_lit_int(Lit, Var, S) ;
	    S = 0 ).

min_lit_int(Lit, Var, Size) :-
	arg(1, Lit, Arg1),
	Arg1 == Var, !,
	arg(2, Lit, Arg2),
	( integer(Arg2) ->
	    ( functor(Lit, F, _),
		min_lit_int1(F, Arg2, Size) ) ;
	    Size = 0 ).
min_lit_int(Lit, Var, Size) :-
	arg(2, Lit, Arg2),
	Arg2 == Var, !,
	arg(1, Lit, Arg1),
	( integer(Arg1) ->
	    ( functor(Lit, F, _),
		min_lit_int2(F, Arg1, Size) ) ;
	    Size = 0 ).
min_lit_int(_, _, 0).

min_lit_int1(('arithmetic:=:='),    N, N).
min_lit_int1(('term_compare:=='),   N, N).
min_lit_int1(('arithmetic:>='),     N, N).
min_lit_int1(('arithmetic:>'),      N, M) :- M is N+1.
min_lit_int1(('arithmetic:=<'),     _, 0).
min_lit_int1(('arithmetic:<'),      _, 0).
min_lit_int1(('arithmetic:=\\='),   _, 0).
min_lit_int1(('term_compare:\\=='), _, 0).

min_lit_int2(('arithmetic:=:='),    N, N).
min_lit_int2(('term_compare:=='),   N, N).
min_lit_int2(('arithmetic:>='),     _, 0).
min_lit_int2(('arithmetic:>'),      _, 0).
min_lit_int2(('arithmetic:=<'),     N, N).
min_lit_int2(('arithmetic:<'),      N, M) :- M is N+1.
min_lit_int2(('arithmetic:=\\='),   _, 0).
min_lit_int2(('term_compare:\\=='), _, 0).

:- push_prolog_flag(discontiguous_warnings, off).

min_unifiable_term_size1(length, _Bound, ClausePPKey, Pos, Size) :-
	clause_head(ClausePPKey, Head),
	arg(Pos, Head, Term),
	min_length(Term, Size).

min_length(Term, 0) :- var(Term).
min_length(Term, Size) :- nonvar(Term), Term = [_|L],
	min_length(L, S), Size is S+1.

min_unifiable_term_size1(depth(R), Bound, ClausePPKey, PrinciplePos, Size) :-
	clause_head(ClausePPKey, Head),
	arg(PrinciplePos, Head, Term),
	min_depth(Term, Bound, R, Size).

min_depth(Term, _Bound, _, 0) :-
	var(Term),
	!.
min_depth(Term, Bound, R, Size) :-
	compound(Term),
	functor(Term, _, N),
	min_depth1(N, Bound, Term, R, S),
	Size is S + 1.

min_depth1(0, _Bound, _,    _, 0).
min_depth1(N, Bound,  Term, R, Size) :- N > 0,
	(
	    utility_res:member(R, N) ->
	    arg(N, Term, Arg),
	    min_depth(Arg, Bound, R, S)
	;
	    S = 0
	),
	N1 is N-1,
	min_depth1(N1, Bound, Term, R, Ss),
% Commented by PLG (22-Mar-97)
% max(S,Ss,Size).
	up_low_approximation_minmax(Bound, S, Ss, Size).

min_unifiable_term_size1(size, _Bound, ClausePPKey, PrinciplePos, Size) :-
	clause_head(ClausePPKey, Head),
	arg(PrinciplePos, Head, Term),
	min_size(Term, Size).

:- pop_prolog_flag(discontiguous_warnings).

min_size(Term, 1) :- var(Term).
min_size(Term, Size) :- compound(Term),
	functor(Term, _, N),
	min_size1(N, Term, S), Size is S+1.

min_size1(0, _,    0).
min_size1(N, Term, Size) :- N > 0,
	arg(N, Term, Arg),
	min_size(Arg, S),
	N1 is N-1,
	min_size1(N1, Term, Ss),
	add(S, Ss, Size).

%
%  Solve the complexity non-difference equations.
%

%% Commented out by PLG (22-Mar-97)
%% solve_comp_non_diff_equs(Equ,Sol) :-
%% 	comp_list(Equ,Comp),
%% 	maximum_list(Comp,Sol).

% Added by PLG (22-Mar-97)

%% solve_comp_non_diff_equs(Equ,Sol) :-
%% 	comp_list(Equ,Comp),
%% 	up_low_approximation_list(Comp,Sol).

solve_comp_non_diff_equs(Bound, Equ, Sol) :- % Added Sep 97 -PLG
	up_low_approximation_list(Bound, Equ, Sol).

%% up_low_approximation_list(Comp,Sol):-
%%    approximation(Approx),
%%    up_low_approximation_list(Approx, Comp, Sol).

up_low_approximation_list(lower, Equ, Sol) :-
	!,
	low_exclu_list(Equ, Comp), minimum_list(Comp, Sol).
up_low_approximation_list(upper, Equ, Sol) :-
	comp_list(Equ, Comp), maximum_list(Comp, Sol).

low_exclu_list([],                 []).
low_exclu_list([equ(_, _, C)|Equ], [C|Comp]) :-
	low_exclu_list(Equ, Comp).

% End added

comp_list([],                 []).
comp_list([equ(_, _, C)|Equ], [C|Comp]) :-
	comp_list(Equ, Comp).

%
%  Compute the maximum of a list of normal form expressions.
%
maximum_list([],       Zero) :- normal_form(0, Zero).
maximum_list([S|Size], Sol) :-
	maximum_list(Size, Sols),
%general_form(Sols,GSol),
%write(GSol),nl,
	max_expr(Sols, S, Sol).



%
%  Fetch the input sizes for the clauses of a predicate.
%
input_sizes(Size, _, []) :-
	var(Size),
	!.
input_sizes([S|Size], Mode, [IS|ISize]) :-
	input_size(Mode, S, IS),
	input_sizes(Size, Mode, ISize).

%
%  Fetch the input sizes for a clause.
%
input_size([], _, []) :-
	!.
input_size([(+)|Mode], [S|Size], [S|ISize]) :-
	input_size(Mode, Size, ISize),
	!.
input_size([(-)|Mode], [_|Size], ISize) :-
	input_size(Mode, Size, ISize).

%
%  Fetch the nth output sizes for the clauses of a predicate.
%
output_sizes(Size, _, []) :-
	var(Size),
	!.
output_sizes([S|Size], N, [O|OSize]) :-
	ith_list_element(N, S, O),
	output_sizes(Size, N, OSize).

input_size_vars(Mode, Size) :-
	input_size_vars_(Mode, 1, Size).

input_size_vars_([],         _, []).
input_size_vars_([(+)|Mode], N, [$(0, N)|Size]) :-
	N1 is N + 1,
	input_size_vars_(Mode, N1, Size).
input_size_vars_([(-)|Mode], N, Size) :-
	input_size_vars_(Mode, N, Size).

all_numbers([Size]) :-
	!,
	all_numbers_(Size).
all_numbers([Size|RSize]) :-
	all_numbers_(Size),
	all_numbers(RSize).

all_numbers_([Size]) :-
	!,
	number(Size).
all_numbers_([Size|RSize]) :-
	number(Size),
	all_numbers_(RSize).

:- pred consistent_input_sizes/5 :: predname * list(symbol_entry) *
	list(predname) * term * term # "Check if the input sizes of
	the clauses of a predicate is consistent.  recursive and
	nonrecursive predicates are considered differently.".
consistent_input_sizes(Pred, ST, Component, ISize, Size) :-
	( recursive_predicate(Pred, ST, Component) ->
	    find_symbol_field(ST, Pred, clause, ClauseKeys),
	    recursive_consistent_input(ClauseKeys, Component, ISize, 0, Size)
	; find_symbol_field(ST, Pred, (mode), Mode),
	    ( all_numbers(ISize) ->
		input_size_vars(Mode, Size)
	    ;
		nonrecursive_consistent_input(ISize, 0, Size) ) ).

%
%  If the predicate is recursive, the input sizes of recursive clauses must
%  be the same.
%
recursive_consistent_input(ClauseKeys, _, _, _, _) :-
	var(ClauseKeys),
	!.
recursive_consistent_input([ClauseKey|ClauseKeys], Component, [IS|ISize], S,
	    Size) :-
	clause_key(ClauseKey, ClausePPKey, _Key),
	( recursive_clause(ClausePPKey, Component) ->
	    assign_consistent_input(S, Size, IS),
	    recursive_consistent_input(ClauseKeys, Component, ISize, 1, Size) ;
	    recursive_consistent_input(ClauseKeys, Component, ISize, S, Size) ).

assign_consistent_input(0, Size0, Size) :- Size0 = Size.
assign_consistent_input(1, Size0, Size) :- Size0 == Size.

%
%  If the predicate is nonrecursive, the input sizes for all the clauses
%  must be the same.
%

nonrecursive_consistent_input([],         _, _).
nonrecursive_consistent_input([IS|ISize], S, Size) :-
	assign_consistent_input(S, Size, IS),
	nonrecursive_consistent_input(ISize, 1, Size).

%
%  Construct the complexity equations.
%
construct_comp_equs([], _, _, []).
construct_comp_equs([I|ISize], [C|Comp], ClauseNum,
	    [equ(ClauseNum, I, NC)|Equs]) :-
	normal_form(C, NC),
	ClauseNum1 is ClauseNum+1,
	construct_comp_equs(ISize, Comp, ClauseNum1, Equs).

classify_equs([],       [],   []).
classify_equs([E|Equs], DEqu, BEqu) :-
	E = equ(_, _, O),
%general_form(O,GO),
%write(GO),nl,
	( diff_equ(O) ->
	    ( DEqu = [E|DEqus],
		BEqu = BEqus ) ;
	    ( DEqu = DEqus,
		BEqu = [E|BEqus] ) ),
	classify_equs(Equs, DEqus, BEqus).
%
%  Combine the input sizes and output sizes.
%
combine_size([],        _,         _,     []).
combine_size([+| Mode], [I|Isize], Osize, [S|Size]) :-
	simplification(I, S),
	combine_size(Mode, Isize, Osize, Size).
combine_size([-| Mode], Isize, [O|Osize], [S|Size]) :-
	simplification(O, S),
	combine_size(Mode, Isize, Osize, Size).

%
%  Construct a list of dummy sizes.
%

:- push_prolog_flag(multi_arity_warnings, off).

dummy_size([],       _, []).
dummy_size([+|Mode], I, [($(0, I))|Size]) :-
	I1 is I+1,
	dummy_size(Mode, I1, Size).
dummy_size([-| Mode], I, [inf|Size]) :-
	I1 is I+1,
	dummy_size(Mode, I1, Size).

dummy_size([],       _, _,     _,     []).
dummy_size([+|Mode], I, Bound, Sizes, [($(0, I))|Size]) :-
	I1 is I+1,
	dummy_size(Mode, I1, Bound, Sizes, Size).
dummy_size([-|Mode], I, Bound, Sizes, [Osize|Size]) :-
	output_sizes(Sizes, I, OSize),
	solve_output_dummy_sizes(OSize, Bound, Osize),
	I1 is I+1,
	dummy_size(Mode, I1, Bound, Sizes, Size).

:- pop_prolog_flag(multi_arity_warnings).

solve_output_dummy_sizes([Osize],                 _Bound, Osize).
solve_output_dummy_sizes([Osize1, Osize2|OSizes], Bound,  OSizes_) :-
	( Bound == upper ->
	    maximum(Osize1, Osize2, Osize)
	;
	    minimum(Osize1, Osize2, Osize)
	),
	( OSizes = [] -> OSizes_ = Osize
	; solve_output_dummy_sizes([Osize|OSizes], Bound, OSizes__),
	    OSizes_ = OSizes__ ).

%
%  Test if an equation is a difference equation or not.
%
% Commented out by PLG (25-Mar-97)

diff_equ(expr(T, _)) :-
	T \== [].

% Added by PLG (25-Mar-97)

%% diff_equ(NFEqu) :-
%%         NFEqu = expr(T,_),
%% 	T \== [],
%%         general_form(NFEqu, GFEqu), 
%%         \+ is_a_nonrecursive_equation(GFEqu).
%% 
%% is_a_nonrecursive_equation(X):-
%% 	number(X),!.
%% is_a_nonrecursive_equation(X) :-
%% 	compound(X), userfunc(X), !,
%%         functor(X, _, N),
%% 	userfunc_is_a_nonrecursive_equation(N, X).
%% is_a_nonrecursive_equation(X):-
%%         compound(X),
%%         functor(X, _, N),
%%         comp_is_a_nonrecursive_equation(N,X).
%% 
%% comp_is_a_nonrecursive_equation(0,_).
%% comp_is_a_nonrecursive_equation(N,X) :-
%% 	N > 0,
%% 	arg(N,X,Arg),
%% 	is_a_nonrecursive_equation(Arg),
%% 	N1 is N-1,
%% 	comp_is_a_nonrecursive_equation(N1,X).
%% 
%% userfunc_is_a_nonrecursive_equation(0,_).
%% userfunc_is_a_nonrecursive_equation(N,X) :-
%% 	N > 0,
%% 	arg(N,X,Arg),
%% 	integer(Arg),
%% 	N1 is N-1,
%% 	userfunc_is_a_nonrecursive_equation(N1,X).

% End added

% non_diff_equ(expr([], _)).
% %non_diff_equ(bot).
% non_diff_equ(inf).

%
indirect_recursion([],                _).
indirect_recursion([equ(_, _, E)|DE], Pred) :-
%general_form(E,GE),
%write(GE),nl,
	indirect_recursion_expr(E, Pred),
	indirect_recursion(DE, Pred).

indirect_recursion_expr(expr(T, _), Pred) :-
% Added by PLG
% debug
% nl, write('Entering indirect_recursion_expr/2'), nl, 
% copy_term(T, T1),
% numbervars(T1,0,_),
% debug  
%  general_form(expr(T,_),GT),!,
% end debug
% copy_term(T, T2),
% numbervars(T2,0,_),
% T1 == T2,
% debug  
% write(GT), 
% write(' ... '),
% end debug
%End added. 
	( indirect_recursion_term(T, Pred) ->
	    true
% debug
% write(' true '), nl
% end debug
	;
% debug
% write(' false '), nl, 
% end debug
	    fail ).
%End added. 
indirect_recursion_term([],              _).
indirect_recursion_term([term(P, _)|Ts], Pred) :-
	indirect_recursion_primary(P, Pred),
	indirect_recursion_term(Ts, Pred).

indirect_recursion_primary([],       _).
indirect_recursion_primary([DVar|P], F/N) :-
	userfunc(DVar),
	functor(DVar, F1, _), F1 \== F, !,
	indirect_recursion_primary(P, F/N).
indirect_recursion_primary([DVar|P], F/N) :-
	userfunc(DVar),
	functor(DVar, F, M), M > 2,
	arg(1, DVar, Arity), general_form(Arity, A), A \== N,
	indirect_recursion_primary(P, F/N).

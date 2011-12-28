%
% From Susana modified by VPC.
%
:- module(cneg_rt_Stuckey, [cneg_rt_Stuckey/2], [assertions]).
% NOT NEEDED:  perform_a_call_to/1
:- meta_predicate cneg(goal).
%:- meta_predicate cneg_processed_pred(goal,?). 

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.
:- multifile call_to/1.

:- use_module(cneg_aux, _).
:- use_module(cneg_diseq, [diseq_uqv/3, eq_uqv/3, diseq_eqv/3, eq_eqv/3, 
	diseq_euqv/4, eq_euqv/4,
	diseq_euqv_adv/5, eq_euqv_adv/5]).
:- use_module(library(aggregates),[setof/3]).
%:- use_module(library(cneg_diseq),[cneg_diseq/3]).
% Esta linea para cuando cneg sea una libreria.

:- comment(title, "Contructive Negation Runtime Library - Stuckey's Proposal").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module implements Stuckey's proposal of Constructive Negation.").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_rt_Stuckey(UQV, Goal):-
	echo_msg(1, 'cneg_rt_Stuckey', '', 'cneg_rt :: (UQV, Goal)', (UQV, Goal)),
	varsbag(Goal, UQV, [], GoalVars),
	compute_neg_frontier(Goal, GoalVars, 'true').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% compute_neg_frontier(Goal,Frontier) 
% obtains in Frontier the frontier of the  goal Goal.
% It is a list of list that represent the disjunction of its
% elements where each element is a conjunction of subgoals.

% Just to debug.
compute_neg_frontier(Goal, GoalVars, Prev_Front_Residua) :-
	echo_msg(1, 'cneg_rt_Stuckey', '', '--------------------------------------------------------------------------------------------------------------', ' '),
	echo_msg(1, 'cneg_rt_Stuckey', '', 'compute_neg_frontier :: (Goal, GoalVars, Prev_Front_Residua)', (Goal, GoalVars, Prev_Front_Residua)),	
	fail. % Just debug and use backtracking to continue.

% First remove $ and qualification from the goal's name.
compute_neg_frontier(Goal, GoalVars, Prev_Front_Residua) :-
	goal_clean_up(Goal, Tmp_Goal), !,
	compute_neg_frontier(Tmp_Goal, GoalVars, Prev_Front_Residua).

compute_neg_frontier(Goal, GoalVars, Prev_Front_Residua):- 
	Prev_Front_Residua \== 'true',
	echo_msg(1, 'cneg_rt_Stuckey', '', 'compute_neg_frontier :: Prev_Front_Residua', Prev_Front_Residua),	
	evaluate_prev_frontier_residua(Goal, GoalVars, Prev_Front_Residua).

% Manage true and fail ...
compute_neg_frontier('true', _GoalVars, 'true') :- !, fail.
compute_neg_frontier('fail', _GoalVars, 'true') :- !.

% Now go for the disjunctions.
compute_neg_frontier(Goal, GoalVars, 'true'):- 
	goal_is_disjunction(Goal, G1, G2), !,
	echo_msg(1, 'cneg_rt_Stuckey', '', 'compute_neg_frontier :: disj :: negate G1', ' '),
	compute_neg_frontier(G1, GoalVars, 'true'),
	echo_msg(1, 'cneg_rt_Stuckey', '', 'compute_neg_frontier :: disj :: negate G2', ' '),
	compute_neg_frontier(G2, GoalVars, 'true').

% Now go for the conjunctions.
compute_neg_frontier(Goal, GoalVars, 'true'):- 
	goal_is_conjunction(Goal, G1, G2), !,
	(
	    (
		echo_msg(1, 'cneg_rt_Stuckey', '', 'compute_neg_frontier :: conj :: negate G1', ' '),
		compute_neg_frontier(G1, GoalVars, 'true')
	    )
	;
	    (
		echo_msg(1, 'cneg_rt_Stuckey', '', '--------------------------------------------------------------------------------------------------------------', ' '),
		echo_msg(1, 'cneg_rt_Stuckey', '', 'compute_neg_frontier :: conj :: assume G1, negate G2', ' '),
		compute_neg_frontier(G2, GoalVars, G1)
	    )
	).

% Now go for the functors for equality and disequality.
compute_neg_frontier(Goal, GoalVars, 'true'):- 
	goal_is_disequality(Goal, T1, T2, EQV, UQV), !, 
	EQV = [],
	varsbag(Goal, GoalVars, UQV, UQV_Aux),
	eq_uqv(T1, T2, UQV_Aux).

compute_neg_frontier(Goal, GoalVars, 'true'):- 
	goal_is_equality(Goal, T1, T2, EQV, UQV), !,
	EQV = [], 
	varsbag(Goal, GoalVars, UQV, UQV_Aux),
	% cneg_diseq(T1,T2, UQV_In, UQV_Out, Do_Not_Fail, Result).
	diseq_uqv(T1,T2, UQV_Aux).

compute_neg_frontier(Goal, _GoalVars, 'true'):- 
	goal_is_negation(Goal, _UQV, SubGoal), !,
	call_to(SubGoal). % Forget GoalVars and UQV.

% Now go for other functors stored in our database.
compute_neg_frontier(Goal, GoalVars, 'true') :-
	goal_is_not_conj_disj_eq_diseq_dneg(Goal),
	echo_msg(1, 'cneg_rt_Stuckey', '', 'compute_neg_frontier :: Goal', Goal),
	look_for_the_relevant_clauses(Goal, Frontier_Tmp_1),
	echo_msg(1, 'cneg_rt_Stuckey', '', 'compute_neg_frontier :: format', '(Head, Body, FrontierTest)'),
	echo_msg(1, 'cneg_rt_Stuckey', '', 'Frontier_Tmp_1', Frontier_Tmp_1),
	simplify_frontier(Frontier_Tmp_1, Goal, Frontier_Tmp_2),
	echo_msg(1, 'cneg_rt_Stuckey', '', 'Frontier_Tmp_2', Frontier_Tmp_2), 
	combine_frontiers_by_disjunction(Frontier_Tmp_2, 'fail', Frontier_Tmp_3),
	echo_msg(1, 'cneg_rt_Stuckey', '', 'Frontier_Tmp_3', Frontier_Tmp_3), 
	!, % Frontier is uniquely determined if this clause is used.
	compute_neg_frontier(Frontier_Tmp_3, GoalVars, 'true').

% And at last report an error if it was impossible to found a valid entry.
compute_neg_frontier(Goal, GoalVars, Prev_Front_Residua) :-
	echo_msg(1, 'cneg_rt_Stuckey', '', 'ERROR: compute_neg_frontier :: (Goal, GoalVars, Prev_Front_Residua)', (Goal, GoalVars, Prev_Front_Residua)), 
	nl, !, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Structure to manage all the info about the frontier in an easy way.
frontier_contents(frontier(Head, Body, FrontierTest), Head, Body, FrontierTest).

% Look for those saved clauses with same name and arity.	
%look_for_the_relevant_clauses(Goal, _Frontier) :-
%	functor(Goal, Name, Arity),  % Security
%	Name \== ',', Name \== ';',    % Issues
%	cneg_processed_pred(Name, Arity, SourceFileName, _Occurences), 
%	cneg_dynamic_cl(Name, Arity, SourceFileName, Head, Body),
%	debug_clause('look_for_the_relevant_clauses', cneg_dynamic_cl(Name, Arity, SourceFileName, Head, Body)),
%	fail.

%look_for_the_relevant_clauses(_Goal, _Frontier) :-
%	cneg_processed_pred(G, H, I, J), 
%	debug_clause('look_for_the_relevant_clauses', cneg_processed_pred(G, H, I, J)),
%	fail.

look_for_the_relevant_clauses(Goal, Frontier) :-
	functor(Goal, Name, Arity),  % Security
	Name \== ',', Name \== ';',    % Issues
	!, % Backtracking forbiden.
	cneg_pre_frontier(Name, Arity, _SourceFileName, _Head_Aux, _Body_Aux, _FrontierTest_Aux), 
%	debug_clause('look_for_the_relevant_clauses :: (Name, Arity, SourceFileName)', (Name, Arity, SourceFileName)),
	setof(frontier(Head, Body, FrontierTest), 
	cneg_pre_frontier(Name, Arity, _SourceFileName, Head, Body, FrontierTest), Frontier).

combine_frontiers_by_disjunction([], Input, Input) :- !.
combine_frontiers_by_disjunction([Frontier|Frontiers_List], Input, Output) :- !,
	combine_frontiers_by_disjunction_aux(Frontier, Input, Temp),
	combine_frontiers_by_disjunction(Frontiers_List, Temp, Output).

combine_frontiers_by_disjunction_aux('fail', Frontier, Frontier) :- !.
combine_frontiers_by_disjunction_aux(Frontier, 'fail', Frontier) :- !.
combine_frontiers_by_disjunction_aux(Frontier_1, Frontier_2, ((Frontier_1) ; (Frontier_2))) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% simplify_frontier(Front,Frontier) simplifies the frontier Front.
simplify_frontier([], _Goal, []) :- !.
simplify_frontier([Frontier | More_Frontier_In], Goal, [Body | More_Bodies]):-
	frontier_contents(Frontier, Head, Body, Frontier_Test),
	test_frontier_is_valid(Goal, Head, Frontier_Test), !,
	echo_msg(1, 'cneg_rt_Stuckey', '', 'simplify_frontier :: valid: ', Frontier),
	simplify_frontier(More_Frontier_In, Goal, More_Bodies).
simplify_frontier([Frontier|More_Frontier_In], Goal, More_Bodies):-
	echo_msg(1, 'cneg_rt_Stuckey', '', 'simplify_frontier :: not valid: ', Frontier),
	simplify_frontier(More_Frontier_In, Goal, More_Bodies).

% simplify_frontier_unifying_variables(H, Body_In, G, Body_Out) 
% returns in Body_Out the elements of Body whose head unifies with G.
test_frontier_is_valid(Goal, Head, Frontier_Test) :-
	echo_msg(1, 'cneg_rt_Stuckey', '', 'test_frontier_is_valid :: (Head, FrontierTest, Goal)', (Head, Frontier_Test, Goal)),
        copy_term((Head, Frontier_Test, Goal), (Head_Tmp, Frontier_Test_Tmp, Goal_Tmp)), 
        eq_uqv(Head_Tmp, Goal_Tmp, []), 
	goal_is_equality(Frontier_Test_Tmp, Tmp_Left, Tmp_Right, _Tmp_EQV, Tmp_UQV), % It must be an equality.
	eq_uqv(Tmp_Left, Tmp_Right, Tmp_UQV), % Note that UQV = [].
%	call_combined_solutions(FrontierTest_Tmp), 
%	echo_msg(1, 'cneg_rt_Stuckey', '', 'test_frontier_is_valid', 'YES'),
	!, % Backtracking forbidden.
	functor_local(Goal, _Name, _Arity, Goal_Args), 
	goal_is_equality(Frontier_Test, Test_Left, _Test_Right, _Test_EQV, _Test_UQV),
	eq_uqv(Test_Left, Goal_Args, []). % Note that UQV = [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evaluate_prev_frontier_residua(Goal, GoalVars_In, Prev_Front_Residua) :-
	echo_msg(1, 'cneg_rt_Stuckey', '', 'evaluate_prev_frontier_residua :: Prev_Front_Residua', Prev_Front_Residua),	
	Prev_Front_Residua \== 'true',

	varsbag(GoalVars_In, [], [], GoalVars),
	varsbag((Goal, Prev_Front_Residua), GoalVars, [], LocalVars),
	echo_msg(1, 'cneg_rt_Stuckey', '', 'evaluate_prev_frontier_residua :: GoalVars_In', GoalVars_In),
	% We need to obtain 1 solution and ask for more solutions sharing its solutions for goalvars. 
	% In this way we do not hang when an infinite number of solutions is found.
	% This is not completely true because it fails for p(X) :- p(X). but in this case it's clearly 
	% a programming error (this has no meaning at all).
	copy_term((Prev_Front_Residua, GoalVars, LocalVars), (Prev_Front_Residua_Copy, GoalVars_Copy, _LocalVars_Copy)),
	!,
	evaluate_prev_frontier_residua_aux(Prev_Front_Residua_Copy),
	eq_uqv(GoalVars, GoalVars_Copy, []),
	setof(([(Goal, Prev_Front_Residua, LocalVars)], GoalVars), evaluate_prev_frontier_residua_aux(Prev_Front_Residua), Pre_F_Sols),
	echo_msg(1, 'cneg_rt_Stuckey', '', 'evaluate_prev_frontier_residua :: Pre_F_Sols', Pre_F_Sols),
	echo_msg(1, 'cneg_rt_Stuckey', 'nl', '', ''),
%	prepare_pre_frontier_sols(Pre_F_Sols, Pre_F_Sols_Aux),
	unify_pre_sols_when_possible(Pre_F_Sols, Pre_F_Sols_Final),
	take_and_compute_one_solution(GoalVars, Pre_F_Sols_Final).

%prepare_pre_frontier_sols([], []) :- !.
%prepare_pre_frontier_sols([Pre_F_Sol | Pre_F_Sols], [Pre_F_Sol_Out | Pre_F_Sols_Out]) :-
%	pre_front_sol(Pre_F_Sol, Prev_Front_Residua, LocalVars, GoalVars),
%	pre_front_sol(Pre_F_Sol, [Prev_Front_Residua], [LocalVars], GoalVars),
%	prepare_pre_frontier_sols(Pre_F_Sols, Pre_F_Sols_Out).

pre_front_sol((Local_Stuff, GoalVars), Local_Stuff, GoalVars).

take_and_compute_one_solution(_Real_GoalVars, []) :- fail.
take_and_compute_one_solution(Real_GoalVars, [Pre_F_Sol | Pre_F_Sols]) :-
	(
	    pre_front_sol(Pre_F_Sol, Local_Stuff, GoalVars),
	    echo_msg(1, 'cneg_rt_Stuckey', '', 'take_one_solution :: Local_Stuff ', Local_Stuff),
	    eq_uqv(Real_GoalVars, GoalVars, []),
	    compute_localstuff_solutions(Real_GoalVars, Local_Stuff)
	;
	    take_and_compute_one_solution(Real_GoalVars, Pre_F_Sols)
	).

compute_localstuff_solutions(_Real_GoalVars, []).
compute_localstuff_solutions(Real_GoalVars, [(Goal, _Prev_Front_Residua, _LocalVars) | Local_Stuff]) :-
	compute_neg_frontier(Goal, Real_GoalVars, 'true'),
	compute_localstuff_solutions(Real_GoalVars, Local_Stuff).

evaluate_prev_frontier_residua_aux(Prev_Front_Residua) :-
	goal_is_conjunction(Prev_Front_Residua, Prev_Front_Residua_1, Prev_Front_Residua_2), 
	evaluate_prev_frontier_residua_aux(Prev_Front_Residua_1),
	evaluate_prev_frontier_residua_aux(Prev_Front_Residua_2).

evaluate_prev_frontier_residua_aux(Prev_Front_Residua) :-
	goal_is_disjunction(Prev_Front_Residua, Prev_Front_Residua_1, Prev_Front_Residua_2), 
	(
	    evaluate_prev_frontier_residua_aux(Prev_Front_Residua_1)
	;
	    evaluate_prev_frontier_residua_aux(Prev_Front_Residua_2)
	).

evaluate_prev_frontier_residua_aux(Prev_Front_Residua) :-
	goal_is_disequality(Prev_Front_Residua, Left, Right, EQV, UQV), 
	EQV = [], 
	diseq_uqv(Left, Right, UQV).

evaluate_prev_frontier_residua_aux(Prev_Front_Residua) :-
	goal_is_equality(Prev_Front_Residua, Left, Right, EQV, UQV), 
	EQV = [], 
	eq_uqv(Left, Right, UQV).

evaluate_prev_frontier_residua_aux(Prev_Front_Residua) :-
	goal_is_negation(Prev_Front_Residua, UQV, SubGoal), 
	cneg_rt_Stuckey(UQV, SubGoal).

evaluate_prev_frontier_residua_aux(Prev_Front_Residua) :-
	nonvar(Prev_Front_Residua),
	goal_is_not_conj_disj_eq_diseq_dneg(Prev_Front_Residua),
	call_to(Prev_Front_Residua).

unify_pre_sols_when_possible([], []) :- !.
unify_pre_sols_when_possible(To_Unify, After_Unified) :- 
	echo_msg(1, 'cneg_rt_Stuckey', '', 'evaluate_prev_frontier_residua :: To_Unify', To_Unify),
	unify_sols_when_possible_aux_1(To_Unify, [], Unified, [], Not_Unified), !,
	(
	    (
		Unified == [], 
		echo_msg(1, 'cneg_rt_Stuckey', '', 'evaluate_prev_frontier_residua :: Not_Unified', Not_Unified),
		After_Unified = Not_Unified
	    )
	;
	    (
		Unified \== [], 
		append(Not_Unified, Unified, New_To_Unify), 
		echo_msg(1, 'cneg_rt_Stuckey', '', 'evaluate_prev_frontier_residua :: New_To_Unify', New_To_Unify),
		unify_pre_sols_when_possible(New_To_Unify, After_Unified)
	    )
	).

unify_sols_when_possible_aux_1([Head_To_Unify], Unified_Out, Unified_Out, Not_Unified_In, Not_Unified_Out) :-
	unify_sols_when_possible_aux_2(Head_To_Unify, Unified_Out, [], _Unified_With_Head, Not_Unified_With_Head),
	append(Not_Unified_With_Head, Not_Unified_In, Not_Unified_Out).

unify_sols_when_possible_aux_1([Head | Tail_To_Unify], Unified_In, Unified_Out, Not_Unified_In, Not_Unified_Out) :-
	unify_sols_when_possible_aux_2(Head, Tail_To_Unify, [], Unified_With_Head, Not_Unified_With_Head),
	append(Unified_With_Head, Unified_In, Unified_Aux),
	append(Not_Unified_With_Head, Not_Unified_In, Not_Unified_Aux),
	unify_sols_when_possible_aux_1(Tail_To_Unify, Unified_Aux, Unified_Out, Not_Unified_Aux, Not_Unified_Out).

unify_sols_when_possible_aux_2(Head_To_Unify, [], [], [], [Head_To_Unify]) :- !.
unify_sols_when_possible_aux_2(_Head_To_Unify, [], Unified, Unified, []) :- !.
unify_sols_when_possible_aux_2(Head_1, [Head_2 | Tail_To_Unify], Unified_In, Unified_Out, Not_Unified_Out) :-
	copy_term((Head_1, Head_2), (Head_1_Copy, Head_2_Copy)),
	pre_front_sol(Head_1_Copy, Local_Stuff_1, GoalVars_1),
	pre_front_sol(Head_2_Copy, Local_Stuff_2, GoalVars_2),
	eq_uqv(GoalVars_1, GoalVars_2, []), !, % They unify !!!
	append(Local_Stuff_1, Local_Stuff_2, Local_Stuff_Aux),
	pre_front_sol(Head_Out, Local_Stuff_Aux, GoalVars_1),
	unify_sols_when_possible_aux_2(Head_1, Tail_To_Unify, [Head_Out | Unified_In], Unified_Out, Not_Unified_Out).
unify_sols_when_possible_aux_2(Head_1, [_Head_2 | Tail_To_Unify], Unified_In, Unified_Out, Not_Unified_Out) :-
	unify_sols_when_possible_aux_2(Head_1, Tail_To_Unify, Unified_In, Unified_Out, Not_Unified_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


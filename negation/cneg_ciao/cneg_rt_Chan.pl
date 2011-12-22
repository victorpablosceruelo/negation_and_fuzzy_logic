%
% From Susana modified by VPC.
%
:- module(cneg_rt_Chan, [cneg_rt_Chan/2, cneg_rt_New/2], [assertions]).
% NOT NEEDED:  perform_a_call_to/1
:- meta_predicate cneg(goal).
%:- meta_predicate cneg_processed_pred(goal,?). 

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.
:- multifile call_to/1.

:- use_module(cneg_aux, _).
:- use_module(cneg_diseq, [diseq_uqv/3, eq_uqv/3, diseq_eqv/3, eq_eqv/3, 
	portray_attributes_in_term/2,
	cneg_diseq_eqv_uqv/4, cneg_eq_eqv_uqv/4,
	cneg_diseq_eqv_uqv_adv/5, cneg_eq_eqv_uqv_adv/5]).
:- use_module(library(aggregates),[setof/3]).

%:- use_module(library(cneg_diseq),[cneg_diseq/3]).
% Esta linea para cuando cneg sea una libreria.

:- comment(title, "Contructive Negation Runtime Library - Chan's Proposal").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module implements Chan's proposal of Constructive Negation.").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_rt_Chan(UQV, Goal):-
	cneg_rt_Aux(UQV, Goal, 'cneg_rt_Chan', [], Result),
	call_to(Result).

cneg_rt_New(UQV, Goal):-
	cneg_rt_Aux(UQV, Goal, 'cneg_rt_New', [], Result),
	call_to(Result).

cneg_rt_Aux(UQV_In, Goal, Proposal, Trace, Result) :-
	echo_msg_nl(2),
	echo_statistics,
	echo_msg_nl(2),
	echo_msg(1, 'cneg_rt_Aux :: (UQV_In, Goal, Proposal)', (UQV_In, Goal, Proposal)),
	by_pass_universallity_of_variables(UQV_In, UQV_Aux),
	echo_msg(2, 'cneg_rt_Aux :: (UQV_In, Goal, Proposal)', (UQV_Aux, Goal, Proposal)),
	portray_attributes_in_term(2, Goal),
	varsbag(UQV_Aux, [], [], UQV),
	varsbag(Goal, UQV, [], GoalVars),
	
	compute_set_of_frontiers(Goal, Proposal, Trace, UQV, Frontier, New_UQV),
	echo_msg_list(2, 'cneg_rt_Aux :: Frontier', Frontier),

	negate_set_of_frontiers(Frontier, Proposal, GoalVars, New_UQV, Result), !,
	echo_msg(2, 'cneg_rt_Aux :: Result', Result),
	echo_msg_nl(2).

by_pass_universallity_of_variables(UQV_In, UQV_Aux) :-
	varsbag(UQV_In, [], [], UQV_Aux). % All vars in UQV_In are now UQV.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Structure to manage all the info about the frontier in an easy way.
frontier_contents(frontier(Goal, Head, Body, FrontierTest), Goal, Head, Body, FrontierTest).

% combine_frontiers(F1,F2,F3) returns F3 that is the resulting frontier
% from combining the frontiers F1 and F2 in all possible ways.
combine_frontiers_from_conjunction([],_F2,[]):-!.
combine_frontiers_from_conjunction([F1_1 | More_F1], F2, F3):-
        combine_frontiers_from_conjunction_aux(F1_1, F2, F3_1),
        combine_frontiers_from_conjunction(More_F1, F2, More_F3),
        cneg_aux:append(F3_1, More_F3, F3).

% combine_frontiers_aux_1(F1_1,F2, F3) returns F3 that is 
% the result of combining F1_1 with each element of F2.
combine_frontiers_from_conjunction_aux(_F1_1, [], []).
combine_frontiers_from_conjunction_aux(F1_1, [F2_1 | More_F2], [F3 | More_F3]):-
	frontier_contents(F1_1, F1_1_Real_Goal, F1_1_Head, F1_1_Body, F1_1_F_Test),
	frontier_contents(F2_1, F2_1_Real_Goal, F2_1_Head, F2_1_Body, F2_1_F_Test),
	F3_Real_Goal = ((F1_1_Real_Goal), (F2_1_Real_Goal)),
	F3_Head = ((F1_1_Head), (F2_1_Head)),
	F3_Body = ((F1_1_Body), (F2_1_Body)),
	F3_F_Test = ((F1_1_F_Test), (F2_1_F_Test)),
	frontier_contents(F3, F3_Real_Goal, F3_Head, F3_Body, F3_F_Test),
        combine_frontiers_from_conjunction_aux(F1_1, More_F2, More_F3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adequate_frontier([], Real_UQV, [], Real_UQV) :- !.
adequate_frontier([F_In | Frontier_In], Real_UQV, [F_Out | Frontier_Out], Frontier_New_UQV_Out) :-
	adequate_frontier_aux(Real_UQV, F_In, F_Out, F_New_UQV),
	adequate_frontier(Frontier_In, Real_UQV, Frontier_Out, Frontier_New_UQV_Aux),
	append(F_New_UQV, Frontier_New_UQV_Aux, Frontier_New_UQV_Out).

adequate_frontier_aux(Real_UQV, F_In, Body_Copy, New_UQV) :-
	frontier_contents(F_In, Real_Goal, Head, Body, _F_Test),
	copy_term((Head, Body), (Head_Copy, Body_Copy)), % Fresh copy to avoid variable clashes.
	varsbag(Real_Goal, Real_UQV, [], Real_Goal_GoalVars), % Determine goalvars.
	copy_term((Real_Goal, Real_Goal_GoalVars, Real_UQV), (Real_Goal_Copy, Real_Goal_GoalVars_Copy, New_UQV)),
	Real_Goal_GoalVars = Real_Goal_GoalVars_Copy, % Unify goalvars, but not UQV.
	Real_Goal_Copy = Head_Copy, % Unify head and goal definitely
	!. % Backtracking is forbidden.

adequate_frontier_aux(Real_UQV, F_In, _Body_Copy, _New_UQV) :-
	echo_msg(2, 'ERROR: adequate_frontier_aux(Real_UQV, F_In)', (Real_UQV, F_In)),
	!, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute_set_of_frontiers(Goal, Proposal, Trace, UQV, Frontier, New_UQV) :-
	split_goal_with_disjunctions_into_goals(Goal, Proposal, Goals),
	compute_set_of_frontiers_aux(Goals, Proposal, Trace, UQV, Frontier, New_UQV).

compute_set_of_frontiers_aux([], _Proposal, _Trace, _UQV, [], []) :- !.
compute_set_of_frontiers_aux([Goal | More_Goals], Proposal, Trace, UQV, Frontier_Out, New_UQV_Out) :-
	compute_goal_frontier(Goal, Proposal, Trace, Frontier_Aux), !,
	echo_msg_list(2, 'cneg_rt_Aux :: Frontier_Aux', Frontier_Aux),
	adequate_frontier(Frontier_Aux, UQV, Frontier_Tmp, New_UQV_Tmp), !,
%	echo_msg(2, 'cneg_rt_Aux :: (UQV)', (New_UQV_Tmp)),
	compute_set_of_frontiers_aux(More_Goals, Proposal, Trace, UQV, Frontier_In, New_UQV_In),
	append(Frontier_Tmp, Frontier_In, Frontier_Out),
	varsbag(New_UQV_Tmp, [], New_UQV_In, New_UQV_Out).

% compute_neg_frontier(Goal,Frontier) 
% obtains in Frontier the frontier of the  goal Goal.
% It is a list of list that represent the disjunction of its
% elements where each element is a conjunction of subgoals.

% Just to debug.
%compute_goal_frontier(Goal, _Frontier) :-
%	echo_msg(2, '--------------------------------------------------------------------------------------------------------------', ' '),
%	echo_msg(2, 'compute_goal_frontier :: (Goal)', (Goal)),	
%	fail. % Just debug and use backtracking to continue.

% First remove $ and qualification from the goal's name.
compute_goal_frontier(Goal, Proposal, Trace, Frontier) :-
	goal_clean_up(Goal, Tmp_Goal), !,
	compute_goal_frontier(Tmp_Goal, Proposal, Trace, Frontier).

% Manage true and fail ...
compute_goal_frontier('true', _Proposal, _Trace, [F_Out]) :- !,
	frontier_contents(F_Out, 'true', 'true', 'true', 'true').
compute_goal_frontier('fail', _Proposal, _Trace, [F_Out]) :- !,
	frontier_contents(F_Out, 'fail', 'fail', 'fail', 'true').

% Now go for the disjunctions.
% The frontiers need to evaluated one at a time. 
compute_goal_frontier(Goal, _Proposal, _Trace, _Frontier_Out):- 
	goal_is_disjunction(Goal, _G1, _G2), !,
	echo_msg(2, 'ERROR: Not possible computing the frontier for a disjunction', Goal), 
	echo_msg_nl(2), !, % Backtracking is forbidden.
	fail.

% Now go for the conjunctions.
compute_goal_frontier(Goal, Proposal, Trace, Frontier):- 
	goal_is_conjunction(Goal, G1, G2), !,
	compute_goal_frontier(G1, Proposal, Trace, Frontier_G1),
	compute_goal_frontier(G2, Proposal, Trace, Frontier_G2),
	combine_frontiers_from_conjunction(Frontier_G1, Frontier_G2, Frontier).

% Now go for the functors for equality and disequality.
% None of them is managed yet, so just bypass them.
compute_goal_frontier(Goal, _Proposal, _Trace, [F_Out]) :- 
	goal_is_disequality(Goal, T1, T2, EQV, UQV), !,
	functor_local(Real_Goal, 'diseq_eqv_uqv', 4, [ T1 |[ T2 |[ EQV |[ UQV ]]]]),
	frontier_contents(F_Out, Real_Goal, Real_Goal, Real_Goal, 'true').

compute_goal_frontier(Goal, _Proposal, _Trace, [F_Out]) :- 
	goal_is_equality(Goal, T1, T2, EQV, UQV), !,
	functor_local(Real_Goal, 'eq_uqv', 4, [ T1 |[ T2 |[ EQV |[ UQV ]]]]),
	frontier_contents(F_Out, Real_Goal, Real_Goal, Real_Goal, 'true').

% Double negation is not managed yet. Bypass it.
%compute_goal_frontier(Goal, Proposal, Real_Goal, [F_Out]) :- 
compute_goal_frontier(Goal, Proposal, Trace, Frontier) :- 
	goal_is_negation(Goal, UQV, SubGoal), !,
	(
	    (
		Proposal = 'Chan',
		cneg_rt_Aux(UQV, SubGoal, Proposal, Trace, Result)
	    )
	;
	    (
		Proposal = 'New',
		cneg_rt_Aux(UQV, SubGoal, Proposal, Trace, Result)
	    )
	),
	echo_msg(0, 'compute_goal_frontier :: Trace', [Goal | Trace]),
	split_goal_with_disjunctions_into_goals(Result, Proposal, Results),
	build_a_frontier_from_each_result(Goal, Results, Frontier).
%	compute_goal_frontier(Result, Proposal, [Goal | Trace], Real_Goal, Frontier).
%	OLD: functor_local(Real_Goal, 'cneg_rt', 2, [ UQV |[ SubGoal ]]),
%	OLD: frontier_contents(F_Out, Real_Goal, Real_Goal, 'true').

% Only as info.
% frontier_contents(frontier(Head, Body, FrontierTest), Head, Body, FrontierTest).

% Now go for other functors stored in our database.
compute_goal_frontier(Goal, _Proposal, _Trace, Frontier_Out) :-
	goal_is_not_conj_disj_eq_diseq_dneg(Goal),
%	echo_msg(0, 'compute_goal_frontier :: Goal', Goal),
	look_for_the_relevant_clauses(Goal, Frontier_Tmp_1),
%	echo_msg(0, 'compute_neg_frontier :: format', '(Head, Body, FrontierTest)'),
%	echo_msg_list(2, 'Frontier_Tmp_1', Frontier_Tmp_1),
	simplify_frontier(Frontier_Tmp_1, [], Frontier_Out),
%	echo_msg(0, 'Frontier_Out', Frontier_Out), 
	!. % Backtracking is forbidden.

% And at last report an error if it was impossible to found a valid entry.
compute_goal_frontier(Goal, _Proposal, _Trace, []) :-
	echo_msg(2, 'ERROR: Not found frontier for Goal', Goal), 
	echo_msg_nl(2), !. % Backtracking is forbidden.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_a_frontier_from_each_result(_Real_Goal, [], []) :- !.
build_a_frontier_from_each_result(Real_Goal, [Result | Results], [Frontier | Frontiers]) :-
	frontier_contents(Frontier, Real_Goal, Real_Goal, Result, 'true'),
	build_a_frontier_from_each_result(Real_Goal, Results, Frontiers).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

look_for_the_relevant_clauses(Goal, Frontier) :-
	functor(Goal, Name, Arity),  % Security
	Name \== ',', Name \== ';',    % Issues
	!, % Backtracking forbiden.
	cneg_pre_frontier(Name, Arity, _SourceFileName, _Head_Aux, _Body_Aux, _FrontierTest_Aux), 
%	debug_clause('look_for_the_relevant_clauses :: (Name, Arity, SourceFileName)', (Name, Arity, SourceFileName)),
	setof(frontier(Goal, Head, Body, FrontierTest), 
	cneg_pre_frontier(Name, Arity, _SourceFileName, Head, Body, FrontierTest), Frontier).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% simplify_frontier(Front,Frontier) simplifies the frontier Front.
% Since the frontiers retrieved are in an inverted order, 
% we must reorder them to keep procedural semantics unchanged.
simplify_frontier([], Frontier_Acc, Frontier_Acc) :- !.
simplify_frontier([F_In | Frontier_In], Frontier_Acc, Frontier_Out) :-
	test_frontier_is_valid(F_In), !,
%	echo_msg(2, 'simplify_frontier :: valid: ', F_In),
	simplify_frontier(Frontier_In, [F_In | Frontier_Acc], Frontier_Out).
simplify_frontier([_F_In | Frontier_In], Frontier_Acc, Frontier_Out) :-
%	echo_msg(2, 'simplify_frontier :: not valid: ', F_In),
	simplify_frontier(Frontier_In, Frontier_Acc, Frontier_Out).

% simplify_frontier_unifying_variables(H, Body_In, G, Body_Out) 
% returns in Body_Out the elements of Body whose head unifies with G.
test_frontier_is_valid(F_In) :-
	frontier_contents(F_In, Goal, Head, _Body, F_Test),
	echo_msg(0, 'test_frontier_is_valid :: (Goal, Head, F_Test)', (Goal, Head, F_Test)),
	copy_term((Goal, F_Test), (Goal_Tmp, F_Test_Tmp)),
	F_Test_Tmp = Goal_Tmp, % Test that test and goal can be unified. 
% Old way:
%        copy_term((Goal, Head, F_Test), (Goal_Tmp, Head_Tmp, F_Test_Tmp)), 
 %       Head_Tmp = Goal_Tmp, % Test that head and goal can be unified. 
%	goal_is_equality(F_Test_Tmp, Tmp_Left, Tmp_Right, _Eq_EQV, _Eq_UQV), % It must be an equality.
%	Tmp_Left = Tmp_Right, % Test that unifications previously in head can be performed.
	!. % Backtracking forbidden. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% negate_set_of_frontiers(Frontier,Goal,LSolutions) returns in LSolutions
% a list with the same number of elements of the list Frontier.
% Each element of LSolutions will be one solution of negating the 
% conjuction that is in the same position in Frontier. The predicate 
% is a "map" where the fuction that is applied to each element of
% Frontier (each conjunction) is the negation.
% Frontier is the frontier of subgoals of deep 1 of Goal and we need
% it to keep the variables of the Goal and obtain the unifications
negate_set_of_frontiers([], _Proposal, _GoalVars, _UQV, true) :- !. % Optimization.
negate_set_of_frontiers([Frontier | More_Frontiers], Proposal, GoalVars, UQV, Result) :-
%	echo_msg(2, 'negate_frontier: (Frontier, GoalVars)', (Frontier, GoalVars)),
	negate_frontier(Frontier, Proposal, GoalVars, UQV, Result_Frontier),
%	echo_msg(2, 'negate_frontier: Result_Frontier', Result_Frontier),
	!, % Reduce the stack's memory.
	negate_set_of_frontiers(More_Frontiers, Proposal, GoalVars, UQV, Result_More_Frontiers),
	combine_negated_frontiers(Result_Frontier, Result_More_Frontiers, Result), 
	!. % Reduce the stack's memory.
	

% combine_negated_subfrontiers(Result_Subfr, Result_More_Subfr, Result_Tmp),
combine_negated_frontiers(fail, _Result_More_Subfr, fail) :- !.
combine_negated_frontiers(_Result_Subfr, fail, fail) :- !.
combine_negated_frontiers(true, Result_More_Subfr, Result_More_Subfr) :- !.
combine_negated_frontiers(Result_Subfr, true, Result_Subfr) :- !.
combine_negated_frontiers(Result_Subfr, Result_More_Subfr, (Result_Subfr, Result_More_Subfr)) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% negate_frontier((Head, BodyList), Proposal, GoalVars, Result) 
% returns in Result a solution of the negation of the conjunction of subgoals 
% (Head, BodyList) of the goal Goal.

negate_frontier(Frontier_In, Proposal, GoalVars, UQV, Result):-
%	echo_msg(2, 'negate_frontier :: Frontier_In', (Frontier_In)),
	split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Aux_1),
%	split_frontier_contents(Frontier_Aux_1, E_Aux_1, IE_Aux_1, NIE_Aux_1),
%	echo_msg(2, 'negate_frontier :: (E_Aux_1, IE_Aux_1, NIE_Aux_1)', (E_Aux_1, IE_Aux_1, NIE_Aux_1)),
	!, % Reduce the stack's memory.
	normalize_E_IE_NIE(Proposal, Frontier_Aux_1, GoalVars, Frontier_Aux_2, ImpVars),
	split_frontier_contents(Frontier_Aux_2, E_Aux_2, IE_Aux_2, NIE_Aux_2),
%	echo_msg(2, 'negate_frontier :: (E_Aux_2, IE_Aux_2, NIE_Aux_2)', (E_Aux_2, IE_Aux_2, NIE_Aux_2)),
	split_IE_NIE_between_imp_and_exp(IE_Aux_2, NIE_Aux_2, ImpVars, IE_imp, NIE_imp, IE_NIE_exp),
%	echo_msg(2, 'negate_frontier :: (IE_imp, NIE_imp, IE_NIE_exp)', (IE_imp, NIE_imp, IE_NIE_exp)),
	negate_formula(E_Aux_2, IE_imp, NIE_imp, IE_NIE_exp, Proposal, GoalVars, ImpVars, UQV, Result),
%	echo_msg(2, 'negate_frontier :: (Result)', (Result)),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rebuild_conjunction_of_goals([], [], []) :- !. % Empty elements when re-joining
rebuild_conjunction_of_goals(Goals, [], Goals) :- !. % Empty element when re-joining
rebuild_conjunction_of_goals([], Goals, Goals) :- !. % Empty element when re-joining
rebuild_conjunction_of_goals(Goals_1, Goals_2, (Goals_1, Goals_2)) :- 
	Goals_1 \== [], 
	Goals_2 \== [], !.

split_frontier_contents(frontier(E_In, IE_In, NIE_In), E_In, IE_In, NIE_In).

%empty_frontier(Frontier) :- split_frontier_contents(Frontier, [], [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_frontier_into_E_IE_NIE(Frontier_In, _Frontier_Out) :-
	goal_is_disjunction(Frontier_In, _G1, _G2), !, 
	echo_msg(2, 'ERROR: split_frontier_into_E_IE_NIE can not deal with disjunctions. Frontier_In', Frontier_In),
	fail.

split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :-
	goal_is_conjunction(Frontier_In, G1, G2), !,
	split_frontier_into_E_IE_NIE(G1, Frontier_G1),
	split_frontier_into_E_IE_NIE(G2, Frontier_G2),
	split_frontier_contents(Frontier_G1, E_G1, IE_G1, NIE_G1),
	split_frontier_contents(Frontier_G2, E_G2, IE_G2, NIE_G2),
	rebuild_conjunction_of_goals(E_G1, E_G2, E_Out),
	rebuild_conjunction_of_goals(IE_G1, IE_G2, IE_Out),
	rebuild_conjunction_of_goals(NIE_G1, NIE_G2, NIE_Out),
	split_frontier_contents(Frontier_Out, E_Out, IE_Out, NIE_Out).

split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_equality(Frontier_In, _Term1, _Term2, _EQV, _UQV), !,
	split_frontier_contents(Frontier_Out, Frontier_In, [], []).

split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_disequality(Frontier_In, _Term1, _Term2, _EQV, _UQV), !,
	split_frontier_contents(Frontier_Out, [], Frontier_In, []).

% This leads to infinite loops because double negation 
% sould be managed when generating the frontier.
% The way to fix this is remove cneg(cneg(...))
% when evaluating the frontier. To be done.
split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_negation(Frontier_In, _UQV, _SubGoal), !,
	split_frontier_contents(Frontier_Out, [], [], Frontier_In).

split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_not_conj_disj_eq_diseq_dneg(Frontier_In), !,
	split_frontier_contents(Frontier_Out, [], [], Frontier_In).

split_frontier_into_E_IE_NIE(Frontier_In, _Frontier_Out) :- 
	echo_msg(2, 'ERROR: split_frontier_into_E_IE_NIE can not deal with frontier. Frontier_In', Frontier_In),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% normalize_I_D_R(I,D,R,GoalVars, In,Dn,Rn,ImpVars) 
% returns In and Dn that are the equalities of I and the disequalities
% of D but after normalizating them.
normalize_E_IE_NIE('New', Formula_In, GoalVars, Formula_In, ImpVars):-
	split_frontier_contents(Formula_In, E_In, _IE_In, _NIE_In),
	varsbag(GoalVars, [], [], Real_GoalVars), % Sometimes we have non vars in GoalVars.
	varsbag(E_In, [], Real_GoalVars, ImpVars). % ImpVars = vars(E) + GoalVars

normalize_E_IE_NIE('Chan', Formula_In, GoalVars, Formula_Out, ImpVars):-
	split_frontier_contents(Formula_In, E_In, _IE_In, _NIE_In),
	varsbag(GoalVars, [], [], Real_GoalVars), % Sometimes we have non vars in GoalVars.
	varsbag(E_In, Real_GoalVars, [], Vars_EnoG), % Vars_EnoG = vars(E) - GoalVars
	remove_from_E_irrelevant_equalities(Formula_In, Vars_EnoG, Formula_Aux),  
	split_frontier_contents(Formula_Aux, E_Out, IE_Aux, NIE_Out),
%	echo_msg(2, 'normalize_E_IE_NIE :: (E_Out, IE_Aux, NIE_Out)', (E_Out, IE_Aux, NIE_Out)),
	varsbag(E_Out, [], Real_GoalVars, ImpVars), % ImpVars = vars(E) + GoalVars
	varsbag(NIE_Out, Real_GoalVars, [], RelVars), % RelVars = vars(NIE) - GoalVars
	varsbag_addition(ImpVars, RelVars, ImpVars_and_RelVars),
%	echo_msg(2, 'remove_from_IE_irrelevant_disequalities :: (IE_Aux, ImpVars_and_RelVars)', (IE_Aux, ImpVars_and_RelVars)),
	remove_from_IE_irrelevant_disequalities(IE_Aux, ImpVars_and_RelVars, IE_Out),
%	echo_msg(2, 'remove_from_IE_irrelevant_disequalities :: (IE_Out)', (IE_Out)),
	split_frontier_contents(Formula_Out, E_Out, IE_Out, NIE_Out). 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% remove_from_I_irrelevant_equalities(I,D,R,GoalVars,Iac,Iv,Dv,Rv) 
% removes from I the equalities X=Y that contain a variable, 
% X or Y,that is not in GoalVars and 
% makes the unification of the corresponding value in D and R.
% Iac is use to acumulate the equalities not removed untill the process ends,
% and update the value of I.
remove_from_E_irrelevant_equalities(Formulae_In, Vars_EnoG, Formulae_Out) :- 
	split_frontier_contents(Formulae_In, E_In, _IE_In, _NIE_In), 
	detect_irrelevant_equalities(E_In, Vars_EnoG, [], Irrelevant_Eqs),
%	echo_msg(2, 'remove_from_E_irrelevant_equalities :: Irrelevant_Eqs', Irrelevant_Eqs),
	(
	    (   
		Irrelevant_Eqs = [],
		Formulae_Out = Formulae_In
	    )
	;
	    (
		Irrelevant_Eqs \== [],
		remove_irrelevant_equalities_1(Irrelevant_Eqs, Vars_EnoG, Formulae_In, Formulae_Aux_1),
%		echo_msg(2, 'remove_irrelevant_equalities_1 :: Formulae_Aux_1', Formulae_Aux_1),
		split_frontier_contents(Formulae_Aux_1, E_Aux_1, IE_Aux, NIE_Aux), 
		remove_irrelevant_equalities_2(E_Aux_1, [], _Eqs_Visited, E_Aux_2),
%		echo_msg(2, 'remove_irrelevant_equalities_2 :: E_Aux_2', E_Aux_2),
		split_frontier_contents(Formulae_Aux_2, E_Aux_2, IE_Aux, NIE_Aux), 
		remove_from_E_irrelevant_equalities(Formulae_Aux_2, Vars_EnoG, Formulae_Out)
	    )
	).

detect_irrelevant_equalities([], _Vars_EnoG, Irrelevant_Eqs_In, Irrelevant_Eqs_In) :- !.
detect_irrelevant_equalities(E_In, Vars_EnoG, Irrelevant_Eqs_In, Irrelevant_Eqs_Out) :-
	goal_is_conjunction(E_In, E_In_1, E_In_2), !,
	detect_irrelevant_equalities(E_In_1, Vars_EnoG, Irrelevant_Eqs_In, Irrelevant_Eqs_Aux),
	detect_irrelevant_equalities(E_In_2, Vars_EnoG, Irrelevant_Eqs_Aux, Irrelevant_Eqs_Out).

detect_irrelevant_equalities(E_In, Vars_EnoG, Irrelevant_Eqs_In, Irrelevant_Eqs_Out) :-
	goal_is_disjunction(E_In, E_In_1, E_In_2), !, % Unnecessary ??
	detect_irrelevant_equalities(E_In_1, Vars_EnoG, Irrelevant_Eqs_In, Irrelevant_Eqs_Aux),
	detect_irrelevant_equalities(E_In_2, Vars_EnoG, Irrelevant_Eqs_Aux, Irrelevant_Eqs_Out).

detect_irrelevant_equalities(E_In, Vars_EnoG, Irrelevant_Eqs_In, [ E_In | Irrelevant_Eqs_In]) :-
	goal_is_equality(E_In, Value_1, Value_2, _EQV, _UQV),
	(
	    (
		var(Value_1),
		memberchk(Value_1, Vars_EnoG), ! % Vars_EnoG = vars(E) - GoalVars
	    )
	;
	    (
		var(Value_2),
		memberchk(Value_2, Vars_EnoG), ! % Vars_EnoG = vars(E) - GoalVars
	    )
	).
detect_irrelevant_equalities(_E_In, _Vars_EnoG, Irrelevant_Eqs_In, Irrelevant_Eqs_In).

remove_irrelevant_equalities_1([], _Vars_EnoG, Formula_In, Formula_In) :- !.
remove_irrelevant_equalities_1([Irrelevant_Eq | Irrelevant_Eqs], Vars_EnoG, Formula_In, Formula_Out) :-
	goal_is_equality(Irrelevant_Eq, Value_1, Value_2, _EQV, _UQV),
	(
	    (
		var(Value_1),
		memberchk(Value_1, Vars_EnoG), !, % Vars_EnoG = vars(E) - GoalVars
		replace_in_term_var_by_value(Formula_In, Value_1, Value_2, Formula_Aux)
	    )
	;
	    (
		var(Value_2),
		memberchk(Value_2, Vars_EnoG), !, % Vars_EnoG = vars(E) - GoalVars
		replace_in_term_var_by_value(Formula_In, Value_2, Value_1, Formula_Aux)
	    )
	;   % In case they have been previously removed ...
	    true
	),
	remove_irrelevant_equalities_1(Irrelevant_Eqs, Vars_EnoG, Formula_Aux, Formula_Out).

remove_irrelevant_equalities_2([], Eqs_Visited_In, Eqs_Visited_In, []) :- !.
remove_irrelevant_equalities_2(E_In, Eqs_Visited_In, Eqs_Visited_Out, E_Out) :-
	goal_is_conjunction(E_In, E_In_1, E_In_2), !,
	remove_irrelevant_equalities_2(E_In_1, Eqs_Visited_In, Eqs_Visited_Aux, E_Out_1),
	remove_irrelevant_equalities_2(E_In_2, Eqs_Visited_Aux, Eqs_Visited_Out, E_Out_2),
	rebuild_conjunction_of_goals(E_Out_1, E_Out_2, E_Out).

remove_irrelevant_equalities_2(E_In, Eqs_Visited_In, Eqs_Visited_In, []) :-
	goal_is_equality(E_In, Value_1, Value_2, _EQV, _UQV),
	Value_1 == Value_2, !.

remove_irrelevant_equalities_2(E_In, Eqs_Visited_In, Eqs_Visited_In, []) :-
	goal_is_equality(E_In, _Value_1, _Value_2, _EQV, _UQV),
	memberchk(E_In, Eqs_Visited_In), !.

remove_irrelevant_equalities_2(E_In, Eqs_Visited_In, Eqs_Visited_In, []) :-
	goal_is_equality(E_In, _Value_1, _Value_2, _EQV, _UQV),
	functor_local(E_In, Name, Arity, Args_In), 
	functor_local(E_Aux, Name, Arity, Args_Aux), 
	list_head_and_tail(Args_In, Head_1, Tail_1), 
	list_head_and_tail(Tail_1, Head_2, Tail_2), 
	list_head_and_tail(Tail_3, Head_1, Tail_2),  % Different order
	list_head_and_tail(Args_Aux, Head_2, Tail_3), 
	memberchk(E_Aux, Eqs_Visited_In), !.

remove_irrelevant_equalities_2(E_In, Eqs_Visited_In, [E_In | Eqs_Visited_In], E_In).

% remove_from_D_irrelevant_disequalities(D,ImpVars,RelVars,D1) returns D1
% that is D but without disequalities that contains any variable that
% is not in ImpVars neither RelVars
% We need to assure that the inequalities here are atomic because 
% non-atomic ones result into a disjunction that can not be simplified
% by using this procedure.
remove_from_IE_irrelevant_disequalities([], _ImpVars_and_RelVars, []) :- !.

remove_from_IE_irrelevant_disequalities(IE_In, ImpVars_and_RelVars, IE_Out) :-
	goal_is_conjunction(IE_In, IE_In_1, IE_In_2), !,
	remove_from_IE_irrelevant_disequalities(IE_In_1, ImpVars_and_RelVars, IE_Out_1),
	remove_from_IE_irrelevant_disequalities(IE_In_2, ImpVars_and_RelVars, IE_Out_2),
	rebuild_conjunction_of_goals(IE_Out_1, IE_Out_2, IE_Out).

remove_from_IE_irrelevant_disequalities(IE_In, ImpVars_and_RelVars, IE_Out):-
	goal_is_disequality(IE_In, _Term1, _Term2, _EQV, _UQV), !,
	varsbag(IE_In, ImpVars_and_RelVars, [], Problematic_Vars),
	(
	    (   Problematic_Vars == [],
		IE_Out = IE_In )
	;
	    (   Problematic_Vars \== [],
		IE_Out = [] )
	).
	
remove_from_IE_irrelevant_disequalities(IE_In, _ImpVars_and_RelVars, _IE_Out) :-
	!,
	echo_msg(2, 'ERROR: remove_from_IE_irrelevant_disequalities can not deal with IE_In', IE_In),
	fail.


% split_D_R_into_imp_and_exp(I,D,R,GoalVars,ImpVars,ExpVars,SolC) returns SolC
% that is one of the solutions of the conjunction that is divided in 
% I, D and R (equalities, disequalities and rest of subgoals).
% GoalVars, ImpVars and ExpVars are set of useful variables 
split_IE_NIE_between_imp_and_exp(IE, NIE, ImpVars, IE_imp, NIE_imp, IE_NIE_exp):-
	split_ie_or_nie_between_imp_and_exp(IE, ImpVars, IE_imp, IE_exp),
%	echo_msg(2, 'split_ie_or_nie_between_imp_and_exp(IE, ImpVars, IE_imp, IE_exp)', split_ie_or_nie_between_imp_and_exp(IE, ImpVars, IE_imp, IE_exp)),
	split_ie_or_nie_between_imp_and_exp(NIE, ImpVars, NIE_imp, NIE_exp),
%	echo_msg(2, 'split_ie_or_nie_between_imp_and_exp(NIE, ImpVars, NIE_imp, NIE_exp)', split_ie_or_nie_between_imp_and_exp(NIE, ImpVars, NIE_imp, NIE_exp)),
	rebuild_conjunction_of_goals(IE_exp, NIE_exp, IE_NIE_exp).

% split_formula_between_imp_and_exp(F,ExpVars,Fimp,Fexp) divide F between Fimp and Fexp.
% In Fexp are the elements of F with any variables of ExpVars and
% the rest of elements of F will be in Fimp
split_ie_or_nie_between_imp_and_exp([], _ImpVars, [], []) :- !.
split_ie_or_nie_between_imp_and_exp(IE_or_NIE, ImpVars, IE_or_NIE_imp, IE_or_NIE_exp) :-
	goal_is_conjunction(IE_or_NIE, IE_or_NIE_1, IE_or_NIE_2), !,
	split_ie_or_nie_between_imp_and_exp(IE_or_NIE_1, ImpVars, IE_or_NIE_imp_1, IE_or_NIE_exp_1),
	split_ie_or_nie_between_imp_and_exp(IE_or_NIE_2, ImpVars, IE_or_NIE_imp_2, IE_or_NIE_exp_2),
	rebuild_conjunction_of_goals(IE_or_NIE_imp_1, IE_or_NIE_imp_2, IE_or_NIE_imp),
	rebuild_conjunction_of_goals(IE_or_NIE_exp_1, IE_or_NIE_exp_2, IE_or_NIE_exp).

split_ie_or_nie_between_imp_and_exp(IE_or_NIE, _ImpVars, _IE_or_NIE_imp, _IE_or_NIE_exp) :-
	goal_is_disjunction(IE_or_NIE, _IE_or_NIE_1, _IE_or_NIE_2), !,
	echo_msg(2, 'ERROR: split_ie_or_nie_between_imp_and_exp can not deal with IE_or_NIE', IE_or_NIE),
	fail.

split_ie_or_nie_between_imp_and_exp(IE_or_NIE, ImpVars, IE_or_NIE_imp, IE_or_NIE_exp) :-
	varsbag(IE_or_NIE, ImpVars, [], ExpVars), % Retrieve only vars not in ImpVars.
	(
	    (
		ExpVars == [], !,
		IE_or_NIE_imp = IE_or_NIE,
		IE_or_NIE_exp = []
	    )
	;
	    (
		ExpVars \== [], !,
		IE_or_NIE_imp = [],
		IE_or_NIE_exp = IE_or_NIE
	    )
	).
%	echo_msg(2, 'split_ie_or_nie_between_imp_and_exp', (IE_or_NIE, IE_or_NIE_imp, IE_or_NIE_exp)).

% negate_formula(GoalVars, ImpVars, ExpVars, I, Dimp, Dexp, Rimp, Rexp, R_naf, Sol)
% returns SolC that is one of the solutions of the conjunction that is divided
% I, Dimp,Dexp,Rimp and Rexp (equalities, disequalities and rest of subgoals).
% GoalVars, ImpVars and ExpVars are set of useful variables 
negate_formula([], [], [], [], _Proposal, _GoalVars, _ImpVars, _UQV, true) :- !. % Optimization
negate_formula(E, IE_imp, NIE_imp, IE_NIE_exp, Proposal, GoalVars, ImpVars, UQV, Neg_E_IE_NIE) :-
%	echo_msg(2, 'negate_formula :: (E, IE_imp, NIE_imp, IE_NIE_exp)', (E, IE_imp, NIE_imp, IE_NIE_exp)),
 	negate_IE_NIE_exp(IE_NIE_exp, Proposal, ImpVars, UQV, Neg_IE_NIE_exp),
	negate_imp_form(NIE_imp, Proposal, ImpVars, UQV, Neg_IE_NIE_exp, Neg_NIE_imp_IE_NIE_exp),
	negate_imp_form(IE_imp, Proposal, ImpVars, UQV, Neg_NIE_imp_IE_NIE_exp, Neg_IE_NIE),
	negate_imp_form(E, Proposal, GoalVars, UQV, Neg_IE_NIE, Neg_E_IE_NIE),
	!. % Backtracking forbidden.

% negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,SolC) obtain in
% SolC a solution of negating Dexp y Rexp juntos.
negate_IE_NIE_exp([], _Proposal, _ImpVars, _UQV, []):- !.
negate_IE_NIE_exp(IE_NIE_exp, 'Chan', ImpVars, UQV, Neg_IE_NIE_exp) :-
	IE_NIE_exp \== [],
	varsbag(IE_NIE_exp, ImpVars, [], ExpVars),
	varsbag(UQV, [], ExpVars, New_UQV),
	Neg_IE_NIE_exp = (cneg_rt_Chan(New_UQV, IE_NIE_exp)), !.
negate_IE_NIE_exp(IE_NIE_exp, 'New', ImpVars, UQV, Neg_IE_NIE_exp) :-
	IE_NIE_exp \== [],
	varsbag(IE_NIE_exp, ImpVars, [], ExpVars),
	varsbag(UQV, [], ExpVars, New_UQV),
	Neg_IE_NIE_exp = (cneg_rt_New(New_UQV, IE_NIE_exp)), !.

negate_imp_form([], _Proposal, _ImpVars, _UQV, [], []) :- !. % Optimization.
negate_imp_form(Formula, _Proposal, _ImpVars, _UQV, _Next_Formula, _Neg_Formula) :-
	goal_is_disjunction(Formula, _Formula_1, _Formula_2), !,
	echo_msg(2, 'ERROR: negate_imp_form can not deal with Formula', Formula),
	fail.

negate_imp_form(Formula, Proposal, ImpVars, UQV, Next_Formula, Neg_Formula) :-
	goal_is_conjunction(Formula, Formula_1, Formula_2), !,
 	negate_imp_form(Formula_2, Proposal, ImpVars, UQV, Next_Formula, Next_Formula_Aux),
 	negate_imp_form(Formula_1, Proposal, ImpVars, UQV, Next_Formula_Aux, Neg_Formula).

negate_imp_form(Formula, Proposal, ImpVars, UQV, Next_Formula, Neg_Formula) :-
	negate_imp_atom(Formula, Proposal, ImpVars, UQV, Neg_Atom, Keep_Atom),
	combine_negated(Neg_Atom, Keep_Atom, Next_Formula, Neg_Formula), 
	!.

combine_negated([], [], Neg_Formula, Neg_Formula) :- !.
combine_negated(Neg_Formula, _Keep_Formula, [], Neg_Formula) :- !.
combine_negated(Neg_Formula_1, Keep_Formula_1, Neg_Formula_2, Neg_Formula) :- 
	Neg_Formula_1 \== [],
	Keep_Formula_1 \== [],
	Neg_Formula_2 \== [],
	!,
	Neg_Formula = (Neg_Formula_1 ; (Keep_Formula_1, Neg_Formula_2)).

% negate_I(I,ImpVars,Sol) obtains in Sol a solution of negating I
negate_imp_atom([], _Proposal, _ImpVars, _UQV, [], []) :- !. % Obvious.
negate_imp_atom(true, _Proposal, _ImpVars, _UQV, fail, true):- !. % Trivial predicates
negate_imp_atom(fail, _Proposal, _ImpVars, _UQV, true, fail):- !. % Trivial predicates

% Negation of equalities. We need to take care of converting UQV to EQV and vice-versa.
% Since EQV are not used in equality basic predicates, we assume that EQV variables
% are the result from applying double negation to a disequality. 
negate_imp_atom(Formula, _Proposal, ImpVars, _UQV, Neg_Atom, Keep_Atom) :-
	goal_is_equality(Formula, T1, T2, Eq_EQV, Eq_UQV),
	varsbag(Eq_EQV, [], [], Real_Eq_EQV),
 	varsbag((T1,T2), ImpVars, Real_Eq_EQV, Diseq_UQV), % Eq_EQV -> Diseq_UQV
	varsbag(Eq_UQV, Diseq_UQV, [], Diseq_EQV), % Eq_UQV -> Diseq_EQV
	Neg_Atom = (cneg_diseq_eqv_uqv(T1, T2, Diseq_EQV, Diseq_UQV)),
	Keep_Atom = (cneg_eq_eqv_uqv(T1, T2, Eq_EQV, Eq_UQV)).

% Idem for disequalities.
negate_imp_atom(Formula, _Proposal, ImpVars, _UQV, Neg_Atom, Keep_Atom) :-
	goal_is_disequality(Formula, T1, T2, Diseq_EQV, Diseq_UQV),
	varsbag(Diseq_EQV, [], [], Real_Diseq_EQV), 
 	varsbag((T1,T2), ImpVars, Real_Diseq_EQV, Eq_UQV), % Diseq_EQV -> Eq_UQV
	varsbag(Diseq_UQV, Eq_UQV, [], Eq_EQV), % Diseq_UQV -> Eq_EQV
	Neg_Atom = (cneg_eq_eqv_uqv(T1,T2, Eq_EQV, Eq_UQV)),
	Keep_Atom = (cneg_diseq_eqv_uqv(T1,T2, Diseq_EQV, Diseq_UQV)).

negate_imp_atom(Formula, 'Chan', ImpVars, UQV, Neg_Atom, Keep_Atom) :-
	varsbag(UQV, [], [], Real_UQV),
 	varsbag(Formula, ImpVars, Real_UQV, Delayed_Negation_UQV),
	Neg_Atom = (cneg_rt_Chan(Delayed_Negation_UQV, Formula)),
	Keep_Atom = (Formula). 

negate_imp_atom(Formula, 'New', ImpVars, UQV, Neg_Atom, Keep_Atom) :-
	varsbag(UQV, [], [], Real_UQV),
 	varsbag(Formula, ImpVars, Real_UQV, Delayed_Negation_UQV),
	Neg_Atom = (cneg_rt_New(Delayed_Negation_UQV, Formula)),
	Keep_Atom = (Formula). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


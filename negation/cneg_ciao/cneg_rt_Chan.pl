%
% From Susana modified by VPC.
%
:- module(cneg_rt_Chan, [cneg_rt_Chan/2, cneg_rt_New/2, cneg_rt_Generic/5], [assertions]).
:- meta_predicate cneg(goal).
%:- meta_predicate cneg_processed_pred(goal,?). 

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.
:- multifile call_to/3.

:- use_module(cneg_aux, _).
:- use_module(cneg_diseq, [diseq_uqv/3, eq_uqv/3, diseq_eqv/3, eq_eqv/3, 
	portray_attributes_in_term_vars/3,
	diseq_euqv/4, eq_euqv/4,
	diseq_euqv_adv/5, eq_euqv_adv/5]).
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
	Proposal = 'cneg_rt_Chan',
	generate_empty_trace(Trace),
	cneg_rt_Generic(UQV, Goal, Proposal, 0, Trace).

cneg_rt_New(UQV, Goal) :-
	Proposal = 'cneg_rt_New',
	generate_empty_trace(Trace),
	cneg_rt_Generic(UQV, Goal, Proposal, 0, Trace).

% The following definition of the predicate cneg_rt_Generic
% can be changed by the commented one
% when we find the current bug 
% (It is just to make it easy debugging). 
cneg_rt_Generic(UQV, Goal, Proposal, Level, Trace) :-
	% Save trace information (basic for debugging).
	Debug_Msg = (cneg_rt_Generic(UQV, Goal, Proposal, Level)),
	generate_conjunction_trace(Trace, Trace_1, Trace_2),
	add_predicate_to_trace(Debug_Msg, Trace_1),

	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	get_trace_status_list(Trace_2, Trace_Status_List),
	echo_msg(2, 'list', 'cneg_rt', 'TRACE: ', Trace_Status_List),
	!,
	cneg_rt_Aux(UQV, Goal, Proposal, [], Result_List),
	!, % Reduce the stack's memory by forbidding backtracking.
	call_to_all_negated_subfrontiers(Result_List, Level, Trace_2, Debug_Msg).

% Please note this mechanism wastes less memory and cpu, 
% since it goes one by one, but allows backtracking.
call_to_all_negated_subfrontiers([], _Level, Trace, Debug_Msg) :- 
	generate_conjunction_trace(Trace, Trace_Logo, Trace_End),
	add_predicate_to_trace(ended_subfrontiers_for(Debug_Msg), Trace_Logo),
	add_predicate_to_trace('-----------------------', Trace_End),
	get_trace_final_status_list(Trace, Status_List),
	echo_msg(2, 'list', 'cneg_rt', 'TRACE: ', Status_List).
call_to_all_negated_subfrontiers([Result | Result_List], Level, Trace, Debug_Msg) :-
	generate_conjunction_trace(Trace, Trace_Current_Goal, Trace_Next_Goal),
	generate_conjunction_trace(Trace_Current_Goal, Trace_Info, Trace_Result),
	generate_conjunction_trace(Trace_Info, Trace_Logo, Trace_Subfrontier),
	add_predicate_to_trace(subfrontier_for(Debug_Msg), Trace_Logo),
	add_predicate_to_trace(Result, Trace_Subfrontier),
	echo_msg(2, '', 'cneg_rt', 'SUBFRONTIER for goal', Debug_Msg),
	echo_msg(2, '', 'cneg_rt', '', Result),
	portray_attributes_in_term_vars(2, 'cneg_rt', Result),
	call_to(Result, Level, Trace_Result),
	call_to_all_negated_subfrontiers(Result_List, Level, Trace_Next_Goal, Debug_Msg).

%generate_disjunction_from_list([], fail) :- !.
%generate_disjunction_from_list([Goal], Goal) :- !.
%generate_disjunction_from_list([Goal | Goals], (Goal ; Disj_Goals)) :-
%	Goals \== [],
%	generate_disjunction_from_list(Goals, Disj_Goals).

cneg_rt_Aux(UQV_In, Goal, Proposal, Trace, Result_List) :-
	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: Proposal', Proposal),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: UQV_In', UQV_In),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: Goal', Goal),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, 'statistics', 'statistics', '', (cneg_rt_Aux(UQV_In, Goal, Proposal, Trace))),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: (UQV_In, Goal, Proposal)', (UQV_In, Goal, Proposal)),
	by_pass_universallity_of_variables(UQV_In, UQV_Aux),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: UQV_Aux', UQV_Aux),
	portray_attributes_in_term_vars(2, 'cneg_rt', Goal),
	varsbag(UQV_Aux, [], [], UQV),
	varsbag(Goal, UQV, [], GoalVars),
	!, % Reduce the stack's memory by forbidding backtracking.
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: GoalVars', GoalVars),
	
	compute_set_of_frontiers(Goal, Proposal, Trace, UQV, Frontier, New_UQV),
	!, % Reduce the stack's memory by forbidding backtracking.
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: New_UQV', New_UQV),
	echo_msg(2, 'list', 'cneg_rt', 'cneg_rt_Aux :: Frontier', Frontier),
	!,
	negate_frontier_list(Frontier, Proposal, GoalVars, New_UQV, Result_List),
	!, % Reduce the stack's memory by forbidding backtracking.
	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: Summary for Proposal', Proposal),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: Goal', Goal),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: UQV_In', UQV_In),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, 'list', 'cneg_rt', 'cneg_rt_Aux :: Frontier', Frontier),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, 'list', 'cneg_rt', 'cneg_rt_Aux :: Result (conj)', Result_List),
	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', '').

by_pass_universallity_of_variables(UQV_In, UQV_Aux) :-
	varsbag(UQV_In, [], [], UQV_Aux). % All vars in UQV_In are now UQV.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Structures to manage all the info about the frontier in an easy way.
frontier_contents(frontier(Goal, Head, Body, FrontierTest), Goal, Head, Body, FrontierTest).
frontier_E_IE_NIE_contents(frontier_E_IE_NIE(E, IE, NIE), E, IE, NIE).
frontier_E_IE_NIE_ied_contents(frontier_E_IE_NIE_ied(E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb), E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
vars_info_contents(vars_info(GoalVars, UQV, ImpVars, ExpVars, RelVars, Dumb_Vars, EQ_to_UQ_Vars, UQ_to_EQ_Vars), GoalVars, UQV, ImpVars, ExpVars, RelVars, Dumb_Vars, EQ_to_UQ_Vars, UQ_to_EQ_Vars).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
combine_frontiers_from_conjunction_aux(F1_1, [F2_1 | More_F2], [F3 | More_F3]) :-
	frontier_contents(F1_1, F1_1_Real_Goal, F1_1_Head, F1_1_Body, F1_1_F_Test),
	frontier_contents(F2_1, F2_1_Real_Goal, F2_1_Head, F2_1_Body, F2_1_F_Test),
	F3_Real_Goal = ((F1_1_Real_Goal), (F2_1_Real_Goal)),
	F3_Head = ((F1_1_Head), (F2_1_Head)),
	F3_Body = ((F1_1_Body), (F2_1_Body)),
	F3_F_Test = ((F1_1_F_Test), (F2_1_F_Test)),
	frontier_contents(F3, F3_Real_Goal, F3_Head, F3_Body, F3_F_Test),
	test_frontier_is_valid(F3, F3_Real_Goal), !, 
        combine_frontiers_from_conjunction_aux(F1_1, More_F2, More_F3).
combine_frontiers_from_conjunction_aux(F1_1, [_F2_1 | More_F2], More_F3) :-
	combine_frontiers_from_conjunction_aux(F1_1, More_F2, More_F3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adequate_frontier([], Real_UQV, [], Real_UQV) :- !.
adequate_frontier([F_In | Frontier_In], Real_UQV, [F_Out | Frontier_Out], Frontier_New_UQV_Out) :-
	adequate_frontier_aux(Real_UQV, F_In, F_Out, F_New_UQV), !,
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
	echo_msg(1, '', 'cneg_rt', 'ERROR: adequate_frontier_aux(Real_UQV, F_In)', (Real_UQV, F_In)),
	!, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute_set_of_frontiers(Goal, Proposal, Trace, UQV, Frontier, New_UQV) :-
	echo_msg(2, '', 'cneg_rt', 'compute_set_of_frontiers :: (Goal, Proposal, Trace, UQV)', (Goal, Proposal, Trace, UQV)),
	split_goal_with_disjunctions_into_goals(Goal, Proposal, Goals),
	!,
	echo_msg(2, 'list', 'cneg_rt', 'compute_set_of_frontiers :: Goals', Goals),
	compute_set_of_frontiers_aux(Goals, Proposal, Trace, UQV, Frontier, New_UQV),
	!.

compute_set_of_frontiers_aux([], _Proposal, _Trace, _UQV, [], []) :- !.
compute_set_of_frontiers_aux([Goal | More_Goals], Proposal, Trace, UQV, Frontier_Out, New_UQV_Out) :-
	compute_goal_frontier(Goal, Proposal, Trace, Frontier_Aux), !,
%	echo_msg(2, 'list', 'cneg_rt', 'compute_set_of_frontiers_aux :: Frontier_Aux', Frontier_Aux),
	adequate_frontier(Frontier_Aux, UQV, Frontier_Tmp, New_UQV_Tmp), !,
%	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: (UQV)', (New_UQV_Tmp)),
	compute_set_of_frontiers_aux(More_Goals, Proposal, Trace, UQV, Frontier_In, New_UQV_In),
	append(Frontier_Tmp, Frontier_In, Frontier_Out),
	varsbag(New_UQV_Tmp, [], New_UQV_In, New_UQV_Out).

% compute_neg_frontier(Goal,Frontier) 
% obtains in Frontier the frontier of the  goal Goal.
% It is a list of list that represent the disjunction of its
% elements where each element is a conjunction of subgoals.

% Just to debug.
%compute_goal_frontier(Goal, _Proposal, _Trace, _Frontier) :-
%	echo_msg(2, '', 'cneg_rt', '--------------------------------------------------------------------------------------------------------------', ' '),
%	echo_msg(2, '', 'cneg_rt', 'compute_goal_frontier :: (Goal)', (Goal)),	
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
	echo_msg(1, '', 'cneg_rt', 'ERROR: Not possible computing the frontier for a disjunction', Goal), 
	echo_msg(1, 'nl', 'cneg_rt', '', ''), !, % Backtracking is forbidden.
	fail.

% Now go for the conjunctions.
compute_goal_frontier(Goal, Proposal, Trace, Frontier):- 
	goal_is_conjunction(Goal, G1, G2), !,
	compute_goal_frontier(G1, Proposal, Trace, Frontier_G1),
	compute_goal_frontier(G2, Proposal, Trace, Frontier_G2),
	!,
	combine_frontiers_from_conjunction(Frontier_G1, Frontier_G2, Frontier),
	!.

% Now go for the functors for equality and disequality.
% None of them is managed yet, so just bypass them.
compute_goal_frontier(Goal, _Proposal, _Trace, [F_Out]) :- 
	goal_is_disequality(Goal, T1, T2, EQV, UQV), !,
	functor_local(Real_Goal, 'diseq_euqv', 4, [ T1 |[ T2 |[ EQV |[ UQV ]]]]),
	frontier_contents(F_Out, Real_Goal, Real_Goal, Real_Goal, 'true'),
	!.

compute_goal_frontier(Goal, _Proposal, _Trace, [F_Out]) :- 
	goal_is_equality(Goal, T1, T2, EQV, UQV), !,
	functor_local(Real_Goal, 'eq_euqv', 4, [ T1 |[ T2 |[ EQV |[ UQV ]]]]),
	frontier_contents(F_Out, Real_Goal, Real_Goal, Real_Goal, 'true'), 
	!.

% Double negation is not managed yet. Bypass it.
%compute_goal_frontier(Goal, Proposal, Real_Goal, [F_Out]) :- 
compute_goal_frontier(Goal, Proposal, Trace, Frontier) :- 
	goal_is_negation(Goal, UQV, SubGoal, _Negation_Proposal), !,
	echo_msg(2, '', 'cneg_rt', 'compute_goal_frontier :: dn :: double negation for (Proposal, UQV, SubGoal)', (Proposal, UQV, SubGoal)),
	cneg_rt_Aux(UQV, SubGoal, Proposal, Trace, Conj_List_Result), !,
	echo_msg(0, '', 'cneg_rt', 'compute_goal_frontier :: dn :: Trace', [Goal | Trace]),
	generate_conjunction_from_list(Conj_List_Result, Conj_Of_Disjs_Frontier), !,
	echo_msg(2, 'list', 'cneg_rt', 'compute_goal_frontier :: dn :: Conj_Of_Disjs_Frontier', Conj_Of_Disjs_Frontier),
	split_goal_with_disjunctions_into_goals(Conj_Of_Disjs_Frontier, Proposal, List_Of_Conjs_Frontier), !,
	echo_msg(2, 'list', 'cneg_rt', 'compute_goal_frontier :: dn :: List_Of_Conjs_Frontier', List_Of_Conjs_Frontier),
	build_a_frontier_from_each_result(Goal, List_Of_Conjs_Frontier, Frontier), !,
	echo_msg(2, 'list', 'cneg_rt', 'double neg :: Frontier', Frontier),
	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	!.

% Only as info.
% frontier_contents(frontier(Head, Body, FrontierTest), Head, Body, FrontierTest).

% Now go for other functors stored in our database.
compute_goal_frontier(Goal, _Proposal, _Trace, Frontier_Out) :-
	goal_is_not_conj_disj_eq_diseq_dneg(Goal),
%	echo_msg(0, '', 'cneg_rt', 'compute_goal_frontier :: Goal', Goal),
	look_for_the_relevant_clauses(Goal, Frontier_Tmp_1),
%	echo_msg(0, '', 'cneg_rt', 'compute_neg_frontier :: format', '(Head, Body, FrontierTest)'),
%	echo_msg(2, 'list', 'cneg_rt', 'Frontier_Tmp_1', Frontier_Tmp_1),
	simplify_frontier(Frontier_Tmp_1, Goal, [], Frontier_Out),
%	echo_msg(0, '', 'cneg_rt', 'Frontier_Out', Frontier_Out), 
	!. % Backtracking is forbidden.

% And at last report an error if it was impossible to found a valid entry.
compute_goal_frontier(Goal, _Proposal, _Trace, []) :-
	echo_msg(1, '', 'cneg_rt', 'ERROR: Not found frontier for Goal', Goal), 
	echo_msg(1, 'nl', 'cneg_rt', '', ''), !. % Backtracking is forbidden.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_conjunction_from_list([], fail) :- !,
	echo_msg(1, '', 'cneg_rt', 'ERROR: generate_conjunction_from_list can not generate a frontier from an empty list.', ''), 
	echo_msg(1, 'nl', 'cneg_rt', '', ''), !, fail. % Backtracking is forbidden.
generate_conjunction_from_list([Goal], Goal) :- !.
generate_conjunction_from_list([Goal | Goals], (Goal , Disj_Goals)) :-
	Goals \== [],
	generate_conjunction_from_list(Goals, Disj_Goals).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_a_frontier_from_each_result(_Real_Goal, [], []) :- !.
build_a_frontier_from_each_result(Real_Goal, [Result | Results], [Frontier | Frontiers]) :-
	frontier_contents(Frontier, Real_Goal, Real_Goal, Result, Real_Goal),
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
	setof(frontier(_Real_Goal, Head, Body, FrontierTest), 
	cneg_pre_frontier(Name, Arity, _SourceFileName, Head, Body, FrontierTest), Frontier).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% simplify_frontier(Front,Frontier) simplifies the frontier Front.
% Since the frontiers retrieved are in an inverted order, 
% we must reorder them to keep procedural semantics unchanged.
simplify_frontier([], _Goal, Frontier_Acc, Frontier_Acc) :- !,
	echo_msg(2, 'nl', 'cneg_rt', '', '').
simplify_frontier([F_In | Frontier_In], Goal, Frontier_Acc, Frontier_Out) :-
	test_frontier_is_valid(F_In, Goal), !,
	echo_msg(2, '', 'cneg_rt', 'simplify_frontier :: valid :: F_In', F_In),
	simplify_frontier(Frontier_In, Goal, [F_In | Frontier_Acc], Frontier_Out).
simplify_frontier([F_In | Frontier_In], Goal, Frontier_Acc, Frontier_Out) :-
	echo_msg(2, '', 'cneg_rt', 'simplify_frontier :: not valid :: F_In', F_In),
	simplify_frontier(Frontier_In, Goal, Frontier_Acc, Frontier_Out).

% simplify_frontier_unifying_variables(H, Body_In, G, Body_Out) 
% returns in Body_Out the elements of Body whose head unifies with G.
test_frontier_is_valid(F_In, Goal) :-
	frontier_contents(F_In, Goal, _Head, _Body, F_Test),
%	echo_msg(2, '', 'cneg_rt', 'test_frontier_is_valid :: F_In', F_In),
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

% negate_frontier_list(Frontier,Goal,LSolutions) returns in LSolutions
% a list with the same number of elements of the list Frontier.
% Each element of LSolutions will be one solution of negating the 
% conjuction that is in the same position in Frontier. The predicate 
% is a "map" where the fuction that is applied to each element of
% Frontier (each conjunction) is the negation.
% Frontier is the frontier of subgoals of deep 1 of Goal and we need
% it to keep the variables of the Goal and obtain the unifications
negate_frontier_list([], _Proposal, _GoalVars, _UQV, [true]) :- !. % Optimization.
negate_frontier_list(Frontier, Proposal, GoalVars, UQV, Negated_Frontier) :- 
	Frontier \== [], !,
	negate_frontier_list_aux(Frontier, Proposal, GoalVars, UQV, Negated_Frontier),
	!.

negate_frontier_list_aux([], _Proposal, _GoalVars, _UQV, []) :- !.
negate_frontier_list_aux([Frontier | More_Frontiers], Proposal, GoalVars, UQV, [Result_Frontier | Result_More_Frontiers]) :-
%	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier: (Frontier, GoalVars)', (Frontier, GoalVars)),
	negate_subfrontier(Frontier, Proposal, GoalVars, UQV, Result_Frontier),
%	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier: Result_Frontier', Result_Frontier),
	!, % Reduce the stack's memory by forbidding backtracking.
	negate_frontier_list_aux(More_Frontiers, Proposal, GoalVars, UQV, Result_More_Frontiers).
%	combine_negated_frontiers(Result_Frontier, Result_More_Frontiers, Result), 
%	!. % Reduce the stack's memory by forbidding backtracking.
	

% combine_negated_subfrontiers(Result_Subfr, Result_More_Subfr, Result_Tmp),
%combine_negated_frontiers(fail, _Result_More_Subfr, fail) :- !.
%combine_negated_frontiers(_Result_Subfr, fail, fail) :- !.
%combine_negated_frontiers(true, Result_More_Subfr, Result_More_Subfr) :- !.
%combine_negated_frontiers(Result_Subfr, true, Result_Subfr) :- !.
%combine_negated_frontiers(Result_Subfr, Result_More_Subfr, (Result_Subfr, Result_More_Subfr)) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% negate_subfrontier((Head, BodyList), Proposal, GoalVars, Result) 
% returns in Result a solution of the negation of the conjunction of subgoals 
% (Head, BodyList) of the goal Goal.

negate_subfrontier(Frontier_In, Proposal, GoalVars, UQV, (Result)):-
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier :: Frontier_In', (Frontier_In)),
	split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Aux_1),
	!, % Reduce the stack's memory by forbidding backtracking.
	normalize_E_IE_NIE(Proposal, Frontier_Aux_1, GoalVars, UQV, Frontier_Aux_2, Vars_Info),
	!, % Reduce the stack's memory by forbidding backtracking.
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier', Vars_Info),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier', Frontier_Aux_2),
	split_IE_NIE_between_imp_exp_and_dumb(Frontier_Aux_2, Vars_Info, Frontier_Aux_3),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier', Frontier_Aux_3),
	!, % Reduce the stack's memory by forbidding backtracking.
	negate_formula(Frontier_Aux_3, Proposal, Vars_Info, Result),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier :: (Result)', (Result)),
	!. % Reduce the stack's memory by forbidding backtracking.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rebuild_conjunction_of_goals([], [], []) :- !. % Empty elements when re-joining
rebuild_conjunction_of_goals(Goals, [], Goals) :- !. % Empty element when re-joining
rebuild_conjunction_of_goals([], Goals, Goals) :- !. % Empty element when re-joining
rebuild_conjunction_of_goals(Goals_1, Goals_2, (Goals_1, Goals_2)) :- 
	Goals_1 \== [], 
	Goals_2 \== [], !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_frontier_into_E_IE_NIE(Frontier_In, _Frontier_Out) :-
	goal_is_disjunction(Frontier_In, _G1, _G2), !, 
	echo_msg(1, '', 'cneg_rt', 'ERROR: split_frontier_into_E_IE_NIE can not deal with disjunctions. Frontier_In', Frontier_In),
	fail.

split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :-
	goal_is_conjunction(Frontier_In, G1, G2), !,
	split_frontier_into_E_IE_NIE(G1, Frontier_G1),
	split_frontier_into_E_IE_NIE(G2, Frontier_G2),
	frontier_E_IE_NIE_contents(Frontier_G1, E_G1, IE_G1, NIE_G1),
	frontier_E_IE_NIE_contents(Frontier_G2, E_G2, IE_G2, NIE_G2),
	rebuild_conjunction_of_goals(E_G1, E_G2, E_Out),
	rebuild_conjunction_of_goals(IE_G1, IE_G2, IE_Out),
	rebuild_conjunction_of_goals(NIE_G1, NIE_G2, NIE_Out),
	frontier_E_IE_NIE_contents(Frontier_Out, E_Out, IE_Out, NIE_Out).

split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_equality(Frontier_In, _Term1, _Term2, _EQV, _UQV), !,
	frontier_E_IE_NIE_contents(Frontier_Out, Frontier_In, [], []).

split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_disequality(Frontier_In, _Term1, _Term2, _EQV, _UQV), !,
	frontier_E_IE_NIE_contents(Frontier_Out, [], Frontier_In, []).

% This leads to infinite loops because double negation 
% sould be managed when generating the frontier.
% The way to fix this is remove cneg(cneg(...))
% when evaluating the frontier. To be done.
split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_negation(Frontier_In, _UQV, _SubGoal, _Negation_Proposal), !,
	frontier_E_IE_NIE_contents(Frontier_Out, [], [], Frontier_In).

split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_not_conj_disj_eq_diseq_dneg(Frontier_In), !,
	frontier_E_IE_NIE_contents(Frontier_Out, [], [], Frontier_In).

split_frontier_into_E_IE_NIE(Frontier_In, _Frontier_Out) :- 
	echo_msg(1, '', 'cneg_rt', 'ERROR: split_frontier_into_E_IE_NIE can not deal with frontier. Frontier_In', Frontier_In),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% normalize_I_D_R(I,D,R,GoalVars, In,Dn,Rn,ImpVars) 
% returns In and Dn that are the equalities of I and the disequalities
% of D but after normalizating them.
normalize_E_IE_NIE('cneg_rt_New', Formula_In, GoalVars, UQV, Formula_In, Vars_Info):-
	compute_variables_information(Formula_In, GoalVars, UQV, Vars_Info).

normalize_E_IE_NIE('cneg_rt_Chan', Formula_In, GoalVars, UQV, Formula_Out, Vars_Info_Out):-
	compute_variables_information(Formula, GoalVars, UQV, Vars_Info_1),
	remove_from_E_redundant_eqs_and_vars(Formula_In, Vars_Info_1, Formula_Aux),  
	echo_msg(2, '', 'cneg_rt', 'normalize_E_IE_NIE :: Formula_Aux', Formula_Aux),
	compute_variables_information(Formula, GoalVars, UQV, Vars_Info_2),
	remove_from_IE_irrelevant_disequalities(Formula_Aux, Vars_Info_2, Formula_Out),
	echo_msg(2, '', 'cneg_rt', 'normalize_E_IE_NIE :: Formula_Out', Formula_Out),
	compute_variables_information(Formula_Out, GoalVars, UQV, Vars_Info_Out).
 
normalize_E_IE_NIE(Proposal, _Formula_In, _GoalVars, _UQV, _Formula_Out, _Vars_Info) :-
	Proposal \== 'cneg_rt_New', 
	Proposal \== 'cneg_rt_Chan', !, 
	echo_msg(2, '', 'cneg_rt', 'normalize_E_IE_NIE :: Unknown proposal', Proposal), !, 
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute_variables_information(Formula, GoalVars, UQV, Vars_Info) :-
	frontier_E_IE_NIE_contents(Formula, E, IE, NIE),
	varsbag(GoalVars, [], [], Real_GoalVars), % Sometimes we have non vars in GoalVars.

	identify_uq_to_eq_vars(E, [], UQ_to_EQ_Vars_E),
	identify_uq_to_eq_vars(IE, UQ_to_EQ_Vars_E, UQ_to_EQ_Vars_E_IE),
	identify_uq_to_eq_vars(NIE, UQ_to_EQ_Vars_E_IE, UQ_to_EQ_Vars),

	varsbag(E, UQ_to_EQ_Vars, [], Vars_E), % Vars_E = vars(E) - UQ_to_EQ_Vars
	varsbag(IE, UQ_to_EQ_Vars, [], Vars_IE), % Vars_IE = vars(IE) - UQ_to_EQ_Vars
	varsbag(NIE, UQ_to_EQ_Vars, [], Vars_NIE),  % Vars_NIE = vars(NIE) - UQ_to_EQ_Vars

	varsbag(Vars_E, [], Real_GoalVars, ImpVars), % ImpVars = vars(E) + GoalVars
	varsbag_difference(Vars_NIE, ImpVars, ExpVars), % Expvars = vars(NIE) - ImpVars
	varsbag_difference(Vars_NIE, Real_GoalVars, RelVars), % RelVars = vars(NIE) - GoalVars

 	varsbag_difference(Vars_IE, ImpVars, Dumb_Vars_Tmp), % Dumb_Vars_Tmp = vars(IE) - GoalVars - vars(E)
 	varsbag_difference(Dumb_Vars_Tmp, Vars_NIE, Dumb_Vars), % Dumb_Vars = vars(IE) - GoalVars - Vars(E) - Vars(NIE)

	varsbag((Vars_E, Vars_IE, Vars_NIE), GoalVars, [], EQ_to_UQ_Vars), % EQ_to_UQ_Vars = Vars(Formula) - UQ_to_EQ_Vars - GoalVars

	vars_info_contents(Vars_Info, GoalVars, UQV, ImpVars, ExpVars, RelVars, Dumb_Vars, EQ_to_UQ_Vars, UQ_to_EQ_Vars).

identify_uq_to_eq_vars(Frontier, UQ_to_EQ_Vars_In, UQ_to_EQ_Vars_Out) :- % Conjunctions
	goal_is_conjunction(Frontier, Frontier_Left, Frontier_Right), !,
	identify_uq_to_eq_vars(Frontier_Left, UQ_to_EQ_Vars_In, UQ_to_EQ_Vars_Aux),
	identify_uq_to_eq_vars(Frontier_Right, UQ_to_EQ_Vars_Aux, UQ_to_EQ_Vars_Out).

identify_uq_to_eq_vars(Frontier, UQ_to_EQ_Vars_In, UQ_to_EQ_Vars_Out) :- % Equalities
	goal_is_equality(Frontier, _Value_1, _Value_2, _EQV, UQV),
	varsbag(UQV, [], UQ_to_EQ_Vars_In, UQ_to_EQ_Vars_Out).

identify_uq_to_eq_vars(Frontier, UQ_to_EQ_Vars_In, UQ_to_EQ_Vars_Out) :- % Disequalities
	goal_is_disequality(Frontier, _Term1, _Term2, _EQV, UQV), !,
	varsbag(UQV, [], UQ_to_EQ_Vars_In, UQ_to_EQ_Vars_Out).

identify_uq_to_eq_vars(Frontier, UQ_to_EQ_Vars_In, UQ_to_EQ_Vars_Out) :- % Negations
	goal_is_negation(Frontier, UQV, _SubGoal, _Negation_Proposal), !,
	varsbag(UQV, [], UQ_to_EQ_Vars_In, UQ_to_EQ_Vars_Out).

identify_uq_to_eq_vars(Frontier, UQ_to_EQ_Vars_In, UQ_to_EQ_Vars_In) :- % Other subgoals
	goal_is_not_conj_disj_eq_diseq_dneg(Frontier), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% removes from E redundnant equalities and variables
remove_from_E_redundant_eqs_and_vars(Formulae_In, GoalVars, Formulae_Out) :- 
	frontier_E_IE_NIE_contents(Formulae_In, E_In, IE_In, NIE_In), 
	remove_from_E_redundant_vars(E_In, GoalVars, 'fail', Changes),
	remove_from_E_redundant_eqs(E_In, [], _Visited, E_Aux),
	frontier_E_IE_NIE_contents(Formulae_Aux, E_Aux, IE_In, NIE_In), 
	(
	    (   % If there are no changes then we have a fixed point.
		Changes == 'fail', % No redundant vars.
		E_In == E_Aux, !, % No redundant eqs.
		Formulae_Out = Formulae_Aux
	    )
	;
	    (   % Else, we need to iterate again
		remove_from_E_redundant_eqs_and_vars(Formulae_Aux, GoalVars, Formulae_Out)	 
	    )
	).


remove_from_E_redundant_eqs(E_In, Visited_In, Visited_Out, E_Out) :- 
	goal_is_conjunction(E_In, E_In_Left, E_In_Right), !,
	remove_from_E_redundant_eqs(E_In_Left, Visited_In, Visited_Out, E_Out_Left),
	remove_from_E_redundant_eqs(E_In_Right, Visited_In, Visited_Out, E_Out_Right),
	(
	    ( E_Out_Left == [], E_Out_Right == [], !, E_Out = [] )
	;
	    ( E_Out_Left == [], !, E_Out = E_Out_Right )
	;
	    ( E_Out_Right == [], !, E_Out = E_Out_Left )
	;
	    ( E_Out = (E_Out_Left, E_Out_Right) )
	).

remove_from_E_redundant_eqs(E_In, Visited_In, Visited_In, []) :- 
	goal_is_equality(E_In, _Value_1, _Value_2, _EQV, _UQV),
	memberchk(E_In, Visited_In), !. % Eq has been seen before. Redundant.

remove_from_E_redundant_eqs(E_In, Visited_In, Visited_In, []) :- 
	goal_is_equality(E_In, Value_1, Value_2, EQV, UQV),
	goal_is_equality(E_Tmp, Value_2, Value_1, EQV, UQV), % Args interchanged.
	memberchk(E_Tmp, Visited_In), !. % Eq has been seen before. Redundant.

remove_from_E_redundant_eqs(E_In, Visited_In, [ E_In | Visited_In ], E_In) :- 
	goal_is_equality(E_In, _Value_1, _Value_2, _EQV, _UQV).
		
remove_from_E_redundant_vars(E_In, GoalVars, Changes_In, Changes_Out) :- 
	goal_is_conjunction(E_In, E_In_Left, E_In_Right), !,
	remove_from_E_redundant_vars(E_In_Left, GoalVars, Changes_In, Changes_Aux),
	remove_from_E_redundant_vars(E_In_Right, GoalVars, Changes_Aux, Changes_Out),
	!.

remove_from_E_redundant_vars(E_In, GoalVars, Changes_In, Changes_Out) :- 
	goal_is_equality(E_In, Value_1, Value_2, _EQV, _UQV),
	remove_from_E_redundant_vars_aux(Value_1, Value_2, GoalVars, Changes_In, Changes_Out).

remove_from_E_redundant_vars_aux(Value_1, Value_2, GoalVars, _Changes_In, 'true') :-
	var(Value_1), 
	varsbag(GoalVars, [], [], Real_GoalVars), % Sometimes we have non vars in GoalVars.
	varsbag((Value_1, Value_2), Real_GoalVars, [], Non_GoalVars),
	(
	    (   % Value_1 is a var in Non_GoalVars
		memberchk(Value_1, Non_GoalVars), !,
		Value_1 = Value_2
	    )
	;
	    (   % Value_2 is a var in Non_GoalVars
		memberchk(Value_2, Non_GoalVars), !,
		Value_2 = Value_1
	    )
	).

remove_from_E_redundant_vars_aux(Value_1, Value_2, _GoalVars, Changes_In, Changes_In) :-
	(   var(Value_1) ; var(Value_2)   ),
	!, fail.

remove_from_E_redundant_vars_aux(Value_1, Value_2, GoalVars, Changes_In, Changes_Out) :-
	functor(Value_1, Name, Arity),
	functor(Value_2, Name, Arity), !,
	Value_1=..[Name|Args1],
	Value_2=..[Name|Args2],
	remove_from_E_redundant_vars_aux_list(Args1, Args2, GoalVars, Changes_In, Changes_Out).

remove_from_E_redundant_vars_aux(Value_1, Value_2, _GoalVars, Changes_In, Changes_In) :-
	functor(Value_1, Name1, Arity1),
	functor(Value_2, Name2, Arity2), 
	(
	    ( Name1 \== Name2) ;
	    ( Arity1 \== Arity2)
	).

remove_from_E_redundant_vars_aux_list([], [], _GoalVars, Changes_In, Changes_In) :- !.
remove_from_E_redundant_vars_aux_list([Arg1|Args1], [Arg2|Args2], GoalVars, Changes_In, Changes_Out) :-
	remove_from_E_redundant_vars_aux(Arg1, Arg2, GoalVars, Changes_In, Changes_Aux), 
	remove_from_E_redundant_vars_aux_list(Args1, Args2, GoalVars, Changes_Aux, Changes_Out).

% remove_from_IE_irrelevant_disequalities(Formula_In, Vars_Info, Formula_Out) :-
% returns IE_Out that is IE_In but without disequalities 
% whose variables are not in ImpVars nor in RelVars (So they are in Dumb_Vars).
% In practice we unify each var in Dumb_Vars to avoid a result after its negation.

remove_from_IE_irrelevant_disequalities(Formula_In, Vars_Info_In, Formula_Out) :-
%	Recompute.
%	vars_info_contents(Vars_Info, GoalVars, UQV, ImpVars, ExpVars, RelVars, UQ_to_EQ_Vars, Dumb_Vars).
	vars_info_contents(Vars_Info_In, GoalVars_In, UQV_In, _ImpVars_In, _ExpVars_In, _RelVars_In, _Dumb_Vars_In, _EQ_to_UQ_Vars_In, _UQ_to_EQ_Vars_In),

	compute_variables_information(Formula_In, GoalVars_In, UQV_In, Vars_Info),
	frontier_E_IE_NIE_contents(Formula_In, E_In, IE_In, NIE_In),
	remove_from_IE_irrelevant_disequalities_aux(IE_In, Vars_Info, IE_Out),
	frontier_E_IE_NIE_contents(Formula_Out, E_In, IE_Out, NIE_In). 

remove_from_IE_irrelevant_disequalities_aux([], _Vars_Info, []) :- !.
remove_from_IE_irrelevant_disequalities_aux(IE_In, Vars_Info, IE_Out) :-
	goal_is_conjunction(IE_In, IE_In_1, IE_In_2), !,
	remove_from_IE_irrelevant_disequalities_aux(IE_In_1, Vars_Info, IE_Out_1),
	remove_from_IE_irrelevant_disequalities_aux(IE_In_2, Vars_Info, IE_Out_2),
	rebuild_conjunction_of_goals(IE_Out_1, IE_Out_2, IE_Out).

remove_from_IE_irrelevant_disequalities_aux(IE_In, Vars_Info, IE_Out):-
	goal_is_disequality(IE_In, _Term1, _Term2, _EQV_IE_In, _UQV_IE_In), !,
	vars_info_contents(Vars_Info, _GoalVars, _UQV, _ImpVars, _ExpVars, _RelVars, Dumb_Vars, _EQ_to_UQ_Vars, _UQ_to_EQ_Vars),

	varsbag(IE_In, [], [], Vars_IE_In),
	varsbag_intersection(Vars_IE_In, Dumb_Vars, Problematic_Vars),
	(
	    (   % No problematic vars
		Problematic_Vars == [], !,
		IE_Out = IE_In 
	    )
	;
	    (   % At least one problematic var.
		Problematic_Vars \== [], !,
		IE_Out = [] 
	    )
	).
	
% split_IE_NIE_between_imp_exp_and_dumb(Frontier_In, ImpVars, ExpVars, UQ_to_EQ_Vars, Dumb_Vars, Frontier_Out)
% returns Frontier_Out that is the frontier divided betwen 
% ImpVars, ExpVars and UQ_Vars.
split_IE_NIE_between_imp_exp_and_dumb(Frontier_In, Vars_Info, Frontier_Out):-
	frontier_E_IE_NIE_contents(Frontier_In, E, IE, NIE),
	% echo_msg(2, '', 'cneg_rt', 'split_IE_NIE_between_imp_exp_and_dumb :: (E, IE, NIE)', (E, IE, NIE)),

	split_ie_or_nie_between_imp_exp_and_dumb(IE, Vars_Info, IE_Imp, IE_Exp, IE_Dumb),
	% echo_msg(2, '', 'cneg_rt', 'split_ie_or_nie_between_imp_exp_and_dumb :: (IE_Imp, IE_Exp)', (IE_Imp, IE_Exp, IE_Dumb)),

	split_ie_or_nie_between_imp_exp_and_dumb(NIE, Vars_Info, NIE_Imp, NIE_Exp, NIE_Dumb),
	% echo_msg(2, '', 'cneg_rt', 'split_ie_or_nie_between_imp_exp_and_dumb :: (NIE_Imp, NIE_Exp)', (NIE_Imp, NIE_Exp, NIE_Dumb)),

	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	frontier_E_IE_NIE_ied_contents(Frontier_Out, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).

% split_formula_between_imp_and_exp(F,ExpVars,Fimp,Fexp) divide F between Fimp and Fexp.
% In Fexp are the elements of F with any variables of ExpVars and
% the rest of elements of F will be in Fimp
split_ie_or_nie_between_imp_exp_and_dumb([], _Vars_Info, [], [], []) :- !. % Optimization.
split_ie_or_nie_between_imp_exp_and_dumb(Form, Vars_Info, Form_imp, Form_exp, Form_dumb) :-
	goal_is_conjunction(Form, Form_1, Form_2), !,
	split_ie_or_nie_between_imp_exp_and_dumb(Form_1, Vars_Info, Form_imp_1, Form_exp_1, Form_dumb_1),
	split_ie_or_nie_between_imp_exp_and_dumb(Form_2, Vars_Info, Form_imp_2, Form_exp_2, Form_dumb_2),
	rebuild_conjunction_of_goals(Form_imp_1, Form_imp_2, Form_imp),
	rebuild_conjunction_of_goals(Form_exp_1, Form_exp_2, Form_exp),
	rebuild_conjunction_of_goals(Form_dumb_1, Form_dumb_2, Form_dumb).

split_ie_or_nie_between_imp_exp_and_dumb(Form, _Vars_Info, _Form_imp, _Form_exp, _Form_dumb) :-
	goal_is_disjunction(Form, _Form_1, _Form_2), !,
	echo_msg(1, '', 'cneg_rt', 'ERROR: split_ie_or_nie_between_imp_exp_and_dumb can not deal with disjunctions. Form', Form),
	fail.

split_ie_or_nie_between_imp_exp_and_dumb(Form, Vars_Info, Form_imp, Form_exp, Form_dumb) :-
%	vars_info_contents(Vars_Info, GoalVars, UQV, ImpVars, ExpVars, RelVars, Dumb_Vars, EQ_to_UQ_Vars, UQ_to_EQ_Vars).
	vars_info_contents(Vars_Info, _GoalVars, _UQV, _ImpVars, ExpVars, _RelVars, Dumb_Vars, _EQ_to_UQ_Vars, _UQ_to_EQ_Vars),
	varsbag(Form, [], [], Form_Vars), 
	varsbag_intersection(Form_Vars, ExpVars, Form_ExpVars),
	varsbag_intersection(Form_Vars, Dumb_Vars, Form_Dumb_Vars),
	(
	    (   % Form has some Some ExpVars
		Form_ExpVars \== [], !,
		Form_exp = Form, Form_dumb = [], Form_imp = []
	    )
	;
	    (   % Form has no ExpVars but some DumbVars
		Form_ExpVars == [], Form_Dumb_Vars \== [], !,
		Form_exp = [], Form_dumb = Form, Form_imp = []
	    )
	;
	    (   % No ExpVars nor DumbVars
		ExpVars == [], Form_Dumb_Vars == [], !,
		Form_exp = [], Form_dumb = [], Form_imp = Form
	    )
	).
%	echo_msg(2, '', 'cneg_rt', 'split_ie_or_nie_between_imp_exp_and_dumb', (Form, Form_imp, Form_exp)).

% negate_formula(Frontier, Proposal, GoalVars, UQV, ImpVars, ExpVars, UQ_to_EQ_Vars, Dumb_Vars, Result)
% returns Result that is the result from negating the frontier.
negate_formula(Frontier, _Proposal, _Vars_Info, true) :- 
	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	frontier_E_IE_NIE_ied_contents(Frontier, [], [], [], [], [], [], []),
	!. % Optimization

negate_formula(Frontier, Proposal, Vars_Info, Neg_E_IE_NIE) :-
	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	frontier_E_IE_NIE_ied_contents(Frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, _NIE_Dumb),

	rebuild_conjunction_of_goals(IE_Exp, NIE_Exp, IE_NIE_Exp), 
	rebuild_conjunction_of_goals(IE_Imp, IE_Dumb, IE_Imp_Dumb), % IE_imp_dumb == [] in Chan proposal.

	rebuild_conjunction_of_goals(E, IE_Imp_Dumb, E_IE_Imp_Dumb),
	rebuild_conjunction_of_goals(E_IE_Imp_Dumb, NIE_Imp, Formulae_Imp),

	echo_msg(2, '', 'cneg_rt', 'negate_formula :: Formulae_Imp', Formulae_Imp),
	echo_msg(2, '', 'cneg_rt', 'negate_formula :: IE_NIE_Exp', IE_NIE_Exp),
 	negate_IE_NIE_exp(IE_NIE_Exp, Proposal, Vars_Info, Neg_IE_NIE_Exp),
	negate_imp_form(Formulae_Imp, Proposal, Vars_Info, Neg_IE_NIE_Exp, Neg_E_IE_NIE),
	!. % Backtracking forbidden.

% negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,SolC) obtain in
% SolC a solution of negating Dexp y Rexp juntos.
negate_IE_NIE_exp([], _Proposal, _Vars_Info, []):- !.
negate_IE_NIE_exp(IE_NIE_exp, Proposal, Vars_Info, Neg_IE_NIE_exp) :-
	IE_NIE_exp \== [],
	% vars_info_contents(Vars_Info, GoalVars, UQV, ImpVars, ExpVars, RelVars, Dumb_Vars, EQ_to_UQ_Vars, UQ_to_EQ_Vars).
	vars_info_contents(Vars_Info, _GoalVars, _UQV, ImpVars, _ExpVars, _RelVars, _Dumb_Vars, _EQ_to_UQ_Vars, UQ_to_EQ_Vars),

	varsbag(ImpVars, [], [], Real_ImpVars), % Sometimes we have non-vars (Chan method).
	varsbag_addition(Real_ImpVars, UQ_to_EQ_Vars, Non_UQV), % Non_UQV = vars(UQ_to_EQ) + vars(E) + GoalVars
	varsbag(IE_NIE_exp, Non_UQV, [], IE_NIE_Exp_UQV), % IE_NIE_Exp_UQV = vars(IE_NIE_Exp) - Non_UQV
	functor_local(Neg_IE_NIE_exp, Proposal, 2, [IE_NIE_Exp_UQV |[ IE_NIE_exp ]]), !.

negate_imp_form([], _Proposal, _Vars_Info, [], []) :- !. % Optimization.
negate_imp_form(Formula, _Proposal, _Vars_Info, _Next_Formula, _Neg_Formula) :-
	goal_is_disjunction(Formula, _Formula_1, _Formula_2), !,
	echo_msg(1, '', 'cneg_rt', 'ERROR: negate_imp_form can not deal with disjunctions. Formula', Formula),
	fail.

negate_imp_form(Formula, Proposal, Vars_Info, Next_Formula, Neg_Formula) :-
	goal_is_conjunction(Formula, Formula_1, Formula_2), !,
 	negate_imp_form(Formula_2, Proposal, Vars_Info, Next_Formula, Next_Formula_Aux),
 	negate_imp_form(Formula_1, Proposal, Vars_Info, Next_Formula_Aux, Neg_Formula).

negate_imp_form(Formula, Proposal, Vars_Info, Next_Formula, Neg_Formula) :-
	negate_imp_atom(Formula, Proposal, Vars_Info, Neg_Atom, Keep_Atom),
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
negate_imp_atom([], _Proposal, _Vars_Info, [], []) :- !. % Obvious.
negate_imp_atom(true, _Proposal, _Vars_Info, fail, true):- !. % Trivial predicates
negate_imp_atom(fail, _Proposal, _Vars_Info, true, fail):- !. % Trivial predicates

% Negation of equalities. We need to take care of converting UQV to EQV and vice-versa.
% Since EQV are not used in equality basic predicates, we assume that EQV variables
% are the result from applying double negation to a disequality. 
negate_imp_atom(Formula, _Proposal, Vars_Info, Neg_Atom, Keep_Atom) :-
	goal_is_equality(Formula, T1, T2, Eq_EQV, Eq_UQV), !,
%	vars_info_contents(Vars_Info, GoalVars, UQV, ImpVars, ExpVars, RelVars, Dumb_Vars, EQ_to_UQ_Vars, UQ_to_EQ_Vars).
	vars_info_contents(Vars_Info, GoalVars, _UQV, _ImpVars, _ExpVars, _RelVars, _Dumb_Vars, _EQ_to_UQ_Vars, UQ_to_EQ_Vars),

	varsbag_addition(GoalVars, UQ_to_EQ_Vars, Diseq_EQV), % Diseq_EQV = GoalVars + UQ_to_EQ_Vars
	varsbag(Formula, Diseq_EQV, [], Diseq_UQV), % Diseq_UQV = vars(Formula) - Diseq_EQV

	Neg_Atom = (diseq_euqv(T1, T2, Diseq_EQV, Diseq_UQV)),
	Keep_Atom = (eq_euqv(T1, T2, Eq_EQV, Eq_UQV)).

% Idem for disequalities.
negate_imp_atom(Formula, _Proposal, Vars_Info, Neg_Atom, Keep_Atom) :-
	goal_is_disequality(Formula, T1, T2, Diseq_EQV, Diseq_UQV), !,
%	vars_info_contents(Vars_Info, GoalVars, UQV, ImpVars, ExpVars, RelVars, Dumb_Vars, EQ_to_UQ_Vars, UQ_to_EQ_Vars, ).
	vars_info_contents(Vars_Info, _GoalVars, _UQV, ImpVars, _ExpVars, _RelVars, _Dumb_Vars, _EQ_to_UQ_Vars, UQ_to_EQ_Vars),

	varsbag_addition(ImpVars, UQ_to_EQ_Vars, Eq_EQV), % Eq_EQV = GoalVars + vars(E) + UQ_to_EQ_Vars
	varsbag(Formula, Eq_EQV, [], Eq_UQV), % Eq_UQV = vars(Formula) - Eq_EQV

	Neg_Atom = (eq_euqv(T1,T2, Eq_EQV, Eq_UQV)),
	Keep_Atom = (diseq_euqv(T1,T2, Diseq_EQV, Diseq_UQV)).

negate_imp_atom(Formula, Proposal, Vars_Info, Neg_Atom, Keep_Atom) :-
%	vars_info_contents(Vars_Info, GoalVars, UQV, ImpVars, ExpVars, RelVars, Dumb_Vars, EQ_to_UQ_Vars, UQ_to_EQ_Vars).
	vars_info_contents(Vars_Info, _GoalVars, _UQV, ImpVars, _ExpVars, _RelVars, _Dumb_Vars, _EQ_to_UQ_Vars, UQ_to_EQ_Vars),

	varsbag_addition(ImpVars, UQ_to_EQ_Vars, EQ_Vars), % EQ_Vars = GoalVars + vars(E) + UQ_to_EQ_Vars
 	varsbag(Formula, EQ_Vars, [], Delayed_Negation_UQV),

	functor_local(Neg_Atom, Proposal, 2, [Delayed_Negation_UQV |[ Formula ]]),
	Keep_Atom = (Formula). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


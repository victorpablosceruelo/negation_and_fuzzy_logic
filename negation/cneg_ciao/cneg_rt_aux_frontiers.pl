
:- module(cneg_rt_aux_frontiers, 
	[  compute_frontier/4, 
	   subfrontier_contents/5, 
	   subfrontier_E_IE_NIE_contents/4,
	   subfrontier_E_IE_NIE_ie_contents/6,
	   split_subfrontier_into_E_IE_NIE/2,
	   rebuild_conjunction_of_goals/3,
	   split_IE_NIE_between_imp_and_exp/3
	], [assertions]).

:- comment(title, "Contructive Negation Runtime Library - Chan's Proposal").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module implements predicates to deal with frontiers.").

:- use_module(cneg_aux, _).
:- use_module(cneg_diseq, [cneg_diseq_echo/4]).
:- use_module(cneg_rt_dynamic, [cneg_rt_dynamic/5]).
:- use_module(library(aggregates),[setof/3]).

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Structures to manage all the info about the subfrontier in an easy way.
subfrontier_contents(subfrontier(Goal, Head, Body, FrontierTest), Goal, Head, Body, FrontierTest).
subfrontier_E_IE_NIE_contents(subfrontier_E_IE_NIE(E, IE, NIE), E, IE, NIE).
subfrontier_E_IE_NIE_ie_contents(subfrontier_E_IE_NIE_ie(E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp), E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% compute_set_of_frontiers(Goal, Real_GoalVars, Proposal, Frontier)
compute_frontier(Goal, GoalVars, Proposal, Frontier) :-
	echo_msg(2, '', 'cneg_rt', 'compute_set_of_frontiers :: (Goal, GoalVars, Proposal)', (Goal, GoalVars, Proposal)),
	split_goal_with_disjunctions_into_goals(Goal, Proposal, Goals),
	!,
	echo_msg(2, 'list', 'cneg_rt', 'compute_set_of_frontiers :: Goals', Goals),
	compute_frontier_aux(Goals, GoalVars, Proposal, Frontier),
	!.

compute_frontier_aux([], _GoalVars, _Proposal, []) :- !.
compute_frontier_aux([Goal | More_Goals], GoalVars, Proposal, Frontier_Out) :-
	compute_goal_frontier(Goal, Proposal, Frontier_Aux), !,
%	echo_msg(2, 'list', 'cneg_rt', 'compute_set_of_frontiers_aux :: Frontier_Aux', Frontier_Aux),
	adequate_frontier(Frontier_Aux, GoalVars, Frontier_Tmp), !,
	compute_frontier_aux(More_Goals, GoalVars, Proposal, Frontier_In),
	append(Frontier_Tmp, Frontier_In, Frontier_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adequate_frontier([], _GoalVars, []) :- !.
adequate_frontier([F_In | Frontier_In], GoalVars, [F_Out | Frontier_Out]) :-
	adequate_frontier_aux(F_In, GoalVars, F_Out), !,
	adequate_frontier(Frontier_In, GoalVars, Frontier_Out).

adequate_frontier_aux(F_In, GoalVars, Body_Copy) :-
%	subfrontier_contents(Frontier, Goal, Head, Body, FrontierTest).
	subfrontier_contents(F_In, Real_Goal, Head, Body, _F_Test),
	copy_term((Head, Body), (Head_Copy, Body_Copy)), % Fresh copy to avoid variable clashes.
	varsbag(Real_Goal, GoalVars, [], Real_Goal_UQV), 
	varsbag(Real_Goal, Real_Goal_UQV, [], Real_Goal_GoalVars), % Determine affected goalvars.
	copy_term((Real_Goal, Real_Goal_GoalVars), (Real_Goal_Copy, Real_Goal_GoalVars_Copy)),
	Real_Goal_GoalVars = Real_Goal_GoalVars_Copy, % Unify goalvars, but not UQV.
	Real_Goal_Copy = Head_Copy, % Unify head and goal definitely
	!. % Backtracking is forbidden.

adequate_frontier_aux(F_In, GoalVars, _Body_Copy) :-
	echo_msg(1, '', 'cneg_rt', 'ERROR: adequate_frontier_aux(F_In, GoalVars)', (F_In, GoalVars)),
	!, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Just to debug.
%compute_goal_frontier(Goal, _Proposal, _Frontier) :-
%	echo_msg(2, '', 'cneg_rt', '--------------------------------------------------------------------------------------------------------------', ' '),
%	echo_msg(2, '', 'cneg_rt', 'compute_goal_frontier :: (Goal)', (Goal)),	
%	fail. % Just debug and use backtracking to continue.

% First remove $ and qualification from the goal's name.
compute_goal_frontier(Goal, Proposal, Frontier) :-
	goal_clean_up(Goal, Tmp_Goal), !,
	compute_goal_frontier(Tmp_Goal, Proposal, Frontier).

% Manage true and fail ...
compute_goal_frontier('true', _Proposal, [F_Out]) :- !,
%	subfrontier_contents(Frontier, Goal, Head, Body, FrontierTest).
	subfrontier_contents(F_Out, 'true', 'true', 'true', 'true').
compute_goal_frontier('fail', _Proposal, [F_Out]) :- !,
%	subfrontier_contents(Frontier, Goal, Head, Body, FrontierTest).
	subfrontier_contents(F_Out, 'fail', 'fail', 'fail', 'fail').

% Now go for the disjunctions.
% The frontiers need to evaluated one at a time. 
compute_goal_frontier(Goal, _Proposal, _Frontier_Out):- 
	goal_is_disjunction(Goal, _G1, _G2), !,
	echo_msg(1, '', 'cneg_rt', 'ERROR: Not possible computing the frontier for a disjunction', Goal), 
	echo_msg(1, 'nl', 'cneg_rt', '', ''), !, % Backtracking is forbidden.
	fail.

% Now go for the conjunctions.
compute_goal_frontier(Goal, Proposal, Frontier):- 
	goal_is_conjunction(Goal, G1, G2), !,
	compute_goal_frontier(G1, Proposal, Frontier_G1),
	compute_goal_frontier(G2, Proposal, Frontier_G2),
	!,
	combine_frontiers_from_conjunction(Frontier_G1, Frontier_G2, Frontier),
	!.

% Now go for the functors for equality and disequality.
% None of them is managed yet, so just bypass them.
compute_goal_frontier(Goal, _Proposal, [F_Out]) :- 
	goal_is_disequality(Goal, T1, T2, GV, EQV, UQV), !,
	functor_local(Real_Goal, 'diseq_geuqv', 5, [ T1 |[ T2 |[ GV |[ EQV |[ UQV ]]]]]),
%	subfrontier_contents(Frontier, Goal, Head, Body, FrontierTest).
	subfrontier_contents(F_Out, Real_Goal, Real_Goal, Real_Goal, Real_Goal),
	!.

compute_goal_frontier(Goal, _Proposal, [F_Out]) :- 
	goal_is_equality(Goal, T1, T2, GV, EQV, UQV), !,
	functor_local(Real_Goal, 'eq_geuqv', 5, [ T1 |[ T2 |[ GV |[ EQV |[ UQV ]]]]]),
%	subfrontier_contents(Frontier, Goal, Head, Body, FrontierTest).
	subfrontier_contents(F_Out, Real_Goal, Real_Goal, Real_Goal, Real_Goal), 
	!.

% Double negation is not managed yet. Bypass it.
%compute_goal_frontier(Goal, Proposal, Real_Goal, [F_Out]) :- 
compute_goal_frontier(Goal, Proposal, Frontier) :- 
	goal_is_cneg_rt(Goal, UQV, GoalVars, SubGoal, _Unused_Negation_Proposal), !,
	echo_msg(2, '', 'cneg_rt', 'compute_goal_frontier :: dn :: double negation for (Proposal, UQV, GoalVars, SubGoal)', (Proposal, UQV, GoalVars, SubGoal)),
	cneg_rt_dynamic(UQV, GoalVars, SubGoal, Proposal, Conj_List_Result), !,
	generate_conjunction_from_list(Conj_List_Result, Conj_Of_Disjs_Frontier), !,
	echo_msg(2, 'list', 'cneg_rt', 'compute_goal_frontier :: dn :: Conj_Of_Disjs_Frontier', Conj_Of_Disjs_Frontier),
	split_goal_with_disjunctions_into_goals(Conj_Of_Disjs_Frontier, 'cneg_rt_gv', List_Of_Conjs_Frontier), !,
	echo_msg(2, 'list', 'cneg_rt', 'compute_goal_frontier :: dn :: List_Of_Conjs_Frontier', List_Of_Conjs_Frontier),
	build_a_frontier_from_each_result(Goal, List_Of_Conjs_Frontier, Frontier), !,
	echo_msg(2, 'list', 'cneg_rt', 'double neg :: Frontier', Frontier),
	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	!.

% Now go for other functors stored in our database.
compute_goal_frontier(Goal, _Proposal, Frontier_Out) :-
	goal_is_not_conj_disj_eq_diseq_dneg(Goal),
	echo_msg(2, '', 'cneg_rt', 'compute_goal_frontier :: Goal', Goal),
	look_for_the_relevant_clauses(Goal, Frontier_Tmp_1),
%	echo_msg(0, '', 'cneg_rt', 'compute_neg_frontier :: format', '(Head, Body, FrontierTest)'),
%	echo_msg(2, 'list', 'cneg_rt', 'Frontier_Tmp_1', Frontier_Tmp_1),
	simplify_frontier(Frontier_Tmp_1, Goal, [], Frontier_Out),
	echo_msg(2, 'list', 'cneg_rt', 'compute_goal_frontier :: Frontier_Out', Frontier_Out), 
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	!. % Backtracking is forbidden.

% And at last report an error if it was impossible to found a valid entry.
compute_goal_frontier(Goal, _Proposal, []) :-
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
%	subfrontier_contents(Frontier, Goal, Head, Body, FrontierTest).
	subfrontier_contents(Frontier, Real_Goal, Real_Goal, Result, Real_Goal),
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
	setof(subfrontier(_Real_Goal, Head, Body, FrontierTest), 
	cneg_pre_frontier(Name, Arity, _SourceFileName, Head, Body, FrontierTest), Frontier).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% simplify_frontier(Front,Frontier) simplifies the frontier Front.
% Since the frontiers retrieved are in an inverted order, 
% we must reorder them to keep procedural semantics unchanged.
simplify_frontier([], _Goal, Frontier_Acc, Frontier_Acc) :- !.
%	echo_msg(2, 'nl', 'cneg_rt', '', '').
simplify_frontier([F_In | Frontier_In], Goal, Frontier_Acc, Frontier_Out) :-
	test_frontier_is_valid(F_In, Goal), !,
%	echo_msg(2, '', 'cneg_rt', 'simplify_frontier :: valid :: F_In', F_In),
	simplify_frontier(Frontier_In, Goal, [F_In | Frontier_Acc], Frontier_Out).
simplify_frontier([_F_In | Frontier_In], Goal, Frontier_Acc, Frontier_Out) :-
%	echo_msg(2, '', 'cneg_rt', 'simplify_frontier :: not valid :: F_In', F_In),
	simplify_frontier(Frontier_In, Goal, Frontier_Acc, Frontier_Out).

% simplify_frontier_unifying_variables(H, Body_In, G, Body_Out) 
% returns in Body_Out the elements of Body whose head unifies with G.
test_frontier_is_valid(F_In, Goal) :-
%	subfrontier_contents(Frontier, Goal, Head, Body, FrontierTest).
	subfrontier_contents(F_In, Goal, _Head, _Body, F_Test),
	copy_term((Goal, F_Test), (Goal_Tmp, F_Test_Tmp)),
	F_Test_Tmp = Goal_Tmp, % Test that test and goal can be unified. 
	echo_msg(2, '', 'cneg_rt', 'test_frontier_is_valid :: VALID :: (Goal, F_In)', (Goal, F_In)),
	!. % Backtracking forbidden. 
test_frontier_is_valid(F_In, Goal) :-
	echo_msg(2, '', 'cneg_rt', 'test_frontier_is_valid :: NOT VALID :: (Goal, F_In)', (Goal, F_In)),
	cneg_diseq_echo(2, '', 'cneg_rt', Goal),
	cneg_diseq_echo(2, '', 'cneg_rt', F_In),
	!, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% combine_frontiers(F1,F2,F3) returns F3 that is the resulting frontier
% from combining the frontiers F1 and F2 in all possible ways.
combine_frontiers_from_conjunction([],_F2,[]) :- !, % Optimization
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'combine_frontiers_from_conjunction', 'empty F1 -> empty F3'),
	echo_msg(2, 'nl', 'cneg_rt', '', '').
combine_frontiers_from_conjunction(_F1,[],[]) :- !, % Optimization
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'combine_frontiers_from_conjunction', 'empty F2 -> empty F3'),
	echo_msg(2, 'nl', 'cneg_rt', '', '').
combine_frontiers_from_conjunction(F1,F2,F3) :-
	F1 \== [], F2 \== [],
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'combine_frontiers_from_conjunction', 'F1 /\\ F2 -> F3 '),
	combine_frontiers_from_conjunction_aux_1(F1,F2,F3),
	echo_msg(2, '', 'cneg_rt', 'combine_frontiers_from_conjunction', 'F3'),
	echo_msg(2, 'list', 'cneg_rt', '', F3),
	echo_msg(2, 'nl', 'cneg_rt', '', '').
	
combine_frontiers_from_conjunction_aux_1([],_F2,[]) :- !.
combine_frontiers_from_conjunction_aux_1([F1_1 | More_F1], F2, F3):-
        combine_frontiers_from_conjunction_aux_2(F1_1, F2, F3_1),
        combine_frontiers_from_conjunction_aux_1(More_F1, F2, More_F3),
        cneg_aux:append(F3_1, More_F3, F3).

% combine_frontiers_aux_1(F1_1,F2, F3) returns F3 that is 
% the result of combining F1_1 with each element of F2.
combine_frontiers_from_conjunction_aux_2(_F1_1, [], []).
combine_frontiers_from_conjunction_aux_2(F1_1, [F2_1 | More_F2], [F3 | More_F3]) :-
%	subfrontier_contents(Frontier, Goal, Head, Body, FrontierTest).
	subfrontier_contents(F1_1, F1_1_Real_Goal, F1_1_Head, F1_1_Body, F1_1_F_Test),
	subfrontier_contents(F2_1, F2_1_Real_Goal, F2_1_Head, F2_1_Body, F2_1_F_Test),
	F3_Real_Goal = ((F1_1_Real_Goal), (F2_1_Real_Goal)),
	F3_Head = ((F1_1_Head), (F2_1_Head)),
	F3_Body = ((F1_1_Body), (F2_1_Body)),
	F3_F_Test = ((F1_1_F_Test), (F2_1_F_Test)),
	subfrontier_contents(F3, F3_Real_Goal, F3_Head, F3_Body, F3_F_Test),
	test_frontier_is_valid(F3, F3_Real_Goal), !, 
        combine_frontiers_from_conjunction_aux_2(F1_1, More_F2, More_F3).
combine_frontiers_from_conjunction_aux_2(F1_1, [_F2_1 | More_F2], More_F3) :-
	combine_frontiers_from_conjunction_aux_2(F1_1, More_F2, More_F3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_subfrontier_into_E_IE_NIE(Frontier_In, _Frontier_Out) :-
	goal_is_disjunction(Frontier_In, _G1, _G2), !, 
	echo_msg(1, '', 'cneg_rt', 'ERROR: split_subfrontier_into_E_IE_NIE can not deal with disjunctions. Frontier_In', Frontier_In),
	fail.

split_subfrontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :-
	goal_is_conjunction(Frontier_In, G1, G2), !,
	split_subfrontier_into_E_IE_NIE(G1, Frontier_G1),
	split_subfrontier_into_E_IE_NIE(G2, Frontier_G2),
	subfrontier_E_IE_NIE_contents(Frontier_G1, E_G1, IE_G1, NIE_G1),
	subfrontier_E_IE_NIE_contents(Frontier_G2, E_G2, IE_G2, NIE_G2),
	rebuild_conjunction_of_goals(E_G1, E_G2, E_Out),
	rebuild_conjunction_of_goals(IE_G1, IE_G2, IE_Out),
	rebuild_conjunction_of_goals(NIE_G1, NIE_G2, NIE_Out),
	subfrontier_E_IE_NIE_contents(Frontier_Out, E_Out, IE_Out, NIE_Out).

split_subfrontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_equality(Frontier_In, _Term1, _Term2, _GV, _EQV, _UQV), !,
	subfrontier_E_IE_NIE_contents(Frontier_Out, Frontier_In, [], []).

split_subfrontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_disequality(Frontier_In, _Term1, _Term2, _GV, _EQV, _UQV), !,
	subfrontier_E_IE_NIE_contents(Frontier_Out, [], Frontier_In, []).

% This leads to infinite loops because double negation 
% sould be managed when generating the subfrontier.
% The way to fix this is remove cneg(cneg(...))
% when evaluating the subfrontier. To be done.
split_subfrontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_negation(Frontier_In, _GoalVars, _UQV, _SubGoal, _Negation_Proposal), !,
	subfrontier_E_IE_NIE_contents(Frontier_Out, [], [], Frontier_In).

split_subfrontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_not_conj_disj_eq_diseq_dneg(Frontier_In), !,
	subfrontier_E_IE_NIE_contents(Frontier_Out, [], [], Frontier_In).

split_subfrontier_into_E_IE_NIE(Frontier_In, _Frontier_Out) :- 
	echo_msg(1, '', 'cneg_rt', 'ERROR: split_subfrontier_into_E_IE_NIE can not deal with subfrontier. Frontier_In', Frontier_In),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rebuild_conjunction_of_goals([], [], []) :- !. % Empty elements when re-joining
rebuild_conjunction_of_goals(Goals, [], Goals) :- !. % Empty element when re-joining
rebuild_conjunction_of_goals([], Goals, Goals) :- !. % Empty element when re-joining
rebuild_conjunction_of_goals(Goals_1, Goals_2, (Goals_1, Goals_2)) :- % Non-empty elements.
	Goals_1 \== [], 
	Goals_2 \== [], !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% split_IE_NIE_between_imp_and_exp(Frontier_In, ImpVars, ExpVars, UQ_to_EQ_Vars, Dumb_Vars, Frontier_Out)
% returns Frontier_Out that is the frontier divided betwen 
% ImpVars, ExpVars and UQ_Vars.
split_IE_NIE_between_imp_and_exp(Frontier_In, ExpVars, Frontier_Out):-
	subfrontier_E_IE_NIE_contents(Frontier_In, E, IE, NIE),
	echo_msg(2, '', 'cneg_rt', 'split_IE_NIE_between_imp_and_expw :: (E, IE, NIE)', (E, IE, NIE)),
	echo_msg(2, '', 'cneg_rt', 'split_IE_NIE_between_imp_and_expw :: ExpVars', ExpVars),

	split_ie_or_nie_between_imp_and_exp(IE, ExpVars, IE_Imp, IE_Exp),
	echo_msg(2, '', 'cneg_rt', 'split_ie_or_nie_between_imp_and_exp :: (IE_Imp, IE_Exp)', (IE_Imp, IE_Exp)),

	split_ie_or_nie_between_imp_and_exp(NIE, ExpVars, NIE_Imp, NIE_Exp),
	echo_msg(2, '', 'cneg_rt', 'split_ie_or_nie_between_imp_and_exp :: (NIE_Imp, NIE_Exp)', (NIE_Imp, NIE_Exp)),

	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	subfrontier_E_IE_NIE_ie_contents(Frontier_Out, E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp).

% split_formula_between_imp_and_exp(F,ExpVars,Fimp,Fexp) divide F between Fimp and Fexp.
% In Fexp are the elements of F with any variables of ExpVars and
% the rest of elements of F will be in Fimp
split_ie_or_nie_between_imp_and_exp([], _ExpVars, [], []) :- !. % Optimization.
split_ie_or_nie_between_imp_and_exp(Form, ExpVars, Form_imp, Form_exp) :-
	goal_is_conjunction(Form, Form_1, Form_2), !,
	split_ie_or_nie_between_imp_and_exp(Form_1, ExpVars, Form_imp_1, Form_exp_1),
	split_ie_or_nie_between_imp_and_exp(Form_2, ExpVars, Form_imp_2, Form_exp_2),
	rebuild_conjunction_of_goals(Form_imp_1, Form_imp_2, Form_imp),
	rebuild_conjunction_of_goals(Form_exp_1, Form_exp_2, Form_exp).

split_ie_or_nie_between_imp_and_exp(Form, _ExpVars, _Form_imp, _Form_exp) :-
	goal_is_disjunction(Form, _Form_1, _Form_2), !,
	echo_msg(1, '', 'cneg_rt', 'ERROR: split_ie_or_nie_between_imp_and_exp can not deal with disjunctions. Form', Form),
	fail.

split_ie_or_nie_between_imp_and_exp(Form, ExpVars, Form_imp, Form_exp) :-
	varsbag(Form, [], [], Form_Vars), 
	varsbag_intersection(Form_Vars, ExpVars, Intersection),
	(
	    (   % Form has some ExpVars
		Intersection \== [], !,
		Form_exp = Form, Form_imp = []
	    )
	;
	    (   % Form has no ExpVars 
		Intersection == [], !,
		Form_exp = [], Form_imp = Form
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(cneg_frontier, [compute_frontier/3, subfrontier_E_IE_NIE_contents/4, subfrontier_E_IE_NIE_ie_contents/6], [assertions]).

:- comment(title, "Contructive Negation Runtime Library - Frontiers generation.").
:- comment(author, "V@'{i}ctor Pablos Ceruelo").
:- comment(summary, "This module implements negation predicates for runtime evaluation.").

:- use_module(library(aggregates),[setof/3]).
:- use_module(library('cneg/cneg_aux')).
% :- reexport(library('cneg/cneg_aux')).
:- use_module(library('cneg/cneg_diseq')).
% :- use_module(library('cneg/cneg_rt')).
% :- reexport(cneg_diseq).

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/9.
% To evaluate predicates only from the top package.
:- multifile call_to_predicate/1.
:- multifile file_debug_is_activated/1.

%:- push_prolog_flag(unused_pred_warnings, no).
%:- meta_predicate pre_frontier_node_to_frontier_node_aux_aux(?, ?, ?, goal, ?).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	 

% Structures to manage all the info about the subfrontier in an easy way.
preFrontierNodeContents(preFrontierNode(Real_Goal, Clean_Head, E, IE, NIE, Head, Body),
	Real_Goal, Clean_Head, E, IE, NIE, Head, Body).
subfrontier_E_IE_NIE_contents(subfrontier_E_IE_NIE(E, IE, NIE), E, IE, NIE).
subfrontier_E_IE_NIE_ie_contents(subfrontier_E_IE_NIE_ie(E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp), E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% compute_set_of_frontiers(Goal, Real_GoalVars, Proposal, Frontier)
compute_frontier(Goal, GoalVars_In, Frontier) :-
	varsbag(GoalVars_In, [], [], GoalVars), % Clean up non-vars in GoalVars.
	print_msg(3, 3, '', 'compute_set_of_frontiers :: (Goal, GoalVars)', (Goal, GoalVars)),
	split_body_with_disjunctions_into_bodies(Goal, Goals),
	!,
	print_msg(3, 3, 'list', 'compute_set_of_frontiers :: Goals', Goals),
	compute_frontier_aux(Goals, GoalVars, [], Frontier),
	!.

compute_frontier_aux([], _GoalVars, Frontier_Out, Frontier_Out) :- !.
compute_frontier_aux([Goal | More_Goals], GoalVars, Frontier_In, Frontier_Out) :-
	print_msg(3, 3, '', 'compute_goal_pre_frontier :: Goal', Goal),	
	compute_goal_pre_frontier(Goal, Goal_PreFrontier), !,
	print_msg(3, 3, '', 'compute_goal_pre_frontier :: Goal_PreFrontier', Goal_PreFrontier),	
	pre_frontier_to_frontier(Goal_PreFrontier, GoalVars, [], Frontier_Tmp), !,
	print_msg(3, 3, '', 'compute_goal_pre_frontier :: Frontier_Tmp', Frontier_Tmp),	
	append(Frontier_In, Frontier_Tmp, Frontier_Aux), !,
	compute_frontier_aux(More_Goals, GoalVars, Frontier_Aux, Frontier_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% compute_goal_pre_frontier(Goal, Frontier)
% This predicate is in charge of expansion of goals that are not equalities or disequalities,
% but without any kind of filter: we do not check yet if the resultant frontier is valid or not.

% First remove $ and qualification from the goal's name.
compute_goal_pre_frontier(Goal, Frontier) :-
	goal_clean_up(Goal, Tmp_Goal), !,
	compute_goal_pre_frontier(Tmp_Goal, Frontier).

% Manage true and fail ...
compute_goal_pre_frontier('true', [F_Out]) :- !,
%	preFrontierNodeContents(PreFrontier, Real_Goal, Clean_Head, E, IE, NIE, Head, Body).
	preFrontierNodeContents(F_Out, 'true', 'true', 'true', 'true', 'true', 'true', 'true').
compute_goal_pre_frontier('fail', [F_Out]) :- !,
%	preFrontierNodeContents(PreFrontier, Real_Goal, Clean_Head, E, IE, NIE, Head, Body).
	preFrontierNodeContents(F_Out, 'fail', 'fail', 'fail', 'fail', 'fail', 'fail', 'fail').

% Now go for the disjunctions.
% The frontiers need to evaluate one goal at a time. ERROR
compute_goal_pre_frontier(Goal, _Frontier_Out):- 
	goal_is_disjunction(Goal, _G1, _G2), !,
	print_msg(1, 3, '', 'ERROR: Not possible computing the frontier for a disjunction', Goal), 
	print_msg(1, 3, 'nl', '', ''), !, % Backtracking is forbidden.
	fail.

% Now go for the conjunctions.
compute_goal_pre_frontier(Goal, Frontier):- 
	goal_is_conjunction(Goal, G1, G2), !,
	compute_goal_pre_frontier(G1, Frontier_G1),
	compute_goal_pre_frontier(G2, Frontier_G2),
	!,
	rebuild_prefrontier_conjunction(Frontier_G1, Frontier_G2, Frontier),
	!.

% Now go for the functors for equality and disequality.
% None of them is managed yet, so just bypass them.
compute_goal_pre_frontier(Goal, [F_Out]) :- 
	goal_is_disequality(Goal, T1, T2, GV, EQV, UQV), !,
	functor_local(Real_Goal, 'diseq_geuqv', 5, [ T1 |[ T2 |[ GV |[ EQV |[ UQV ]]]]]),
%	preFrontierNodeContents(PreFrontier, Real_Goal, Clean_Head, E, IE, NIE, Head, Body).
	preFrontierNodeContents(F_Out, Real_Goal, Real_Goal, 'true', Real_Goal, 'true', Real_Goal, Real_Goal),
	!.

compute_goal_pre_frontier(Goal, [F_Out]) :- 
	goal_is_equality(Goal, T1, T2, GV, EQV, UQV), !,
	functor_local(Real_Goal, 'eq_geuqv', 5, [ T1 |[ T2 |[ GV |[ EQV |[ UQV ]]]]]),
%	preFrontierNodeContents(PreFrontier, Real_Goal, Clean_Head, E, IE, NIE, Head, Body).
	preFrontierNodeContents(F_Out, Real_Goal, Real_Goal, Real_Goal, 'true', 'true', Real_Goal, Real_Goal),
	!.

% Double negation is not managed yet. Bypass it.
%compute_goal_pre_frontier(Goal, Proposal, Real_Goal, [F_Out]) :- 
compute_goal_pre_frontier(Goal, PreFrontier) :- 
	goal_is_negation(Goal, GoalVars, SubGoal), !,
	functor_local(Real_Goal, 'cneg_aux', 2, [ SubGoal |[ GoalVars ]]),
	print_msg(3, 3, '', 'compute_goal_pre_frontier :: dn :: double negation for (SubGoal, GoalVars)', (SubGoal, GoalVars)),
%     cneg_rt(Goal, GoalVars, Depth_Level, Trace) :-
	% copy_term(SubGoal, SubGoal_Copy), 
	call_to_predicate(cneg_rt_aux(SubGoal, GoalVars, Negated_PreFr)), !,
	print_msg(3, 3, 'list', 'compute_goal_pre_frontier :: dn :: Negated_PreFr', Negated_PreFr),
	convert_negPreFr_to_preFr(Real_Goal, Negated_PreFr, PreFrontier).

% Now go for other functors stored in our database.
compute_goal_pre_frontier(Goal, Frontier_Out) :-
	goal_is_not_conj_disj_eq_diseq_dneg(Goal),
	print_msg(3, 3, '', 'compute_goal_pre_frontier :: Goal', Goal),
	look_for_the_relevant_clauses(Goal, Frontier_Tmp_1),
	print_msg(3, 3, 'list', 'Frontier_Tmp_1', Frontier_Tmp_1),
	simplify_and_reorder_goal_prefrontier(Frontier_Tmp_1, Goal, [], Frontier_Out),
	print_msg(3, 3, 'list', 'compute_goal_pre_frontier :: Frontier_Out', Frontier_Out), 
	print_msg(3, 3, 'nl', '', ''),
	!. % Backtracking is forbidden.

% And at last report an error if it was impossible to found a valid entry.
compute_goal_pre_frontier(Goal, []) :-
	print_msg(1, 3, '', 'ERROR: Not found frontier for Goal', Goal), 
	print_msg(1, 3, 'nl', '', ''), !. % Backtracking is forbidden.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Look for all the predicate definitions for the predicate with the name and arity of the one we want.
% We use its name and arity to avoid problematic unifications.
look_for_the_relevant_clauses(Goal, PreFrontierNodes) :-
	functor(Goal, Head_Name, Head_Arity),  % Security
	Head_Name \== ',', Head_Name \== ';',    % Issues
%	!, % Backtracking forbiden.
	print_msg(1, 3, '', 'look_for_the_relevant_clauses :: (Head_Name, Head_Arity)', (Head_Name, Head_Arity)), 
	cneg_pre_frontier(Head_Name, Head_Arity, _SourceFileName_, _Clean_Head_, _E_, _IE_, _NIE_, _Head_, _Body_), !,
	print_msg(1, 3, '', 'look_for_the_relevant_clauses :: (Head_Name, Head_Arity)', (Head_Name, Head_Arity)), 
	setof(PFN, 
	(
	    cneg_pre_frontier(Head_Name, Head_Arity, _SourceFileName, Clean_Head, E, IE, NIE, Head, Body),
	    preFrontierNodeContents(PFN, _Real_Goal, Clean_Head, E, IE, NIE, Head, Body)
	),
	PreFrontierNodes), 
	!.
look_for_the_relevant_clauses(_Goal, _PreFrontier) :-
	print_msg(1, 3, '', 'look_for_the_relevant_clauses :: ', 
	'cneg_pre_frontier(Head_Name, Head_Arity, SourceFileName, Clean_Head, E, IE, NIE, Head, Body)'), 
	cneg_pre_frontier(Name, Arity, SourceFileName, Clean_Head, E, IE, NIE, Head, Body), 
	print_msg(1, 3, '', '', cneg_pre_frontier(Name, Arity, SourceFileName, Clean_Head, E, IE, NIE, Head, Body)), 
	fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% simplify_and_reorder_goal_prefrontier(Front,PreFr) simplifies the frontier Front.
% Since the frontiers retrieved are in an inverted order, 
% we must reorder them to keep procedural semantics unchanged.
simplify_and_reorder_goal_prefrontier([], _Real_Goal, PreFr_Acc, PreFr_Acc) :- !.
%	print_msg(3, 3, 'nl', '', '').
simplify_and_reorder_goal_prefrontier([PreFr | PreFr_In], Real_Goal, PreFr_Acc, PreFr_Out) :-
%	preFrontierNodeContents(PreFr, Real_Goal, Clean_Head, E, IE, NIE, Head, Body).
	preFrontierNodeContents(PreFr, Real_Goal, _Clean_Head, _E, _IE, _NIE, _Head, _Body),
	test_pre_frontier_validity(PreFr), !,
%	print_msg(3, 3, '', 'simplify_and_reorder_goal_prefrontier :: valid :: PreFr', PreFr),
	simplify_and_reorder_goal_prefrontier(PreFr_In, Real_Goal, [PreFr | PreFr_Acc], PreFr_Out).
simplify_and_reorder_goal_prefrontier([_PreFr | PreFr_In], Real_Goal, PreFr_Acc, PreFr_Out) :- !,
%	print_msg(3, 3, '', 'simplify_and_reorder_goal_prefrontier :: not valid :: PreFr', PreFr),
	simplify_and_reorder_goal_prefrontier(PreFr_In, Real_Goal, PreFr_Acc, PreFr_Out).

% simplify_and_reorder_goal_prefrontier_unifying_variables(H, Body_In, G, Body_Out) 
% returns in Body_Out the elements of Body whose head unifies with G.
test_pre_frontier_validity(PreFr) :-
	% We make copies to avoid unifications.
	copy_term(PreFr, PreFr_Copy),

%	preFrontierNodeContents(PreFr, Real_Goal, Clean_Head, E, IE, NIE, Head, Body).
	preFrontierNodeContents(PreFr_Copy, Real_Goal, Clean_Head, E, IE, _NIE, _Head, _Body),

	Clean_Head = Real_Goal, % Unify heads.
	(
	    (
		call_to_predicate((E, IE)), !, 
		print_msg(3, 3, '', 'test_pre_frontier_validity :: VALID :: Goal', Goal),
		print_vars_diseqs(3, '', Goal), 
		print_msg(3, 3, 'nl', '', ''),
		print_msg(3, 3, '', 'test_pre_frontier_validity :: VALID :: PreFr', PreFr),
		print_vars_diseqs(3, '', PreFr), 
		print_msg(3, 3, 'nl', '', ''),
		! % Backtracking forbidden.
	    )
	;
	    (
		print_msg(3, 3, '', 'test_pre_frontier_validity :: NOT VALID :: Goal', Goal),
		print_vars_diseqs(3, '', Goal), 
		print_msg(3, 3, 'nl', '', ''),
		print_msg(3, 3, '', 'test_pre_frontier_validity :: NOT VALID :: PreFr', PreFr),
		print_vars_diseqs(3, '', PreFr), 
		print_msg(3, 3, 'nl', '', ''),
		!, fail
	    )
	), !. % Backtracking forbidden. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert_negPreFr_to_preFr(Goal, Negated_PreFr, PreFrontier) :-
	generate_conjunction_from_list(Negated_PreFr, Conj_Of_Disjs_PreFr), !,
	print_msg(3, 3, 'list', 'compute_goal_pre_frontier :: dn :: Conj_Of_Disjs_PreFr', Conj_Of_Disjs_PreFr),
	split_body_with_disjunctions_into_bodies(Conj_Of_Disjs_PreFr, List_Of_Conjs_PreFr), !,
	print_msg(3, 3, 'list', 'compute_goal_pre_frontier :: dn :: List_Of_Conjs_PreFr', List_Of_Conjs_PreFr),
	split_bodies_into_E_IE_NIE(List_Of_Conjs_PreFr, Split_Bodies),
	print_msg(3, 3, 'list', 'compute_goal_pre_frontier :: dn :: Split_Bodies', Split_Bodies),
	split_bodies_to_prefrontier(Split_Bodies, Goal, PreFrontier), !,
	print_msg(3, 3, 'list', 'double neg :: PreFrontier', PreFrontier),
	print_msg(3, 3, 'separation', '', ''),
	print_msg(3, 3, 'nl', '', ''),
	!.

% split_bodies_to_prefrontier([([E], [IE], [NIE], [Body]) | Bodies], Real_Goal, [PreFr | PreFrs])
split_bodies_to_prefrontier([], _Real_Goal, []) :- !.
split_bodies_to_prefrontier([([E], [IE], [NIE], [Body]) | Split_Bodies], Real_Goal, [PreFr | PreFrs]) :-
%	preFrontierNodeContents(PreFr, Real_Goal, Clean_Head, E, IE, NIE, Head, Body).
	preFrontierNodeContents(PreFr, Real_Goal, Real_Goal, E, IE, NIE, Real_Goal, Body),
	% copy_term(PreFr_Aux, PreFrs), % Different PreFr must have different variables.
	split_bodies_to_prefrontier(Split_Bodies, Real_Goal, PreFrs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% rebuild_prefrontier_conjunction(F1,F2,F3) returns F3 that is the resulting frontier
% from combining the frontiers F1 and F2 in all possible ways.
rebuild_prefrontier_conjunction([], _PreFr_2, []) :- !, % Optimization
	print_msg(3, 3, 'nl', '', ''),
	print_msg(3, 3, '', 'rebuild_prefrontier_conjunction', 'empty PreFr_1 -> empty PreFr_3'),
	print_msg(3, 3, 'nl', '', '').
rebuild_prefrontier_conjunction(_PreFr_1, [], []) :- !, % Optimization
	print_msg(3, 3, 'nl', '', ''),
	print_msg(3, 3, '', 'rebuild_prefrontier_conjunction', 'empty PreFr_2 -> empty PreFr_3'),
	print_msg(3, 3, 'nl', '', '').
rebuild_prefrontier_conjunction(PreFr_1, PreFr_2, PreFr_3) :-
	PreFr_1 \== [], PreFr_2 \== [],
	print_msg(3, 3, 'nl', '', ''),
	print_msg(3, 3, '', 'rebuild_prefrontier_conjunction', 'PreFr_1 /\\ PreFr_2 -> PreFr_3 '),
	rebuild_prefrontier_conjunction_aux_1(PreFr_1, PreFr_2, PreFr_3),
	print_msg(3, 3, '', 'rebuild_prefrontier_conjunction', 'PreFr_3'),
	print_msg(3, 3, 'list', '', PreFr_3),
	print_msg(3, 3, 'nl', '', '').
	
rebuild_prefrontier_conjunction_aux_1([], _PreFr_2, []) :- !.
rebuild_prefrontier_conjunction_aux_1([PreFr_1_1 | More_PreFr_1], PreFr_2, PreFr_3):-
        rebuild_prefrontier_conjunction_aux_2(PreFr_1_1, PreFr_2, PreFr_3_1),
        rebuild_prefrontier_conjunction_aux_1(More_PreFr_1, PreFr_2, More_PreFr_3),
        cneg_aux:append(PreFr_3_1, More_PreFr_3, PreFr_3).

% combine_frontiers_aux_1(PreFr_1_1,PreFr_2, PreFr_3) returns PreFr_3 that is 
% the result of combining PreFr_1_1 with each element of PreFr_2.
rebuild_prefrontier_conjunction_aux_2(_PreFr_1, [], []).
rebuild_prefrontier_conjunction_aux_2(PreFr_1, [PreFr_2 | More_PreFr_2], [PreFr_3 | More_PreFr_3]) :-
%	preFrontierNodeContents(PreFr, Real_Goal, Clean_Head, E, IE, NIE, Head, Body).
	preFrontierNodeContents(PreFr_1, PF1_RG, PF1_CH, PF1_E, PF1_IE, PF1_NIE, PF1_Head, PF1_Body),
	preFrontierNodeContents(PreFr_2, PF2_RG, PF2_CH, PF2_E, PF2_IE, PF2_NIE, PF2_Head, PF2_Body),

	PF3_RG = ((PF1_RG), (PF2_RG)),
	PF3_CH = ((PF1_CH), (PF2_CH)),
	PF3_E = ((PF1_E), (PF2_E)),
	PF3_IE = ((PF1_IE), (PF2_IE)),
	PF3_NIE = ((PF1_NIE), (PF2_NIE)),
	PF3_Head = ((PF1_Head), (PF2_Head)),
	PF3_Body = ((PF1_Body), (PF2_Body)),

	preFrontierNodeContents(PreFr_3, PF3_RG, PF3_CH, PF3_E, PF3_IE, PF3_NIE, PF3_Head, PF3_Body),
	test_pre_frontier_validity(PreFr_3), !, 
        rebuild_prefrontier_conjunction_aux_2(PreFr_1, More_PreFr_2, More_PreFr_3).
rebuild_prefrontier_conjunction_aux_2(PreFr_1, [_PreFr_2 | More_PreFr_2], More_PreFr_3) :-
	rebuild_prefrontier_conjunction_aux_2(PreFr_1, More_PreFr_2, More_PreFr_3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pre_frontier_to_frontier(Goal_PreFrontier, GoalVars, Frontier_In, Frontier_Out) :-
	print_msg(3, 3, 'list', 'pre_frontier_to_frontier :: Frontier_In', Frontier_In),
	pre_frontier_to_frontier_aux(Goal_PreFrontier, GoalVars, Frontier_In, Frontier_Out), !,
	print_msg(3, 3, 'list', 'pre_frontier_to_frontier :: Frontier_Out', Frontier_Out).

pre_frontier_to_frontier(Goal_PreFrontier, GoalVars, Frontier_In, _Frontier_Out) :-
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, 'list', 'ERROR: pre_frontier_to_frontier :: Goal_PreFrontier', Goal_PreFrontier),
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, '', 'ERROR: pre_frontier_to_frontier :: GoalVars', GoalVars),
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, 'list', 'ERROR: pre_frontier_to_frontier :: Frontier_In', Frontier_In),
	print_msg(1, 3, 'nl', '', ''),
	!, fail.

pre_frontier_to_frontier_aux([], _GoalVars, Frontier_Out, Frontier_Out) :- !.
pre_frontier_to_frontier_aux([Goal_PreFrontier_Node | Goal_PreFrontier], GoalVars, Frontier_In, Frontier_Out) :-
	pre_frontier_node_to_frontier_node(Goal_PreFrontier_Node, GoalVars, [], Goal_Frontier_Nodes), !,
	print_msg(3, 3, '', 'pre_frontier_to_frontier_aux :: Goal_Frontier_Nodes', Goal_Frontier_Nodes),
	append(Frontier_In, Goal_Frontier_Nodes, Frontier_Aux), !,
	pre_frontier_to_frontier_aux(Goal_PreFrontier, GoalVars, Frontier_Aux, Frontier_Out).

%
% This is one of the most complicated predicates.
% It has to  
% (1) do a copy of the prefrontier and its real goal,
% (2) retrieve the current status of the goals (the goalvars, the localvars and the constraints),
% (3) unify the clean_head with the real_goal, 
% (4) evaluate (E, IE),
% (5) retrieve the current status of the goals (the goalvars, the localvars and the constraints),
% (6) decide which unifications and constraints have been generated in step (4).
%
% :- meta_predicate pre_frontier_node_to_frontier_node(goal, ?, ?, ?).
pre_frontier_node_to_frontier_node(Goal_PreFrontier_Node, GoalVars_In, Frontier_In, Frontier_Out) :-
%	preFrontierNodeContents(Goal_PreFrontier_Node, Real_Goal, Clean_Head, E, IE, NIE, Head, Body).
	preFrontierNodeContents(Goal_PreFrontier_Node, Real_Goal, Clean_Head, E, IE, NIE, Head, Body),
	unify_real_goal_and_clean_head(Real_Goal, Clean_Head),

	varsbag(GoalVars_In, [], [], GoalVars),
	print_msg(3, 3, '', 'pre_frontier_node_to_frontier_node :: GoalVars', GoalVars),
	varsbag((E, IE, NIE), GoalVars, [], LocalVars),

	print_msg(3, 3, '', 'pre_frontier_node_to_frontier_node :: (Head, Body)', (Head, Body)),
	print_msg(3, 3, '', 'pre_frontier_node_to_frontier_node :: Clean_Head', Clean_Head),
	print_msg(3, 3, '', 'pre_frontier_node_to_frontier_node :: E', E),
	print_msg(3, 3, '', 'pre_frontier_node_to_frontier_node :: IE', IE),
	print_msg(3, 3, '', 'pre_frontier_node_to_frontier_node :: NIE', NIE),
	print_msg(3, 3, '', 'pre_frontier_node_to_frontier_node :: GoalVars', GoalVars),
	print_msg(3, 3, '', 'pre_frontier_node_to_frontier_node :: LocalVars', LocalVars),

	get_disequalities_from_constraints(Real_Goal, RG_Constraints),
	print_msg(3, 3, '', 'pre_frontier_node_to_frontier_node :: RG_Constraints', RG_Constraints),

	get_frontier_from_pre_frontier(E, IE, NIE, GoalVars, LocalVars, RG_Constraints, Frontier_Nodes),
	print_msg(3, 3, '', 'pre_frontier_node_to_frontier_node :: Frontier_Nodes', Frontier_Nodes),
	append(Frontier_In, Frontier_Nodes, Frontier_Out). % Is order really relevant ???

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unify_real_goal_and_clean_head(Real_Goal, Clean_Head) :- Real_Goal = Clean_Head, !.
unify_real_goal_and_clean_head(Real_Goal, Clean_Head) :- 
	print_msg(1, 3, '', 'unify_real_goal_and_clean_head :: Real_Goal', Real_Goal),
	print_msg(1, 3, '', 'unify_real_goal_and_clean_head :: Clean_Head', Clean_Head),
	!, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate get_frontier_from_pre_frontier(goal, goal, ?, ?, ?, ?, ?).
get_frontier_from_pre_frontier(E, IE, NIE, GoalVars, LocalVars, RG_Constraints, Frontier_Nodes) :-
	setof((GoalVars, LocalVars, RG_Constraints, NIE), (E, IE), PreFr_Node_Answers), !,
	print_msg(3, 3, 'list', 'get_frontier_from_pre_frontier :: PreFr_Node_Answers', PreFr_Node_Answers),
	get_eqs_and_diseqs_from_answers(PreFr_Node_Answers, GoalVars, [], Frontier_Nodes), !,
	print_msg(3, 3, 'list', 'get_frontier_from_pre_frontier :: Frontier_Nodes', Frontier_Nodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_eqs_and_diseqs_from_answers([], _GoalVars, Frontier_Nodes, Frontier_Nodes).
get_eqs_and_diseqs_from_answers([Answer | Answers], GoalVars, FN_In, FN_Out) :- !,
	get_eqs_and_diseqs_from_one_answer(Answer, GoalVars, FN), !,
	get_eqs_and_diseqs_from_answers(Answers, GoalVars, [FN | FN_In], FN_Out).

% get_eqs_and_diseqs_from_one_answer(Answer, GoalVars, LocalVars, RG_Constraints, Frontier_Node)
get_eqs_and_diseqs_from_one_answer(Answer, GoalVars, Frontier_Node) :-
	Answer = (Answ_GoalVars, Answ_LocalVars, Answ_RG_Constraints, Answ_NIE),
	print_msg(3, 3, '', 'get_eqs_and_diseqs_from_one_answer :: (GoalVars)', (GoalVars)),
	print_msg(3, 3, '', 'get_eqs_and_diseqs_from_one_answer :: (Answ_GoalVars, Answ_LocalVars)', (Answ_GoalVars, Answ_LocalVars)),
	print_msg(3, 3, '', 'get_eqs_and_diseqs_from_one_answer :: Answ_RG_Constraints', Answ_RG_Constraints),

	% A variable can have attributes coming from a higher level, 
	% and this ones are NOT part of the current frontier.
	get_disequalities_from_constraints_and_remove_them((Answ_GoalVars, Answ_LocalVars), Disequalities),
	attributes_difference(Disequalities, Answ_RG_Constraints, New_Constraints),
	attribute_diseq_to_executable_diseq(New_Constraints, New_IE),

	% The only equalities we are interested in are the ones concerning GoalVars, because
	% Chan mechanism unifies the variables in LocalVars that occur in a equality.
	% We are just saving execution time and memory space.
	get_equalities_conj_from_lists(GoalVars, Answ_GoalVars, true, New_E), 
	!, 
	subfrontier_E_IE_NIE_contents(Frontier_Node, New_E, New_IE, Answ_NIE),
	print_msg(3, 3, 'aux', 'get_eqs_and_diseqs_from_one_answer :: Frontier_Node', Frontier_Node),
	print_vars_diseqs(3, '', Frontier_Node), 
	print_msg(3, 3, 'nl', '', ''),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	 

get_equalities_conj_from_lists([], [], Conj, Conj) :- !.
get_equalities_conj_from_lists([Elto_1], [Elto_2], Conj_In, Conj_Out) :- !,
	goals_join_by_conjunction(Conj_In, (Elto_1 = Elto_2), Conj_Out), !.
get_equalities_conj_from_lists([Elto_1 | List_1], [Elto_2 | List_2], Conj_In, Conj_Out) :- !,
	goals_join_by_conjunction(Conj_In, (Elto_1 = Elto_2), Conj_Aux), !,
	get_equalities_conj_from_lists(List_1, List_2, Conj_Aux, Conj_Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

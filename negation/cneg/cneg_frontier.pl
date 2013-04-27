
:- module(cneg_frontier, [compute_frontier/3], [assertions]).

:- comment(title, "Contructive Negation Runtime Library - Frontiers generation.").
:- comment(author, "V@'{i}ctor Pablos Ceruelo").
:- comment(summary, "This module implements negation predicates for runtime evaluation.").

:- use_module(library(aggregates),[setof/3]).
:- use_module(library('cneg/cneg_aux')).
% :- reexport(library('cneg/cneg_aux')).
:- use_module(library('cneg/cneg_diseq')).
:- use_module(library('cneg/cneg_rt')).
% :- reexport(cneg_diseq).

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/9.
% To evaluate predicates only from the top package.
:- multifile call_to_predicate/1.
:- multifile file_debug_is_activated/1.

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
	echo_msg(2, '', 'cneg_rt', 'compute_set_of_frontiers :: (Goal, GoalVars)', (Goal, GoalVars)),
	split_body_with_disjunctions_into_bodies(Goal, Goals),
	!,
	echo_msg(2, 'list', 'cneg_rt', 'compute_set_of_frontiers :: Goals', Goals),
	compute_frontier_aux(Goals, GoalVars, [], Frontier),
	!.

compute_frontier_aux([], _GoalVars, Frontier_Out, Frontier_Out) :- !.
compute_frontier_aux([Goal | More_Goals], GoalVars, Frontier_In, Frontier_Out) :-
	echo_msg(2, '', 'cneg_rt', 'compute_goal_pre_frontier :: Goal', Goal),	
	compute_goal_pre_frontier(Goal, Goal_PreFrontier), !,
	echo_msg(2, '', 'cneg_rt', 'compute_goal_pre_frontier :: Goal_PreFrontier', Goal_PreFrontier),	
	pre_frontier_to_frontier(Goal_PreFrontier, GoalVars, [], Frontier_Tmp), !,
	echo_msg(2, '', 'cneg_rt', 'compute_goal_pre_frontier :: Frontier_Tmp', Frontier_Tmp),	
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
	echo_msg(1, '', 'cneg_rt', 'ERROR: Not possible computing the frontier for a disjunction', Goal), 
	echo_msg(1, 'nl', 'cneg_rt', '', ''), !, % Backtracking is forbidden.
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
	goal_is_cneg_rt(Goal, UQV, GoalVars, SubGoal), !,
	echo_msg(2, '', 'cneg_rt', 'compute_goal_pre_frontier :: dn :: double negation for (UQV, GoalVars, SubGoal)', (UQV, GoalVars, SubGoal)),
%     cneg_rt(Goal, GoalVars, Depth_Level, Trace) :-
	cneg_rt_aux(SubGoal, GoalVars, Negated_PreFr), !,
	echo_msg(2, 'list', 'cneg_rt', 'compute_goal_pre_frontier :: dn :: Negated_PreFr', Negated_PreFr),
	convert_negPreFr_to_preFr(Goal, Negated_PreFr, PreFrontier).

% Now go for other functors stored in our database.
compute_goal_pre_frontier(Goal, Frontier_Out) :-
	goal_is_not_conj_disj_eq_diseq_dneg(Goal),
	echo_msg(2, '', 'cneg_rt', 'compute_goal_pre_frontier :: Goal', Goal),
	look_for_the_relevant_clauses(Goal, Frontier_Tmp_1),
	echo_msg(2, 'list', 'cneg_rt', 'Frontier_Tmp_1', Frontier_Tmp_1),
	simplify_and_reorder_goal_prefrontier(Frontier_Tmp_1, Goal, [], Frontier_Out),
	echo_msg(2, 'list', 'cneg_rt', 'compute_goal_pre_frontier :: Frontier_Out', Frontier_Out), 
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	!. % Backtracking is forbidden.

% And at last report an error if it was impossible to found a valid entry.
compute_goal_pre_frontier(Goal, []) :-
	echo_msg(1, '', 'cneg_rt', 'ERROR: Not found frontier for Goal', Goal), 
	echo_msg(1, 'nl', 'cneg_rt', '', ''), !. % Backtracking is forbidden.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Look for all the predicate definitions for the predicate with the name and arity of the one we want.
% We use its name and arity to avoid problematic unifications.
look_for_the_relevant_clauses(Goal, PreFrontier) :-
	functor(Goal, Head_Name, Head_Arity),  % Security
	Head_Name \== ',', Head_Name \== ';',    % Issues
	!, % Backtracking forbiden.
	cneg_pre_frontier(Head_Name, Head_Arity, _SourceFileName_, _Clean_Head_, _E_, _IE_, _NIE_, _Head_, _Body_),
%	debug_clause('look_for_the_relevant_clauses :: (Name, Arity, SourceFileName)', (Name, Arity, SourceFileName)),
	setof(preFrontierNode(_Real_Goal, Clean_Head, E, IE, NIE, Head, Body), 
	cneg_pre_frontier(Head_Name, Head_Arity, _SourceFileName, Clean_Head, E, IE, NIE, Head, Body),
	PreFrontier).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% simplify_and_reorder_goal_prefrontier(Front,PreFr) simplifies the frontier Front.
% Since the frontiers retrieved are in an inverted order, 
% we must reorder them to keep procedural semantics unchanged.
simplify_and_reorder_goal_prefrontier([], _Goal, PreFr_Acc, PreFr_Acc) :- !.
%	echo_msg(2, 'nl', 'cneg_rt', '', '').
simplify_and_reorder_goal_prefrontier([PreFr | PreFr_In], Goal, PreFr_Acc, PreFr_Out) :-
	test_frontier_is_valid(PreFr, Goal), !,
%	echo_msg(2, '', 'cneg_rt', 'simplify_and_reorder_goal_prefrontier :: valid :: PreFr', PreFr),
	simplify_and_reorder_goal_prefrontier(PreFr_In, Goal, [PreFr | PreFr_Acc], PreFr_Out).
simplify_and_reorder_goal_prefrontier([_PreFr | PreFr_In], Goal, PreFr_Acc, PreFr_Out) :-
%	echo_msg(2, '', 'cneg_rt', 'simplify_and_reorder_goal_prefrontier :: not valid :: PreFr', PreFr),
	simplify_and_reorder_goal_prefrontier(PreFr_In, Goal, PreFr_Acc, PreFr_Out).

% simplify_and_reorder_goal_prefrontier_unifying_variables(H, Body_In, G, Body_Out) 
% returns in Body_Out the elements of Body whose head unifies with G.
test_frontier_is_valid(PreFr, Goal) :-
	% We make copies to avoid unifications.
	copy_term((PreFr, Goal), (PreFr_Copy, Goal_Copy)),

%	preFrontierNodeContents(PreFr, Real_Goal, Clean_Head, E, IE, NIE, Head, Body).
	preFrontierNodeContents(PreFr_Copy, Real_Goal, Clean_Head, E, IE, _NIE, _Head, _Body),

	Clean_Head = Goal_Copy, % Unify heads.
	(
	    (
		call_to_predicate((E, IE)), !, 
		Real_Goal = Goal, !,
		cneg_diseq_echo(2, '', 'cneg_rt', 'test_frontier_is_valid :: VALID :: Goal', Goal),
		cneg_diseq_echo(2, '', 'cneg_rt', 'test_frontier_is_valid :: VALID :: PreFr', PreFr),
		! % Backtracking forbidden.
	    )
	;
	    (
		cneg_diseq_echo(2, '', 'cneg_rt', 'test_frontier_is_valid :: NOT VALID :: Goal', Goal),
		cneg_diseq_echo(2, '', 'cneg_rt', 'test_frontier_is_valid :: NOT VALID :: PreFr', PreFr),
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
	echo_msg(2, 'list', 'cneg_rt', 'compute_goal_pre_frontier :: dn :: Conj_Of_Disjs_PreFr', Conj_Of_Disjs_PreFr),
	split_body_with_disjunctions_into_bodies(Conj_Of_Disjs_PreFr, List_Of_Conjs_PreFr), !,
	echo_msg(2, 'list', 'cneg_rt', 'compute_goal_pre_frontier :: dn :: List_Of_Conjs_PreFr', List_Of_Conjs_PreFr),
	split_bodies_into_E_IE_NIE(List_Of_Conjs_PreFr, Split_Bodies),
	echo_msg(2, 'list', 'cneg_rt', 'compute_goal_pre_frontier :: dn :: Split_Bodies', Split_Bodies),
	split_bodies_to_prefrontier(Split_Bodies, Goal, PreFrontier), !,
	echo_msg(2, 'list', 'cneg_rt', 'double neg :: PreFrontier', PreFrontier),
	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
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
rebuild_prefrontier_conjunction([],_F2,[]) :- !, % Optimization
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'rebuild_prefrontier_conjunction', 'empty F1 -> empty F3'),
	echo_msg(2, 'nl', 'cneg_rt', '', '').
rebuild_prefrontier_conjunction(_F1,[],[]) :- !, % Optimization
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'rebuild_prefrontier_conjunction', 'empty F2 -> empty F3'),
	echo_msg(2, 'nl', 'cneg_rt', '', '').
rebuild_prefrontier_conjunction(F1,F2,F3) :-
	F1 \== [], F2 \== [],
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'rebuild_prefrontier_conjunction', 'F1 /\\ F2 -> F3 '),
	rebuild_prefrontier_conjunction_aux_1(F1,F2,F3),
	echo_msg(2, '', 'cneg_rt', 'rebuild_prefrontier_conjunction', 'F3'),
	echo_msg(2, 'list', 'cneg_rt', '', F3),
	echo_msg(2, 'nl', 'cneg_rt', '', '').
	
rebuild_prefrontier_conjunction_aux_1([],_F2,[]) :- !.
rebuild_prefrontier_conjunction_aux_1([F1_1 | More_F1], F2, F3):-
        rebuild_prefrontier_conjunction_aux_2(F1_1, F2, F3_1),
        rebuild_prefrontier_conjunction_aux_1(More_F1, F2, More_F3),
        cneg_aux:append(F3_1, More_F3, F3).

% combine_frontiers_aux_1(F1_1,F2, F3) returns F3 that is 
% the result of combining F1_1 with each element of F2.
rebuild_prefrontier_conjunction_aux_2(_F1_1, [], []).
rebuild_prefrontier_conjunction_aux_2(F1_1, [F2_1 | More_F2], [F3 | More_F3]) :-
%	preFrontierNodeContents(PreFrontier, Goal, Head, Body, FrontierTest).
	preFrontierNodeContents(F1_1, F1_1_Real_Goal, F1_1_Head, F1_1_Body, F1_1_F_Test),
	preFrontierNodeContents(F2_1, F2_1_Real_Goal, F2_1_Head, F2_1_Body, F2_1_F_Test),
	F3_Real_Goal = ((F1_1_Real_Goal), (F2_1_Real_Goal)),
	F3_Head = ((F1_1_Head), (F2_1_Head)),
	F3_Body = ((F1_1_Body), (F2_1_Body)),
	F3_F_Test = ((F1_1_F_Test), (F2_1_F_Test)),
	preFrontierNodeContents(F3, F3_Real_Goal, F3_Head, F3_Body, F3_F_Test),
	test_frontier_is_valid(F3, F3_Real_Goal), !, 
        rebuild_prefrontier_conjunction_aux_2(F1_1, More_F2, More_F3).
rebuild_prefrontier_conjunction_aux_2(F1_1, [_F2_1 | More_F2], More_F3) :-
	rebuild_prefrontier_conjunction_aux_2(F1_1, More_F2, More_F3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pre_frontier_to_frontier(Goal_PreFrontier, GoalVars, Frontier_In, Frontier_Out) :-
	echo_msg(2, 'list', 'cneg_rt', 'pre_frontier_to_frontier :: Frontier_In', Frontier_In),
	pre_frontier_to_frontier_aux(Goal_PreFrontier, GoalVars, Frontier_In, Frontier_Out), !,
	echo_msg(2, 'list', 'cneg_rt', 'pre_frontier_to_frontier :: Frontier_Out', Frontier_Out).

pre_frontier_to_frontier(Goal_PreFrontier, GoalVars, Frontier_In, _Frontier_Out) :-
	echo_msg(1, 'nl', 'cneg_rt', '', ''),
	echo_msg(1, 'list', 'cneg_rt', 'ERROR: pre_frontier_to_frontier :: Goal_PreFrontier', Goal_PreFrontier),
	echo_msg(1, 'nl', 'cneg_rt', '', ''),
	echo_msg(1, '', 'cneg_rt', 'ERROR: pre_frontier_to_frontier :: GoalVars', GoalVars),
	echo_msg(1, 'nl', 'cneg_rt', '', ''),
	echo_msg(1, 'list', 'cneg_rt', 'ERROR: pre_frontier_to_frontier :: Frontier_In', Frontier_In),
	echo_msg(1, 'nl', 'cneg_rt', '', ''),
	!, fail.

pre_frontier_to_frontier_aux([], _GoalVars, Frontier_N_Out, Frontier_N_Out) :- !.
pre_frontier_to_frontier_aux([Goal_PreFrontier_Node | Goal_PreFrontier], GoalVars, Frontier_N_In, Frontier_N_Out) :-
	pre_frontier_node_to_frontier_node(Goal_PreFrontier_Node, GoalVars, [], Goal_Frontier_Nodes), !,
	echo_msg(2, '', 'cneg_rt', 'pre_frontier_to_frontier_aux :: Goal_Frontier_Nodes', Goal_Frontier_Nodes),
	append(Frontier_N_In, Goal_Frontier_Nodes, Frontier_N_Aux), !,
	pre_frontier_to_frontier_aux(Goal_PreFrontier, GoalVars, Frontier_N_Aux, Frontier_N_Out).

pre_frontier_node_to_frontier_node(Goal_PreFrontier_Node, GoalVars, Frontier_N_In, Frontier_N_Out) :-
	echo_msg(2, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node :: Goal_PreFrontier_Node', Goal_PreFrontier_Node),
%	preFrontierNodeContents(Goal_PreFrontier_Node, Real_Goal, Clean_Head, E, IE, NIE, Head, Body).
	preFrontierNodeContents(Goal_PreFrontier_Node, Real_Goal, Clean_Head, E, IE, NIE, Head, Body),
	echo_msg(2, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node :: (Head, Body)', (Head, Body)),
	echo_msg(2, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node :: Clean_Head', Clean_Head),
	echo_msg(2, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node :: E', E),
	echo_msg(2, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node :: IE', IE),
	echo_msg(2, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node :: NIE', NIE),

	% Link the query and the pre-node (only goalvars, not uqv).
	copy_term((Real_Goal, GoalVars), (Real_Goal_Copy, GoalVars_Copy)),
	GoalVars_Copy = GoalVars, % Unify Goalvars_Copy, but not UQV_Copy.
	Real_Goal_Copy = Clean_Head, % Unify head and goal, but not UQV.
	!, % Backtracking is forbidden.

	% Proposal == 'cneg_rt_Chan',
	varsbag((Clean_Head, Body), GoalVars, [], PreFr_Node_UQV), % Identify UQV in Pre_Node
	echo_msg(2, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node :: PreFr_Node_UQV', PreFr_Node_UQV),
	pre_frontier_node_to_frontier_node_aux((E, IE), NIE, PreFr_Node_UQV, GoalVars, Frontier_Nodes),
	echo_msg(2, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node :: Frontier_Nodes', Frontier_Nodes),
	append(Frontier_N_In, Frontier_Nodes, Frontier_N_Out). % Is order really relevant ???

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pre_frontier_node_to_frontier_node_aux([], [], _UQV, _GoalVars, []) :- !.
pre_frontier_node_to_frontier_node_aux([], NIE_Body, _UQV, _GoalVars, Frontier_Nodes) :- !,
	generate_conjunction_from_list(NIE_Body, Frontier_Nodes).
pre_frontier_node_to_frontier_node_aux(E_IE_Body, NIE_Body, UQV, GoalVars, Frontier_Nodes) :-
	generate_conjunction_from_list(E_IE_Body, Conj_E_IE_Body),
	echo_msg(2, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node_aux :: Conj_E_IE_Body', Conj_E_IE_Body),
	pre_frontier_node_to_frontier_node_aux_aux(UQV, GoalVars, NIE_Body, Conj_E_IE_Body, Pre_Node_Answers),
	
	get_eqs_and_diseqs_from_answers(Pre_Node_Answers, UQV, GoalVars, [], Frontier_Nodes), !,
	echo_msg(2, 'list', 'cneg_rt', 'pre_frontier_node_to_frontier_node_aux :: Frontier_Nodes', Frontier_Nodes).

%:- push_prolog_flag(unused_pred_warnings, no).
%:- meta_predicate pre_frontier_node_to_frontier_node_aux_aux(?, ?, ?, goal, ?).
pre_frontier_node_to_frontier_node_aux_aux(UQV, GoalVars, NIE_Body, Conj_E_IE_Body, Pre_Node_Answers) :-
	echo_msg(2, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node_aux :: setof :: (UQV, GoalVars)', (UQV, GoalVars)),
	echo_msg(2, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node_aux', 'setof((UQV, GoalVars, NIE_Body), Conj_E_IE_Body, [(UQV, GoalVars, NIE_Body)])'),
	setof((UQV, GoalVars, NIE_Body), Conj_E_IE_Body, Pre_Node_Answers), !,
	cneg_diseq_echo(2, 'list', 'cneg_rt', 'setof :: [(UQV, GoalVars, NIE_Body)]', Pre_Node_Answers),
	echo_msg(2, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node_aux :: setof :: (UQV, GoalVars)', (UQV, GoalVars)),
	!.

pre_frontier_node_to_frontier_node_aux_aux(UQV, GoalVars, NIE_Body, Conj_E_IE_Body, _Pre_Node_Answers) :-
	call(Conj_E_IE_Body), !, % If this succeeds there is an error.
	echo_msg(1, 'nl', 'cneg_rt', '', ''),
	echo_msg(1, '', 'cneg_rt', 'ERROR in pre_frontier_node_to_frontier_node_aux_aux', 'setof/3 has failed'),
	echo_msg(1, 'nl', 'cneg_rt', '', ''),
	echo_msg(1, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node_aux', 'setof((UQV, GoalVars, NIE_Body), Conj_E_IE_Body, Pre_Node_Answers)'),
	echo_msg(1, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node_aux :: setof :: UQV', UQV),
	echo_msg(1, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node_aux :: setof :: GoalVars', GoalVars),
	echo_msg(1, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node_aux :: setof :: NIE_Body', NIE_Body),
	echo_msg(1, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node_aux :: setof :: Conj_E_IE_Body', Conj_E_IE_Body),
 	echo_msg(1, 'nl', 'cneg_rt', '', ''),
	!, fail.

pre_frontier_node_to_frontier_node_aux_aux(_UQV, _GoalVars, _NIE_Body, Conj_E_IE_Body, []) :-
	echo_msg(2, '', 'cneg_rt', 'pre_frontier_node_to_frontier_node_aux :: not satisfiable :: Conj_E_IE_Body', Conj_E_IE_Body), !.
%:- pop_prolog_flag(unused_pred_warnings).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

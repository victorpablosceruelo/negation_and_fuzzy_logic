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
	cneg_diseq_eqv_uqv/5, cneg_eq_eqv_uqv/5]).
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
	cneg_rt_Aux(UQV, Goal, 'Chan').

cneg_rt_New(UQV, Goal):-
	cneg_rt_Aux(UQV, Goal, 'New').

cneg_rt_Aux(UQV, Goal, Proposal) :-
	debug_msg(1, 'cneg_rt_Aux :: (UQV, Goal, Proposal)', (UQV, Goal, Proposal)),
	varsbag(UQV, [], [], Real_UQV),
	varsbag(Goal, Real_UQV, [], GoalVars),
	compute_frontier(Goal, Real_Goal, Frontier_Tmp), !,
	adequate_frontier(Frontier_Tmp, Real_Goal, Real_UQV, Frontier), !,
	debug_msg(1, 'cneg_rt_Aux :: (UQV, Real_Goal)', (UQV, Real_Goal)),
	debug_msg_list(1, 'cneg_rt_Aux :: Frontier', Frontier),
	negate_set_of_frontiers(Frontier, Proposal, GoalVars, Result), !,
	debug_msg(1, 'cneg_rt_Aux :: Result', Result),
	debug_msg_nl(1),
	call_to(Result).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Structure to manage all the info about the frontier in an easy way.
frontier_contents(frontier(Head, Body, FrontierTest), Head, Body, FrontierTest).

% Predicate to change all the heads in a frontier.
change_heads([], _New_Head, []) :- !.
change_heads([F_In | Frontier_Tmp], New_Head, [F_Out | Frontier_Out]) :-
	frontier_contents(F_In, _Old_Head, Body, FrontierTest),
	frontier_contents(F_Out, New_Head, Body, FrontierTest),
	change_heads(Frontier_Tmp, New_Head, Frontier_Out).

% combine_frontiers(F1,F2,F3) returns F3 that is the resulting frontier
% from combining the frontiers F1 and F2 in all possible ways.
combine_frontiers([],_F2,[]):-!.
combine_frontiers([F1_1 | More_F1], F2, F3):-
        combine_frontier_aux(F1_1, F2, F3_1),
        combine_frontiers(More_F1, F2, More_F3),
        cneg_aux:append(F3_1, More_F3, F3).

% combine_frontiers_aux_1(F1_1,F2, F3) returns F3 that is 
% the result of combining F1_1 with each element of F2.
combine_frontier_aux(_F1_1, [], []).
combine_frontier_aux(F1_1, [F2_1 | More_F2], [F3 | More_F3]):-
	frontier_contents(F1_1, F1_1_Head, F1_1_Body, F1_1_F_Test),
	frontier_contents(F2_1, F2_1_Head, F2_1_Body, F2_1_F_Test),
	frontier_contents(F3, ((F1_1_Head), (F2_1_Head)), ((F1_1_Body), (F2_1_Body)), ((F1_1_F_Test), (F2_1_F_Test))),
        combine_frontier_aux(F1_1, More_F2, More_F3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adequate_frontier([], _Goal, _Real_UQV, []) :- !.
adequate_frontier([F_In | Frontier_In], Goal, Real_UQV, [F_Out | Frontier_Out]) :-
	adequate_frontier_aux(Goal, Real_UQV, F_In, F_Out),
	adequate_frontier(Frontier_In, Goal, Real_UQV, Frontier_Out).

adequate_frontier_aux(Goal, Real_UQV, F_In, Body_Copy) :-
	frontier_contents(F_In, Head, Body, _F_Test),
	copy_term((Head, Body), (Head_Copy, Body_Copy)), % Fresh copy to avoid variable clashes.
	varsbag(Goal, Real_UQV, [], Goal_GoalVars), % Determine goalvars.
	copy_term((Goal, Goal_GoalVars), (Goal_Copy, Goal_GoalVars_Copy)),
	Goal_GoalVars = Goal_GoalVars_Copy, % Unify goalvars, but not UQV.
	Goal_Copy = Head_Copy, % Unify head and goal definitely
	!. % Backtracking is forbidden.

adequate_frontier_aux(Goal, Real_UQV, F_In, _Body_Copy) :-
	debug_msg(1, 'ERROR: adequate_frontier_aux(Goal, Real_UQV, F_In)', (Goal, Real_UQV, F_In)),
	!, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% compute_neg_frontier(Goal,Frontier) 
% obtains in Frontier the frontier of the  goal Goal.
% It is a list of list that represent the disjunction of its
% elements where each element is a conjunction of subgoals.

% Just to debug.
%compute_frontier(Goal, _Frontier) :-
%	debug_msg(1, '--------------------------------------------------------------------------------------------------------------', ' '),
%	debug_msg(1, 'compute_frontier :: (Goal)', (Goal)),	
%	fail. % Just debug and use backtracking to continue.

% First remove $ and qualification from the goal's name.
compute_frontier(Goal, Real_Goal, Frontier) :-
	goal_clean_up(Goal, Tmp_Goal), !,
	compute_frontier(Tmp_Goal, Real_Goal, Frontier).

% Manage true and fail ...
compute_frontier('true', 'true', [F_Out]) :- !,
	frontier_contents(F_Out, 'true', 'true', 'true').
compute_frontier('fail', 'fail', [F_Out]) :- !,
	frontier_contents(F_Out, 'fail', 'fail', 'true').

% Now go for the disjunctions.
compute_frontier(Goal, ((Real_G1) ; (Real_G2)), Frontier_Out):- 
	goal_is_disjunction(Goal, G1, G2), !,
	compute_frontier(G1, Real_G1, Frontier_G1),
	compute_frontier(G2, Real_G2, Frontier_G2),
	list_head(Frontier_G1, F_G1),
	list_head(Frontier_G2, F_G2),
	frontier_contents(F_G1, F_G1_Head, _F_G1_Body, _F_G1_F_Test),
	frontier_contents(F_G2, F_G2_Head, _F_G2_Body, _F_G2_F_Test),
	append(Frontier_G1, Frontier_G2, Frontier_Tmp),
	change_heads(Frontier_Tmp, ((F_G1_Head) ; (F_G2_Head)), Frontier_Out).

% Now go for the conjunctions.
compute_frontier(Goal, ((Real_G1) , (Real_G2)), Frontier):- 
	goal_is_conjunction(Goal, G1, G2), !,
	compute_frontier(G1, Real_G1, Frontier_G1),
	compute_frontier(G2, Real_G2, Frontier_G2),
	combine_frontiers(Frontier_G1, Frontier_G2, Frontier).

% Now go for the functors for equality and disequality.
% None of them is managed yet, so just bypass them.
compute_frontier(Goal, Real_Goal, [F_Out]) :- 
	goal_is_disequality(Goal, T1, T2, UQV), !,
	functor_local(Real_Goal, 'diseq_uqv', 3, [ T1 |[ T2 |[ UQV ]]]),
	frontier_contents(F_Out, Real_Goal, Real_Goal, 'true').

compute_frontier(Goal, Real_Goal, [F_Out]) :- 
	goal_is_equality(Goal, T1, T2, UQV), !,
	functor_local(Real_Goal, 'eq_uqv', 3, [ T1 |[ T2 |[ UQV ]]]),
	frontier_contents(F_Out, Real_Goal, Real_Goal, 'true').

% Double negation is not managed yet. Bypass it.
compute_frontier(Goal, Real_Goal, [F_Out]) :- 
	goal_is_negation(Goal, UQV, SubGoal), !,
	functor_local(Real_Goal, 'cneg_rt', 2, [ UQV |[ SubGoal ]]),
	frontier_contents(F_Out, Real_Goal, Real_Goal, 'true').

% Now go for other functors stored in our database.
compute_frontier(Goal, Goal, Frontier_Out) :-
	goal_is_not_conj_disj_eq_diseq_dneg(Goal),
%	debug_msg(0, 'compute_frontier :: Goal', Goal),
	look_for_the_relevant_clauses(Goal, Frontier_Tmp_1),
%	debug_msg(0, 'compute_neg_frontier :: format', '(Head, Body, FrontierTest)'),
%	debug_msg_list(1, 'Frontier_Tmp_1', Frontier_Tmp_1),
	simplify_frontier(Frontier_Tmp_1, Goal, [], Frontier_Out),
%	debug_msg(0, 'Frontier_Out', Frontier_Out), 
	!. % Backtracking is forbidden.

% And at last report an error if it was impossible to found a valid entry.
compute_frontier(Goal, Goal, []) :-
	debug_msg(1, 'ERROR: Not found frontier for Goal', Goal), 
	debug_msg_nl(1), !. % Backtracking is forbidden.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

look_for_the_relevant_clauses(Goal, Frontier) :-
	functor(Goal, Name, Arity),  % Security
	Name \== ',', Name \== ';',    % Issues
	!, % Backtracking forbiden.
	cneg_pre_frontier(Name, Arity, _SourceFileName, _Head_Aux, _Body_Aux, _FrontierTest_Aux), 
%	debug_clause('look_for_the_relevant_clauses :: (Name, Arity, SourceFileName)', (Name, Arity, SourceFileName)),
	setof(frontier(Head, Body, FrontierTest), 
	cneg_pre_frontier(Name, Arity, _SourceFileName, Head, Body, FrontierTest), Frontier).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% simplify_frontier(Front,Frontier) simplifies the frontier Front.
% Since the frontiers retrieved are in an inverted order, 
% we must reorder them to keep procedural semantics unchanged.
simplify_frontier([], _Goal, Frontier_Acc, Frontier_Acc) :- !.
simplify_frontier([F_In | Frontier_In], Goal, Frontier_Acc, Frontier_Out) :-
	test_frontier_is_valid(F_In, Goal), !,
	debug_msg(1, 'simplify_frontier :: valid: ', F_In),
	simplify_frontier(Frontier_In, Goal, [F_In | Frontier_Acc], Frontier_Out).
simplify_frontier([F_In | Frontier_In], Goal, Frontier_Acc, Frontier_Out) :-
	debug_msg(1, 'simplify_frontier :: not valid: ', F_In),
	simplify_frontier(Frontier_In, Goal, Frontier_Acc, Frontier_Out).

% simplify_frontier_unifying_variables(H, Body_In, G, Body_Out) 
% returns in Body_Out the elements of Body whose head unifies with G.
test_frontier_is_valid(F_In, Goal) :-
	frontier_contents(F_In, Head, _Body, F_Test),
	debug_msg(0, 'test_frontier_is_valid :: (Goal, Head, F_Test)', (Goal, Head, F_Test)),
        copy_term((Goal, Head, F_Test), (Goal_Tmp, Head_Tmp, F_Test_Tmp)), 
        Head_Tmp = Goal_Tmp, % Test that head and goal can be unified. 
	goal_is_equality(F_Test_Tmp, Tmp_Left, Tmp_Right, _Eq_UQV), % It must be an equality.
	Tmp_Left = Tmp_Right, % Test that unifications previously in head can be performed.
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
negate_set_of_frontiers([], _Proposal, _GoalVars, true) :- !. % Optimization.
negate_set_of_frontiers([Frontier | More_Frontiers], Proposal, GoalVars, Result) :-
%	debug_msg(1, 'negate_frontier: (Frontier, GoalVars)', (Frontier, GoalVars)),
	negate_frontier(Frontier, Proposal, GoalVars, Result_Frontier),
	debug_msg(1, 'negate_frontier: Result_Frontier', Result_Frontier),
	!, % Reduce the stack's memory.
	negate_set_of_frontiers(More_Frontiers, Proposal, GoalVars, Result_More_Frontiers),
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

negate_frontier(Frontier_In, Proposal, GoalVars, Result):-
%	debug_msg(1, 'negate_frontier :: Frontier_In', (Frontier_In)),
	split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Aux_1),
%	split_frontier_contents(Frontier_Aux_1, E_Aux_1, IE_Aux_1, NIE_Aux_1),
%	debug_msg(1, 'negate_frontier :: (E_Aux_1, IE_Aux_1, NIE_Aux_1)', (E_Aux_1, IE_Aux_1, NIE_Aux_1)),
	!, % Reduce the stack's memory.
	normalize_E_IE_NIE(Proposal, Frontier_Aux_1, GoalVars, Frontier_Aux_2, ImpVars),
	split_frontier_contents(Frontier_Aux_2, E_Aux_2, IE_Aux_2, NIE_Aux_2),
%	debug_msg(1, 'negate_frontier :: (E_Aux_2, IE_Aux_2, NIE_Aux_2)', (E_Aux_2, IE_Aux_2, NIE_Aux_2)),
	split_IE_NIE_between_imp_and_exp(IE_Aux_2, NIE_Aux_2, ImpVars, IE_imp, NIE_imp, IE_NIE_exp),
%	debug_msg(1, 'negate_frontier :: (IE_imp, NIE_imp, IE_NIE_exp)', (IE_imp, NIE_imp, IE_NIE_exp)),
	negate_formula(E_Aux_2, IE_imp, NIE_imp, IE_NIE_exp, Proposal, GoalVars, ImpVars, Result),
%	debug_msg(1, 'negate_frontier :: (Result)', (Result)),
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
	debug_msg(1, 'ERROR: split_frontier_into_E_IE_NIE can not deal with disjunctions. Frontier_In', Frontier_In),
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
	goal_is_equality(Frontier_In, _Term1, _Term2, _UQV), !,
	split_frontier_contents(Frontier_Out, Frontier_In, [], []).

split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_disequality(Frontier_In, _Term1, _Term2, _UQV), !,
	split_frontier_contents(Frontier_Out, [], Frontier_In, []).

split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_not_conj_disj_eq_diseq_dneg(Frontier_In), !,
	split_frontier_contents(Frontier_Out, [], [], Frontier_In).

split_frontier_into_E_IE_NIE(Frontier_In, _Frontier_Out) :- 
	debug_msg(1, 'ERROR: split_frontier_into_E_IE_NIE can not deal with frontier. Frontier_In', Frontier_In),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% normalize_I_D_R(I,D,R,GoalVars, In,Dn,Rn,ImpVars,ExVars) 
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
%	debug_msg(1, 'normalize_E_IE_NIE :: (E_Out, IE_Aux, NIE_Out)', (E_Out, IE_Aux, NIE_Out)),
	varsbag(E_Out, [], Real_GoalVars, ImpVars), % ImpVars = vars(E) + GoalVars
	varsbag(NIE_Out, Real_GoalVars, [], RelVars), % RelVars = vars(NIE) - GoalVars
	varsbag_addition(ImpVars, RelVars, ImpVars_and_RelVars),
%	debug_msg(1, 'remove_from_IE_irrelevant_disequalities :: (IE_Aux, ImpVars_and_RelVars)', (IE_Aux, ImpVars_and_RelVars)),
	remove_from_IE_irrelevant_disequalities(IE_Aux, ImpVars_and_RelVars, IE_Out),
%	debug_msg(1, 'remove_from_IE_irrelevant_disequalities :: (IE_Out)', (IE_Out)),
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
%	debug_msg(1, 'remove_from_E_irrelevant_equalities :: Irrelevant_Eqs', Irrelevant_Eqs),
	(
	    (   
		Irrelevant_Eqs = [],
		Formulae_Out = Formulae_In
	    )
	;
	    (
		Irrelevant_Eqs \== [],
		remove_irrelevant_equalities_1(Irrelevant_Eqs, Vars_EnoG, Formulae_In, Formulae_Aux_1),
%		debug_msg(1, 'remove_irrelevant_equalities_1 :: Formulae_Aux_1', Formulae_Aux_1),
		split_frontier_contents(Formulae_Aux_1, E_Aux_1, IE_Aux, NIE_Aux), 
		remove_irrelevant_equalities_2(E_Aux_1, [], _Eqs_Visited, E_Aux_2),
%		debug_msg(1, 'remove_irrelevant_equalities_2 :: E_Aux_2', E_Aux_2),
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
	goal_is_equality(E_In, Value_1, Value_2, _UQV),
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
	goal_is_equality(Irrelevant_Eq, Value_1, Value_2, _UQV),
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
	goal_is_equality(E_In, Value_1, Value_2, _UQV),
	Value_1 == Value_2, !.

remove_irrelevant_equalities_2(E_In, Eqs_Visited_In, Eqs_Visited_In, []) :-
	goal_is_equality(E_In, _Value_1, _Value_2, _UQV),
	memberchk(E_In, Eqs_Visited_In), !.

remove_irrelevant_equalities_2(E_In, Eqs_Visited_In, Eqs_Visited_In, []) :-
	goal_is_equality(E_In, Value_1, Value_2, UQV),
	goal_is_equality(E_Aux, Value_2, Value_1, UQV), % Different order
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
	goal_is_disequality(IE_In, _Term1, _Term2, _FreeVars), !,
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
	debug_msg(1, 'ERROR: remove_from_IE_irrelevant_disequalities can not deal with IE_In', IE_In),
	fail.


% split_D_R_into_imp_and_exp(I,D,R,GoalVars,ImpVars,ExpVars,SolC) returns SolC
% that is one of the solutions of the conjunction that is divided in 
% I, D and R (equalities, disequalities and rest of subgoals).
% GoalVars, ImpVars and ExpVars are set of useful variables 
split_IE_NIE_between_imp_and_exp(IE, NIE, ImpVars, IE_imp, NIE_imp, IE_NIE_exp):-
	split_ie_or_nie_between_imp_and_exp(IE, ImpVars, IE_imp, IE_exp),
%	debug_msg(1, 'split_ie_or_nie_between_imp_and_exp(IE, ImpVars, IE_imp, IE_exp)', split_ie_or_nie_between_imp_and_exp(IE, ImpVars, IE_imp, IE_exp)),
	split_ie_or_nie_between_imp_and_exp(NIE, ImpVars, NIE_imp, NIE_exp),
%	debug_msg(1, 'split_ie_or_nie_between_imp_and_exp(NIE, ImpVars, NIE_imp, NIE_exp)', split_ie_or_nie_between_imp_and_exp(NIE, ImpVars, NIE_imp, NIE_exp)),
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
	debug_msg(1, 'ERROR: split_ie_or_nie_between_imp_and_exp can not deal with IE_or_NIE', IE_or_NIE),
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
%	debug_msg(1, 'split_ie_or_nie_between_imp_and_exp', (IE_or_NIE, IE_or_NIE_imp, IE_or_NIE_exp)).

% negate_formula(GoalVars, ImpVars, ExpVars, I, Dimp, Dexp, Rimp, Rexp, R_naf, Sol)
% returns SolC that is one of the solutions of the conjunction that is divided
% I, Dimp,Dexp,Rimp and Rexp (equalities, disequalities and rest of subgoals).
% GoalVars, ImpVars and ExpVars are set of useful variables 
negate_formula([], [], [], [], _Proposal, _GoalVars, _ImpVars, true) :- !. % Optimization
negate_formula(E, IE_imp, NIE_imp, IE_NIE_exp, Proposal, GoalVars, ImpVars, Neg_E_IE_NIE) :-
%	debug_msg(1, 'negate_formula :: (E, IE_imp, NIE_imp, IE_NIE_exp)', (E, IE_imp, NIE_imp, IE_NIE_exp)),
 	negate_IE_NIE_exp(IE_NIE_exp, Proposal, ImpVars, Neg_IE_NIE_exp),
	negate_imp_form(NIE_imp, Proposal, ImpVars, Neg_IE_NIE_exp, Neg_NIE_imp_IE_NIE_exp),
	negate_imp_form(IE_imp, Proposal, ImpVars, Neg_NIE_imp_IE_NIE_exp, Neg_IE_NIE),
	negate_imp_form(E, Proposal, GoalVars, Neg_IE_NIE, Neg_E_IE_NIE),
	!. % Backtracking forbidden.

% negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,SolC) obtain in
% SolC a solution of negating Dexp y Rexp juntos.
negate_IE_NIE_exp([], _Proposal, _ImpVars, []):- !.
negate_IE_NIE_exp(IE_NIE_exp, 'Chan', ImpVars, Neg_IE_NIE_exp) :-
	IE_NIE_exp \== [],
	varsbag(IE_NIE_exp, ImpVars, [], ExpVars),
	Neg_IE_NIE_exp = (cneg_rt_Chan(ExpVars, IE_NIE_exp)), !.
negate_IE_NIE_exp(IE_NIE_exp, 'New', ImpVars, Neg_IE_NIE_exp) :-
	IE_NIE_exp \== [],
	varsbag(IE_NIE_exp, ImpVars, [], ExpVars),
	Neg_IE_NIE_exp = (cneg_rt_New(ExpVars, IE_NIE_exp)), !.

negate_imp_form([], _Proposal, _ImpVars, [], []) :- !. % Optimization.
negate_imp_form(Formula, _Proposal, _ImpVars, _Next_Formula, _Neg_Formula) :-
	goal_is_disjunction(Formula, _Formula_1, _Formula_2), !,
	debug_msg(1, 'ERROR: negate_imp_form can not deal with Formula', Formula),
	fail.

negate_imp_form(Formula, Proposal, ImpVars, Next_Formula, Neg_Formula) :-
	goal_is_conjunction(Formula, Formula_1, Formula_2), !,
 	negate_imp_form(Formula_2, Proposal, ImpVars, Next_Formula, Next_Formula_Aux),
 	negate_imp_form(Formula_1, Proposal, ImpVars, Next_Formula_Aux, Neg_Formula).

negate_imp_form(Formula, Proposal, ImpVars, Next_Formula, Neg_Formula) :-
	negate_imp_atom(Formula, Proposal, ImpVars, Neg_Atom, Keep_Atom),
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
negate_imp_atom([], _Proposal, _ImpVars, [], []) :- !. % Obvious.
negate_imp_atom(true, _Proposal, _ImpVars, fail, true):- !. % Trivial predicates
negate_imp_atom(fail, _Proposal, _ImpVars, true, fail):- !. % Trivial predicates

negate_imp_atom(Formula, _Proposal, ImpVars, Neg_Atom, Keep_Atom) :-
	goal_is_equality(Formula, T1, T2, UQV),
%	varsbag(UQV, [], [], Real_UQV), % Not yet
 	varsbag((T1,T2), ImpVars, [], FreeVars),
	Neg_Atom = (diseq_uqv(T1,T2, FreeVars)),
	Keep_Atom = (eq_uqv(T1,T2, UQV)).

negate_imp_atom(Formula, _Proposal, ImpVars, Neg_Atom, Keep_Atom) :-
	goal_is_disequality(Formula, T1, T2, UQV),
%	varsbag(UQV, [], [], Real_UQV), % Not yet
 	varsbag((T1,T2), ImpVars, [], FreeVars),
	Neg_Atom = (eq_uqv(T1,T2, FreeVars)),
	Keep_Atom = (diseq_uqv(T1,T2, UQV)).

negate_imp_atom(Formula, 'Chan', _ImpVars, Neg_Atom, Keep_Atom) :-
	Neg_Atom = (cneg_rt_Chan([], Formula)),
	Keep_Atom = (Formula). 

negate_imp_atom(Formula, 'New', _ImpVars, Neg_Atom, Keep_Atom) :-
	Neg_Atom = (cneg_rt_New([], Formula)),
	Keep_Atom = (Formula). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


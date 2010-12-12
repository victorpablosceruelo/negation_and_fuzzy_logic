%
% From Susana modified by VPC (started 29/06/2010)
%

:- module(cneg_lib, [cneg_lib_aux/3, negate_subfrontier/4], [assertions]).
% NOT NEEDED:  perform_a_call_to/1
:- meta_predicate cneg(goal).
%:- meta_predicate cneg_processed_pred(goal,?). 

% To access predicates from anywhere.
:- multifile cneg_processed_pred/4.
:- multifile cneg_dynamic_cl/6.
:- multifile cneg_static_cl/3.

:- use_module(cneg_aux, _).
:- use_module(cneg_diseq,[
	cneg_diseq/3, 
	remove_universal_quantification/2, 
	put_universal_quantification/1
			 ]).
%:- use_module(library(cneg_diseq),[cneg_diseq/3]).
% Esta linea para cuando cneg sea una libreria.

:- comment(title, "Contructive Negation Library").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module calls the predicates generated during the 
	program transformation.").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%cneg_lib_aux(Goal, [], Result) :- 
%	debug_msg_nl(0),
%	debug_msg(0, 'cneg_lib :: using cneg_static for', Goal), 
%	cneg_static(Goal, Result).

cneg_lib_aux(Goal, UnivVars, Result):-
%	UnivVars \== [],
	debug_msg_nl(1), 
	debug_msg_aux(1, '------------------------------------------------------------------------------', ''),
	debug_msg_nl(1), 
	debug_msg(1, 'cneg_lib :: cneg_dynamic :: Goal', Goal), 
	debug_msg(1, 'cneg_lib :: cneg_dynamic :: UnivVars', UnivVars),
	cneg_dynamic(Goal, UnivVars, Result),
	debug_msg(1, 'cneg_lib :: cneg_dynamic :: Result', Result),
	debug_msg_nl(1), 
	debug_msg_aux(1, '------------------------------------------------------------------------------', ''),
	debug_msg_nl(1), 
	!. % No backtracking allowed


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Be carefull: the following code produces random errors.
%cneg_static(_Goal) :-
%	cneg_static_pred(Goal, SourceFileName, Occurences), 
%	debug_msg(0, 'cneg_static_pred', (Goal, SourceFileName, Occurences)),
%	fail.

cneg_static(Goal, (Result_G1, Result_G2)) :-
	goal_is_conjunction(Goal, G1, G2), !,
%	debug_msg(0, 'cneg_static', goal_is_conjunction(Goal, G1, G2)),
	cneg_static(G1, Result_G1),
	cneg_static(G2, Result_G2).

cneg_static(Goal, Result) :-
	goal_clean_up(Goal, NewGoal), !,
%	debug_msg(0, 'cneg_static', goal_clean_up(Goal, NewGoal)),
	cneg_static(NewGoal, Result).

cneg_static(Goal, cneg_diseq(X, Y, FreeVars)) :-
	goal_is_disequality(Goal, X, Y, FreeVars), !.

cneg_static(Goal, cneg_eq(X, Y)) :-
	goal_is_equality(Goal, X, Y), !.

cneg_static(Goal, cneg_static_predicate_call(Goal, SourceFileName, Occurences)) :-
	functor_local(Goal, Name, Arity, _Args),
	cneg_processed_pred(Name, Arity, SourceFileName, Occurences).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cneg_dynamic(Goal,UnivVars) makes the constructive negation of 
% the Goal considering the set of variables GoalVars as the variables
% of the goal and UnivVars the universal quantified variable of it.

cneg_dynamic(Goal, UnivVars, Solution) :-
	varsbag_local(Goal, [], [], GoalVars),
	debug_msg(0, 'cneg_dynamic :: varsbag_local', (Goal, GoalVars)),
	put_universal_quantification(UnivVars),

	debug_msg(0, 'cneg_dynamic :: copy_term :: Original (Goal, GoalVars)', (Goal, GoalVars)),
	copy_term((Goal, GoalVars), (Goal_Copy, GoalVars_Copy)),
	debug_msg(0, 'cneg_dynamic :: copy_term :: Copy     (Goal, GoalVars)', (Goal_Copy, GoalVars_Copy)), 
	remove_universal_quantification(GoalVars_Copy, _Universally_Quantified),

	frontier(Goal_Copy, Frontier, Goal_Not_Qualified), 
	!, % No backtracking allowed
	negate_frontier(Frontier, Goal_Not_Qualified, Solution),
	!, % No backtracking allowed
	cneg_aux_equality(GoalVars, GoalVars_Copy),
	!. % No backtracking allowed

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% frontier(Goal,Frontier) 
% obtains in Frontier the frontier of the  goal Goal.
% It is a list of list that represent the disjunction of its
% elements where each element is a conjunction of subgoals.

frontier(Goal, Frontier, NewGoal) :-
	debug_msg_nl(0),
%	debug_msg(0, 'frontier :: Goal (IN)', Goal),
	frontier_aux(Goal, Frontier, NewGoal).
%	debug_msg(0, 'frontier :: Goal', Goal),
%	debug_msg(0, 'frontier :: Frontier', Frontier),
%	debug_msg(0, 'frontier :: NewGoal', NewGoal).

% First remove $ and qualification from the goal's name.
frontier_aux(Goal, Frontier, NewGoal) :-
	goal_clean_up(Goal, Tmp_Goal), !,
	frontier(Tmp_Goal, Frontier, NewGoal).

% Now go for the disjunctions.
frontier_aux(Goal, Frontier, (NewG1; NewG2)):- 
	goal_is_disjunction(Goal, G1, G2), !,
	frontier(G1, F1, NewG1),
	frontier(G2, F2, NewG2),
	cneg_aux:append(F1, F2, Front),
%	debug_msg(0, 'frontier :: disjunction', Front),
	simplify_frontier(Front, (NewG1;NewG2), Frontier).

% Now go for the conjunctions.
frontier_aux(Goal, Frontier, (NewG1, NewG2)):- 
	goal_is_conjunction(Goal, G1, G2), !,
	frontier(G1, F1, NewG1),
	frontier(G2, F2, NewG2),
	combine_frontiers(F1, F2, Front),
%	debug_msg(0, 'frontier :: conjunction', Front),
	simplify_frontier(Front, (NewG1,NewG2), Frontier).

% Now go for the functors for equality and disequality.
frontier_aux(Goal, [Frontier], NewGoal):- 
	goal_is_disequality(Goal, X, Y, FreeVars), !,
	cneg_aux_equality(NewGoal, (cneg_diseq(X, Y, FreeVars))),
	frontier_contents(Frontier, NewGoal, [NewGoal], [NewGoal]).

frontier_aux(Goal, [Frontier], NewGoal):- 
	goal_is_equality(Goal, X, Y), !,
	cneg_aux_equality(NewGoal, (cneg_eq(X, Y))),
	frontier_contents(Frontier, NewGoal, [NewGoal], [NewGoal]).


%frontier_aux((X=_Y), [(X=X,[])], (X=_Y)):- !.
%frontier_aux((X==Y),[(X==Y,[X==Y])]):- !.
%frontier_aux((X\==Y),[(X\==Y,[X\==Y])]):- !.

% Now go for other functors stored in our database.
frontier_aux(Goal, Front, Goal):-
	look_for_the_relevant_clauses(Goal, Front_Tmp),
	simplify_frontier(Front_Tmp, Goal, Front),
	!. % Frontier is uniquely determine if this clause is used.

% And at last report an error if it was impossible to found a valid entry.
frontier_aux(Goal, [], Goal) :-
	debug_msg(0, 'ERROR: frontier can not be evaluated for', Goal), 
	debug_msg_nl(0), debug_msg_nl(0), !, fail.

% simplify_frontier(Front,Frontier) simplifies the frontier Front.
simplify_frontier(Front_In, Goal, Front_Out) :-
%	debug_msg_nl(0),
%	debug_msg_list('simplify_frontier :: Front_In (list)', Front_In),
	debug_msg(0, 'simplify_frontier :: Goal', Goal),
	simplify_frontier_aux(Front_In, Goal, Front_Out).
%	debug_msg_list('simplify_frontier :: Front_Out (list)', Front_Out).

simplify_frontier_aux([], _G, []) :- !.
simplify_frontier_aux([SubFrontier | Frontier_In], G, [SubFrontier | Frontier_Out]):-
	test_subfrontier_is_valid(SubFrontier, G), !,
	debug_msg(0, 'simplify_frontier_aux (ok)', SubFrontier),
	simplify_frontier_aux(Frontier_In, G, Frontier_Out).
simplify_frontier_aux([SubFrontier | Frontier_In], G, Frontier_Out):-
	debug_msg(0, 'simplify_frontier_aux (no)', SubFrontier),
	simplify_frontier_aux(Frontier_In, G, Frontier_Out).

% simplify_frontier_unifying_variables(H, Body_In, G, Body_Out) 
% returns in Body_Out the elements of Body whose head unifies with G.
test_subfrontier_is_valid(SubFrontier, Goal):-
	frontier_contents(SubFrontier, Head, _Body, FrontierTest),
        copy_term((Head, FrontierTest), (Head_Tmp, FrontierTest_Tmp)), 
        copy_term(Goal, Goal_Tmp),
        cneg_aux_equality(Head_Tmp, Goal_Tmp), 
	call_combined_solutions(FrontierTest_Tmp), 
	!.

% combine_frontiers(F1,F2,F3) returns F3 that is the combined frontier of the list 
% of lists F1 and F2 after using the distributive rule
combine_frontiers([],_F2,[]):-!.
combine_frontiers([F1_1|More_F1],F2,F3):-
        combine_frontier_with_frontiers(F1_1,F2,F_Aux_1),
        combine_frontiers(More_F1,F2,F_Aux_2),
        cneg_aux:append(F_Aux_1,F_Aux_2,F3).

% combine_frontiers_aux_1(L1,E,L2) returns L2 that is 
% the list obtained after using the distributive rule to 
% the list of list L1 with respect the list E. 
% L1 and L2 are disjunction of conjunctions and E is a conjunction.
combine_frontier_with_frontiers(_F1_1, [], []).
combine_frontier_with_frontiers(F1_1, [F2_1|More_F2], [F3_1|More_F3]):-
        combine_2_simple_frontiers(F1_1, F2_1, F3_1),
        combine_frontier_with_frontiers(F1_1, More_F2, More_F3).

% combine_frontiers_aux_2(C1,C2,C3) joins in C3 clauses C1 and C2
combine_2_simple_frontiers(F1_1, F2_1, F3_1) :-
	frontier_contents(F1_1, H_F11, B_F11, FTest_F11),
	frontier_contents(F2_1, H_F21, B_F21, FTest_F21),
	cneg_aux:append(B_F11, B_F21, B_F31),
	cneg_aux:append(FTest_F11, FTest_F21, FTest_F31),
	frontier_contents(F3_1, (H_F11, H_F21), B_F31, FTest_F31),
	!.
 
% negate_frontier(Frontier,Goal,LSolutions) returns in LSolutions
% a list with the same number of elements of the list Frontier.
% Each element of LSolutions will be one solution of negating the 
% conjuction that is in the same position in Frontier. The predicate 
% is a "map" where the fuction that is applied to each element of
% Frontier (each conjunction) is the negation.
% Frontier is the frontier of subgoals of deep 1 of Goal and we need
% it to keep the variables of the Goal and obtain the unifications
negate_frontier(Frontier, Goal, Solutions) :-
	debug_msg_nl(0),
%	debug_msg_list('negate_frontier :: Frontier (list)', Frontier), 
	debug_msg(0, 'negate_frontier :: Goal', Goal), 
	!,
	varsbag_local(Goal, [], [], GoalVars),
	debug_msg(0, 'negate_frontier :: GoalVars', GoalVars), 
	negate_frontier_aux(Frontier, Goal, GoalVars, Solutions),
	!.
%	debug_msg(0, 'negate_frontier :: Solutions', Solutions).

negate_frontier_aux([], _Goal, _GoalVars, true) :- !.
negate_frontier_aux([Frontier | More_Frontier], Goal, GoalVars, Sol):-
	frontier_contents(Frontier, Head, Body, _FrontierTest),
%	debug_msg(0, 'negate_frontier_aux: (Subfr, Goal, GoalVars)', ((Head, Body), Goal, GoalVars)),
	negate_subfrontier((Head, Body), Goal, GoalVars, Sol_Subfr),
%	debug_msg(0, 'negate_frontier_aux: Sol_Subfr', Sol_Subfr),
	!, % Reduce the stack's memory.
	negate_frontier_aux(More_Frontier, Goal, GoalVars, Sol_More_Subfr),
	combine_negated_subfrontiers(Sol_Subfr, Sol_More_Subfr, Sol), !.
	

% combine_negated_subfrontiers(Sol_Subfr, Sol_More_Subfr, Sol_Tmp),
combine_negated_subfrontiers(fail, _Sol_More_Subfr, fail) :- !.
combine_negated_subfrontiers(_Sol_Subfr, fail, fail) :- !.
combine_negated_subfrontiers(true, Sol_More_Subfr, Sol_More_Subfr) :- !.
combine_negated_subfrontiers(Sol_Subfr, true, Sol_Subfr) :- !.
combine_negated_subfrontiers(Sol_Subfr, Sol_More_Subfr, (Sol_Subfr, Sol_More_Subfr)) :- !.

% negate_subfrontier((Head, BodyList),Goal,GoalVars,SolSubfr) 
% returns in SolSubfr a solution of the negation of the conjunction of subgoals 
% (Head, BodyList) of the goal Goal.
% A conjunction is a pair (Head, BodyList).
% It fails if the negation of the conjunction has no solutions
negate_subfrontier((_Head, [fail]), _G, _GoalVars, true):- !.
negate_subfrontier(SubFrontier, Goal, GoalVars, NegatedFrontier):-
%	debug_msg(0, 'negate_subfrontier :: SubFrontier', SubFrontier),
	split_subfrontier_into_Ci_Bi(SubFrontier, Goal, Ci, Bi),
	!, % Reduce the stack's memory.
%	debug_msg(0, 'negate_subfrontier :: split_subfrontier_into_Ci_Bi', (Ci, Bi)),
	negate_subfrontier_aux(GoalVars, Ci, Bi, NegatedFrontier),
%	debug_msg(0, 'negate_subfrontier :: NegatedFrontier', NegatedFrontier),
	!.

% negate_subfrontier_aux(C, G, GoalVars, I, D, R, SolC)
%negate_subfrontier_aux(_C, _Goal, _GoalVars, [fail], _D, _R, []) :-
%	debug_msg(0, 'negate_subfrontier_aux', 'I = [fail] so the negation is always true.'),
%	!. % Backtracking is not allowed.
negate_subfrontier_aux(_GoalVars, [], [], fail) :-
%	debug_msg(0, 'negate_subfrontier_aux', 'Ci adn Bi are empty lists'),
	!. % Backtracking is not allowed.

negate_subfrontier_aux(GoalVars, [], Bi_In, cneg_aux(Bi_Conj, UnivVars)) :- !,
	varsbag_local(Bi_In, GoalVars, [], UnivVars),
	list_to_conj(Bi_In, Bi_Conj).

negate_subfrontier_aux(GoalVars, Ci_In, [], Answer) :- !,
	negate_Ci(Ci_In, GoalVars, Answer).

negate_subfrontier_aux(GoalVars, Ci_In, Bi_In, Answer) :- !,
	negate_Ci(Ci_In, GoalVars, Ci_Negated),
	varsbag_local(Ci_In, [], [], Ci_Vars),
	varsbag_local(Bi_In, GoalVars, [], Bi_FreeVars),
%	debug_msg(0, 'negate_subfrontier_aux :: Bi_FreeVars', Bi_FreeVars),
	varsbag_difference(Bi_FreeVars, Ci_Vars, UnivVars),
%	debug_msg(0, 'negate_subfrontier_aux :: UnivVars', UnivVars),

	list_to_conj(Ci_In, Ci_Conj),
	list_to_conj(Bi_In, Bi_Conj),
	cneg_aux_equality(Answer, (Ci_Negated 
			; (
			      remove_universal_quantification(Ci_Vars, Any_UQ), 
			      Ci_Conj, 
			      keep_universal_quantification(Any_UQ),
			      cneg_aux(Bi_Conj, UnivVars)))).

negate_Ci([], _GoalVars, fail) :- !.
negate_Ci([Ci], GoalVars, Answer):-
	negate_Ci_aux(Ci, GoalVars, Answer).
negate_Ci([Ci|More_Ci], GoalVars, (Answer ; More_Answer)):-
	negate_Ci_aux(Ci, GoalVars, Answer),
	negate_Ci(More_Ci, GoalVars, More_Answer).

negate_Ci_aux(Ci, GoalVars, Answer) :-
	goal_is_equality(Ci, T1, T2),
 	varsbag_local(cneg_eq(T1,T2), GoalVars, [], FreeVars),
	cneg_aux_equality(Answer, cneg_diseq(T1,T2, FreeVars)).

negate_Ci_aux(Ci, _GoalVars, Answer) :-
	goal_is_disequality(Ci, T1, T2, _FreeVars),
	cneg_aux_equality(Answer, cneg_eq(T1,T2)).

list_to_conj([], []) :- fail.
list_to_conj([Ci_In], Ci_In) :- !.
list_to_conj([Ci_In | More_Ci_In], (Ci_In, More_Ci_Conj)) :- !,
	list_to_conj(More_Ci_In, More_Ci_Conj).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_subfrontier_into_Ci_Bi((Head, BodyList), Goal_Copy, Ci, Bi):-
	copy_term((Head, BodyList), (Head_Copy, BodyList_Copy)),
	unify_goal_structure_into_head(Goal_Copy, Head_Copy),
	split_body_into_ci_Bi([cneg_eq(Goal_Copy, Head_Copy) | BodyList_Copy], [], [], Ci, Bi).
%	debug_msg(0, 'split_body_into_ci_Bi', ([Goal_Copy = Head_Copy | BodyList_Copy], I, D, R)).

% unify_goal_structure_into_head(Goal_Copy, Head_Copy)
unify_goal_structure_into_head(Goal_Copy, Head_Copy) :-
	copy_term(Goal_Copy, Goal_Structure),
	unify_goal_structure_into_head_aux(Head_Copy, Goal_Structure).
unify_goal_structure_into_head_aux(X, X).

% Split the input body into Constraints = and =/= and Body.
split_body_into_ci_Bi([], Ci, B, Ci, B) :- !.
split_body_into_ci_Bi([SubGoal], Ci_In, B_In, Ci_Out, B_Out) :- !,
	split_subgoal_into_ci_Bi(SubGoal, Ci_In, B_In, Ci_Out, B_Out).
split_body_into_ci_Bi([SubGoal|SubGoal_L], Ci_In, B_In, Ci_Out, B_Out) :- !,
	split_subgoal_into_ci_Bi(SubGoal, Ci_In, B_In, Ci_Aux, B_Aux), !,
	split_body_into_ci_Bi(SubGoal_L, Ci_Aux, B_Aux, Ci_Out, B_Out), !.

% split_subgoal_into_ci_Bi(SubGoal, Ci, B) :-
split_subgoal_into_ci_Bi(Subgoal, Ci, B, [NewSubgoal | Ci], B) :- 
	goal_is_equality(Subgoal, Term1, Term2), !,
	cneg_aux_equality(NewSubgoal, cneg_eq(Term1, Term2)).

split_subgoal_into_ci_Bi(Subgoal, Ci, B, [NewSubgoal | Ci], B) :- 
	goal_is_disequality(Subgoal, Term1, Term2, FreeVars), !,
	cneg_aux_equality(NewSubgoal, cneg_diseq(Term1, Term2, FreeVars)).

split_subgoal_into_ci_Bi(SubGoal, Ci, B, Ci, [SubGoal | B]) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% call_combined_solutions(LSolutions) combines all its elements that are the solutions
% of the negation of the conjunctions of the Frontier to obtain a 
% solution of the negation of the main Goal. The rest of solutions will
% be obtained by backtracking.
call_combined_solutions(X) :-
%	debug_msg(0, 'call_combined_solutions', X),
	call_combined_solutions_conjunction(X).

%call_combined_solutions_conjunction(Any) :-
%	debug_msg(0, 'call_combined_solutions_conjunction', Any),
%	fail.
call_combined_solutions_conjunction([]) :- !.
call_combined_solutions_conjunction([Sol]) :- !,
	perform_a_call_to(Sol).
call_combined_solutions_conjunction([Sol|More_Sols]) :- !,
	perform_a_call_to(Sol),
	call_combined_solutions_conjunction(More_Sols).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is for making the calls to the predicates
% in the main program.
perform_a_call_to(Goal) :- 
	goal_clean_up(Goal, NewGoal), !,
	perform_a_call_to(NewGoal).

perform_a_call_to(Goal) :-
	goal_is_disequality(Goal, X, Y, FreeVars), !,
	cneg_diseq(X, Y, FreeVars).

perform_a_call_to(Goal) :-
	goal_is_equality(Goal, X, Y), !,
	cneg_aux_equality(X, Y).

% Qualify the predicate as needed.
perform_a_call_to(Goal) :-
	debug_msg(0, 'perform_a_call_to(Goal)', Goal),
	functor_local(Goal, Goal_Name, Arity, Arguments),
	cneg_processed_pred(Goal_Name, Arity, SourceFileName, _Occurences),
	name(SourceFileName, SourceFileName_String),
	name(Goal_Name, Goal_Name_String),
	qualify_string_name(SourceFileName_String, Goal_Name_String, NewGoal_Name_String),
	name(NewGoal_Name, NewGoal_Name_String),
	functor_local(NewGoal, NewGoal_Name, Arity, Arguments),
	term_to_meta(NewGoal, NewGoal_Meta),
	call(NewGoal_Meta).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


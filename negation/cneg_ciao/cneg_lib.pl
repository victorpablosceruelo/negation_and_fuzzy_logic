%
% From Susana modified by VPC (started 29/06/2010)
%

:- module(cneg_lib, [cneg_lib_aux/3, cneg_eq/2,	negate_subfrontier/4], [assertions]).
% NOT NEEDED:  perform_a_call_to/1
:- meta_predicate cneg(goal).
%:- meta_predicate cneg_processed_pred(goal,?). 

% To access predicates from anywhere.
:- multifile cneg_processed_pred/4.
:- multifile cneg_dynamic_cl/6.
:- multifile cneg_static_cl/3.

:- use_module(cneg_aux, _).
:- use_module(cneg_diseq,[cneg_diseq/3]).
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
%	debug('cneg_lib :: using cneg_static for', Goal), 
%	cneg_static(Goal, Result).

cneg_lib_aux(Goal, UnivVars, Result):-
%	UnivVars \== [],
	debug('cneg_lib :: using cneg_dynamic for', Goal), 
	cneg_dynamic(Goal, UnivVars, Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Be carefull: the following code produces random errors.
%cneg_static(_Goal) :-
%	cneg_static_pred(Goal, SourceFileName, Occurences), 
%	debug('cneg_static_pred', (Goal, SourceFileName, Occurences)),
%	fail.

cneg_static(Goal, (Result_G1, Result_G2)) :-
	goal_is_conjunction(Goal, G1, G2), !,
%	debug('cneg_static', goal_is_conjunction(Goal, G1, G2)),
	cneg_static(G1, Result_G1),
	cneg_static(G2, Result_G2).

cneg_static(Goal, Result) :-
	goal_clean_up(Goal, NewGoal), !,
%	debug('cneg_static', goal_clean_up(Goal, NewGoal)),
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
%	debug('cneg_dynamic :: Goal', Goal),
	cneg_dynamic_aux(Goal, UnivVars, Solution),
	debug('cneg_dynamic :: Goal', Goal),
	debug('cneg_dynamic :: Solution', Solution),
	!. % No backtracking allowed

cneg_dynamic_aux(Goal, UnivVars, Solution) :-
	varsbag_local(Goal, UnivVars, [], GoalVars),
	frontier(Goal, Frontier, Goal_Not_Qualified), 
	debug_list('cneg_dynamic :: Frontier (list)', Frontier),
%	debug('cneg_dynamic :: (GoalVars, UnivVars)', (GoalVars, UnivVars)), 
	copy_term((Goal_Not_Qualified, GoalVars), (Goal_Copy, GoalVars_Copy)),
	%copy_term((Goal_Not_Qualified, GoalVars, UnivVars), (Goal_Copy, GoalVars_Copy, UnivVars_Copy)),
	!, % No backtracking allowed
	negate_frontier(Frontier, Goal_Copy, GoalVars_Copy, Solution),
	!, % No backtracking allowed
	% Unify NewGoal with the Head of the Clause we are playing with ...
	% debug('cneg_dynamic', unify_terms(Goal_Not_Qualified, Goal_Copy)),
	unify_terms(Goal_Not_Qualified, Goal_Copy),
	!. % No backtracking allowed

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% frontier(Goal,Frontier) 
% obtains in Frontier the frontier of the  goal Goal.
% It is a list of list that represent the disjunction of its
% elements where each element is a conjunction of subgoals.

% First remove $ and qualification from the goal's name.
frontier(Goal, Frontier, NewGoal) :-
	goal_clean_up(Goal, Tmp_Goal), !,
	frontier(Tmp_Goal, Frontier, NewGoal).

% Now go for the disjunctions.
frontier(Goal, Frontier, (NewG1; NewG2)):- 
	goal_is_disjunction(Goal, G1, G2), !,
	frontier(G1, F1, NewG1),
	frontier(G2, F2, NewG2),
	cneg_aux:append(F1, F2, Front),
	debug('frontier :: disjunction', Front),
	simplify_frontier(Front, (NewG1;NewG2), Frontier).

% Now go for the conjunctions.
frontier(Goal, Frontier, (NewG1, NewG2)):- 
	goal_is_conjunction(Goal, G1, G2), !,
	frontier(G1, F1, NewG1),
	frontier(G2, F2, NewG2),
% Creo q esta fallando aqui ...
	combine_frontiers(F1, F2, Front),
	debug('frontier :: conjunction', Front),
	simplify_frontier(Front, (NewG1,NewG2), Frontier).

% Now go for the functors for equality and disequality.
frontier(Goal, [Frontier], NewGoal):- 
	goal_is_disequality(Goal, X, Y, FreeVars), !,
	cneg_eq(NewGoal, (cneg_diseq(X, Y, FreeVars))),
	frontier_contents(Frontier, NewGoal, [NewGoal], [NewGoal]).

frontier(Goal, [Frontier], NewGoal):- 
	goal_is_equality(Goal, X, Y), !,
	cneg_eq(NewGoal, (X = Y)),
	frontier_contents(Frontier, NewGoal, [NewGoal], [NewGoal]).


%frontier((X=_Y), [(X=X,[])], (X=_Y)):- !.
%frontier((X==Y),[(X==Y,[X==Y])]):- !.
%frontier((X\==Y),[(X\==Y,[X\==Y])]):- !.

% Now go for other functors stored in our database.
frontier(Goal, Front, Goal):-
	debug('frontier :: Goal', Goal),
	look_for_the_relevant_clauses(Goal, Front_Tmp),
	debug('frontier :: format', '(Head, Body, FrontierTest)'),
	debug_list('frontier_IN', Front_Tmp),
	simplify_frontier(Front_Tmp, Goal, Front),
	debug('frontier_OUT', Front), 
	!. % Frontier is uniquely determine if this clause is used.

% And at last report an error if it was impossible to found a valid entry.
frontier(Goal, [], Goal) :-
	debug('ERROR: frontier can not be evaluated for', Goal), 
	nl, nl, !, fail.

% simplify_frontier(Front,Frontier) simplifies the frontier Front.
simplify_frontier(Front_In, G, Front_Out) :-
	debug_nl,
	debug('simplify_frontier :: Front_In', Front_In),
	debug('simplify_frontier :: Goal', G),
	simplify_frontier_aux(Front_In, G, Front_Out),
	debug('simplify_frontier :: Front_Out', Front_Out),
	debug_nl.

simplify_frontier_aux([], _G, []) :- !.
simplify_frontier_aux([Frontier | More_Frontier_In], G, [Frontier | More_Frontier_Out]):-
	test_frontier_is_valid(Frontier, G), !,
	simplify_frontier(More_Frontier_In, G, More_Frontier_Out).
simplify_frontier_aux([_Frontier|More_Frontier_In], G, More_Frontier_Out):-
	simplify_frontier(More_Frontier_In, G, More_Frontier_Out).

% simplify_frontier_unifying_variables(H, Body_In, G, Body_Out) 
% returns in Body_Out the elements of Body whose head unifies with G.
test_frontier_is_valid(Frontier, Goal):-
	frontier_contents(Frontier, Head, _Body, FrontierTest),
	debug('test_frontier_is_valid(Head, FrontierTest, Goal)', (Head, FrontierTest, Goal)),
        copy_term((Head, FrontierTest), (H_Tmp, FrontierTest_Tmp)), 
        copy_term(Goal, G_Tmp),
        cneg_eq(H_Tmp, G_Tmp), 
	call_combined_solutions(FrontierTest_Tmp), 
	debug('test_frontier_is_valid', 'YES'),
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
negate_frontier(Frontier, Goal, GoalVars, Solutions) :-
	debug_list('negate_frontier :: Frontier (list)', Frontier), 
	debug('negate_frontier :: (Goal, GoalVars)', (Goal, GoalVars)), 
	!,
	negate_frontier_aux(Frontier, Goal, GoalVars, Solutions),
	!.
%	debug('negate_frontier :: Solutions', Solutions).

negate_frontier_aux([], _Goal, _GoalVars, true) :- !.
negate_frontier_aux([Frontier | More_Frontier], Goal, GoalVars, Sol):-
	frontier_contents(Frontier, Head, Body, _FrontierTest),
	debug('negate_frontier_aux: (Subfr, Goal, GoalVars)', ((Head, Body), Goal, GoalVars)),
	negate_subfrontier((Head, Body), Goal, GoalVars, Sol_Subfr),
	debug('negate_frontier_aux: Sol_Subfr', Sol_Subfr),
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
	debug('negate_subfrontier :: SubFrontier', SubFrontier),
	split_subfrontier_into_Ci_Bi(SubFrontier, Goal, Ci, Bi),
	!, % Reduce the stack's memory.
	debug('negate_subfrontier :: split_subfrontier_into_Ci_Bi', (Ci, Bi)),
	negate_subfrontier_aux(GoalVars, Ci, Bi, NegatedFrontier),
	debug('negate_subfrontier :: NegatedFrontier', NegatedFrontier),
	!.

% negate_subfrontier_aux(C, G, GoalVars, I, D, R, SolC)
%negate_subfrontier_aux(_C, _Goal, _GoalVars, [fail], _D, _R, []) :-
%	debug('negate_subfrontier_aux', 'I = [fail] so the negation is always true.'),
%	!. % Backtracking is not allowed.
negate_subfrontier_aux(_GoalVars, [], [], fail) :-
	debug('negate_subfrontier_aux', 'Ci adn Bi are empty lists'),
	!. % Backtracking is not allowed.

negate_subfrontier_aux(GoalVars, [], Bi_In, cneg_aux(Bi_Conj, UnivVars)) :- !,
	varsbag_local(Bi_In, GoalVars, [], UnivVars),
	list_to_conj(Bi_In, Bi_Conj).

negate_subfrontier_aux(GoalVars, Ci_In, [], Answer) :- !,
	negate_Ci(Ci_In, GoalVars, Answer).

negate_subfrontier_aux(GoalVars, Ci_In, Bi_In, Answer) :- !,
	negate_Ci(Ci_In, GoalVars, Ci_Negated),
	varsbag_local(Ci_In, [], [], Ci_Vars),
	varsbag_local(Bi_In, Ci_Vars, [], UnivVars),
	list_to_conj(Ci_In, Ci_Conj),
	list_to_conj(Bi_In, Bi_Conj),
	cneg_eq(Answer, (Ci_Negated ; (Ci_Conj, cneg_aux(Bi_Conj, [UnivVars])))).

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
	split_body_into_ci_Bi([Goal_Copy = Head_Copy | BodyList_Copy], [], [], Ci, Bi).
%	debug('split_body_into_ci_Bi', ([Goal_Copy = Head_Copy | BodyList_Copy], I, D, R)).

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
	cneg_eq(NewSubgoal, (Term1 = Term2)).

split_subgoal_into_ci_Bi(Subgoal, Ci, B, [NewSubgoal | Ci], B) :- 
	goal_is_disequality(Subgoal, Term1, Term2, FreeVars), !,
	cneg_eq(NewSubgoal, cneg_diseq(Term1, Term2, FreeVars)).

split_subgoal_into_ci_Bi(SubGoal, Ci, B, Ci, [SubGoal | B]) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


negate_Ci([], _GoalVars, fail) :- !.

negate_Ci([Ci|More_Ci], GoalVars, (Answer ; More_Answer)):-
	goal_is_equality(Ci, T1, T2),
 	varsbag_local(cneg_eq(T1,T2), GoalVars, [], FreeVars),
	cneg_eq(Answer, cneg_diseq(T1,T2, FreeVars)),
	negate_Ci(More_Ci, GoalVars, More_Answer).

negate_Ci([Ci|More_Ci], GoalVars, (Answer ; More_Answer)):-
	goal_is_disequality(Ci, T1, T2, _FreeVars),
	cneg_eq(Answer, cneg_eq(T1,T2)),
	negate_Ci(More_Ci, GoalVars, More_Answer).



% call_combined_solutions(LSolutions) combines all its elements that are the solutions
% of the negation of the conjunctions of the Frontier to obtain a 
% solution of the negation of the main Goal. The rest of solutions will
% be obtained by backtracking.
call_combined_solutions(X) :-
%	debug('call_combined_solutions', X),
	call_combined_solutions_conjunction(X).

%call_combined_solutions_conjunction(Any) :-
%	debug('call_combined_solutions_conjunction', Any),
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
	cneg_eq(X, Y).

% Qualify the predicate as needed.
perform_a_call_to(Goal) :-
	debug('perform_a_call_to(Goal)', Goal),
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

cneg_eq(X, Y) :- debug('cneg_lib', cneg_eq(X, Y)), fail.
cneg_eq(X, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% From Susana modified by VPC (started 29/06/2010)
%
:- module(cneg_rt, [cneg_rt/2], [assertions]).
% NOT NEEDED:  perform_a_call_to/1
:- meta_predicate cneg(goal).
%:- meta_predicate cneg_processed_pred(goal,?). 

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.

:- use_module(cneg_aux, _).
:- use_module(cneg_diseq, _).
%:- use_module(library(cneg_diseq),[cneg_diseq/3]).
% Esta linea para cuando cneg sea una libreria.

:- comment(title, "Contructive Negation Library").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module calls the predicates generated during the 
	program transformation.").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_rt(Goal, UnivVars):-
	debug_msg(1, 'cneg_lib :: using cneg_dynamic for', Goal), 
	cneg_dynamic(Goal, UnivVars, Result), 
	debug_msg(1, 'cneg_dynamic :: call :: Result', Result),
	debug_msg_nl(1),
	call(Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cneg_dynamic(Goal,UnivVars) makes the constructive negation of 
% the Goal considering the set of variables GoalVars as the variables
% of the goal and UnivVars the universal quantified variable of it.

cneg_dynamic(Goal, UQV, Result) :-
	debug_msg(1, 'cneg_dynamic :: (Goal, UQV)', (Goal, UQV)),
	varsbag(Goal, UQV, [], GoalVars),
	compute_frontier(Goal, Frontier, Goal_Not_Qualified), 
	!, % The frontier is unique !!!
	debug_msg_list(1, 'cneg_dynamic :: Frontier (list)', Frontier),
	copy_term((Goal_Not_Qualified, GoalVars), (Goal_Copy, GoalVars_Copy)),
	%copy_term((Goal_Not_Qualified, GoalVars, UnivVars), (Goal_Copy, GoalVars_Copy, UnivVars_Copy)),

	negate_frontier(Frontier, Goal_Copy, GoalVars_Copy, Result),
	debug_msg(1, 'cneg_dynamic :: (Goal, UQV, Result)', (Goal, UQV, Result)),

	% Unify NewGoal with the Head of the Clause we are playing with ...
	% debug_msg(1, 'cneg_dynamic', unify_terms(Goal_Not_Qualified, Goal_Copy)),
	unify_terms(Goal_Not_Qualified, Goal_Copy).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% compute_frontier(Goal,Frontier) 
% obtains in Frontier the frontier of the  goal Goal.
% It is a list of list that represent the disjunction of its
% elements where each element is a conjunction of subgoals.

% First remove $ and qualification from the goal's name.
compute_frontier(Goal, Frontier, NewGoal) :-
	goal_clean_up(Goal, Tmp_Goal), !,
	compute_frontier(Tmp_Goal, Frontier, NewGoal).

% Now go for the disjunctions.
compute_frontier(Goal, Frontier, (NewG1; NewG2)):- 
	goal_is_disjunction(Goal, G1, G2), !,
	compute_frontier(G1, F1, NewG1),
	compute_frontier(G2, F2, NewG2),
	cneg_aux:append(F1, F2, Front),
	debug_msg(1, 'frontier :: disjunction', Front),
	simplify_frontier(Front, (NewG1;NewG2), Frontier).

% Now go for the conjunctions.
compute_frontier(Goal, Frontier, (NewG1, NewG2)):- 
	goal_is_conjunction(Goal, G1, G2), !,
	compute_frontier(G1, F1, NewG1),
	compute_frontier(G2, F2, NewG2),
% Creo q esta fallando aqui ...
	combine_frontiers(F1, F2, Front),
	debug_msg(1, 'frontier :: conjunction', Front),
	simplify_frontier(Front, (NewG1,NewG2), Frontier).

% Now go for the functors for equality and disequality.
compute_frontier(Goal, [Frontier], NewGoal):- 
	goal_is_disequality(Goal, X, Y, FreeVars), !,
	equality(NewGoal, (disequality(X, Y, FreeVars)), []),
	frontier_contents(Frontier, NewGoal, [NewGoal], [NewGoal]).

compute_frontier(Goal, [Frontier], NewGoal):- 
	goal_is_equality(Goal, X, Y, UQV), !,
	equality(NewGoal, (X = Y), UQV),
	frontier_contents(Frontier, NewGoal, [NewGoal], [NewGoal]).

% Now go for other functors stored in our database.
compute_frontier(Goal, Front, Goal):-
	debug_msg(1, 'compute_frontier :: Goal', Goal),
	look_for_the_relevant_clauses(Goal, Front_Tmp),
	debug_msg(1, 'compute_frontier :: format', '(Head, Body, FrontierTest)'),
%	debug_msg_list(1, 'compute_frontier :: frontier', Front_Tmp),
	simplify_frontier(Front_Tmp, Goal, Front),
%	debug_msg(1, 'frontier_OUT', Front), 
	!. % Frontier is uniquely determined if this clause is used.

% And at last report an error if it was impossible to found a valid entry.
compute_frontier(Goal, [], Goal) :-
	debug_msg(1, 'ERROR: frontier can not be evaluated for', Goal), 
	nl, nl, !, fail.

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
	setof_local(frontier(Head, Body, FrontierTest), 
	cneg_pre_frontier(Name, Arity, _SourceFileName, Head, Body, FrontierTest), Frontier).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% simplify_frontier(Front,Frontier) simplifies the frontier Front.
simplify_frontier(Front_In, G, Front_Out) :-
	debug_msg_nl(0),
	debug_msg_list(1, 'simplify_frontier :: Front_In', Front_In),
	debug_msg(0, 'simplify_frontier :: Goal', G),
	simplify_frontier_aux(Front_In, G, Front_Out),
	debug_msg(0, 'simplify_frontier :: Front_Out', Front_Out),
	debug_msg_nl(0).

simplify_frontier_aux([], _G, []) :- !.
simplify_frontier_aux([Frontier | More_Frontier_In], G, [Frontier | More_Frontier_Out]):-
	test_frontier_is_valid(Frontier, G), !,
	simplify_frontier(More_Frontier_In, G, More_Frontier_Out).
simplify_frontier_aux([_Frontier|More_Frontier_In], G, More_Frontier_Out):-
%	debug_msg(1, 'simplify_frontier_aux :: rejected frontier: ', Frontier),
	simplify_frontier(More_Frontier_In, G, More_Frontier_Out).

% simplify_frontier_unifying_variables(H, Body_In, G, Body_Out) 
% returns in Body_Out the elements of Body whose head unifies with G.
test_frontier_is_valid(Frontier, Goal):-
	frontier_contents(Frontier, Head, Body, _FrontierTest_Aux),
	list_head(Body, Test), 
	debug_msg(1, 'test_frontier_is_valid :: (Head, Test, Goal)', (Head, Test, Goal)),
        copy_term((Head, Test, Goal), (Head_Tmp, Test_Tmp, Goal_Tmp)), 
        equality(Head_Tmp, Goal_Tmp, []), 
	goal_is_equality(Test_Tmp, Test_Left, Test_Right, UQV), % It must be an equality.
	equality(Test_Left, Test_Right, UQV), % Note that UQV = [].
%	call_combined_solutions(FrontierTest_Tmp), 
	debug_msg(1, 'test_frontier_is_valid', 'YES'),
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
	debug_msg_list(1, 'negate_frontier :: Frontier (list)', Frontier), 
	debug_msg(1, 'negate_frontier :: (Goal, GoalVars)', (Goal, GoalVars)), 
	!,
	negate_frontier_aux(Frontier, Goal, GoalVars, Solutions),
	!.
%	debug_msg(1, 'negate_frontier :: Solutions', Solutions).

negate_frontier_aux([], _Goal, _GoalVars, true) :- !.
negate_frontier_aux([Frontier | More_Frontier], Goal, GoalVars, Sol):-
	frontier_contents(Frontier, Head, Body, _FrontierTest),
	debug_msg(1, 'negate_frontier_aux: (Subfr, Goal, GoalVars)', ((Head, Body), Goal, GoalVars)),
	negate_subfrontier((Head, Body), Goal, GoalVars, Sol_Subfr),
	debug_msg(1, 'negate_frontier_aux: Sol_Subfr', Sol_Subfr),
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
negate_subfrontier(C, Goal, GoalVars, SolC):-
%	debug_msg(1, 'negate_subfrontier :: (Head, Body)', (C)),
	split_subfrontier_into_I_D_R(C, Goal, I, D, R),
	!, % Reduce the stack's memory.
%	debug_msg(1, 'negate_subfrontier', split_subfrontier_into_I_D_R(C,Goal,GoalVars,I,D,R)),
	negate_subfrontier_aux(GoalVars, I, D, R, SolC),
%	debug_msg(1, 'negate_subfrontier :: ((Head, Body), SolC)', ((C), SolC)),
	!.

% negate_subfrontier_aux(C, G, GoalVars, I, D, R, SolC)
%negate_subfrontier_aux(_C, _Goal, _GoalVars, [fail], _D, _R, []) :-
%	debug_msg(1, 'negate_subfrontier_aux', 'I = [fail] so the negation is always true.'),
%	!. % Backtracking is not allowed.
negate_subfrontier_aux(_GoalVars, [], [], [], fail) :-
	debug_msg(1, 'negate_subfrontier_aux', 'I, D and R are empty lists'),
	!. % Backtracking is not allowed.
negate_subfrontier_aux(GoalVars, I_In, D_In, R_In, SolC) :-
%	debug_msg(1, 'negate_subfrontier_aux :: (GoalVars)', (GoalVars)),
	normalize_I_D_R(I_In, D_In, R_In, GoalVars, I_Tmp, D_Tmp, R_Tmp, ImpVars, ExpVars), 
%	debug_msg(1, 'negate_subfrontier_aux :: (I_Tmp, D_Tmp, R_Tmp)', (I_Tmp, D_Tmp, R_Tmp)),
%	debug_msg(1, 'negate_subfrontier_aux :: ImpVars (vars(I) + GoalVars)', ImpVars),
%	debug_msg(1, 'negate_subfrontier_aux :: ExpVars (vars(R) - ImpVars)', ExpVars),
	split_D_R_into_imp_and_exp(D_Tmp, R_Tmp, ImpVars, ExpVars, Dimp, Rimp, DRexp),
	!,

	% Final process
%	debug_msg(1, 'negate_subfrontier_aux :: (I, Dimp, Rimp, DRexp)', (I_Tmp, Dimp, Rimp, DRexp)), 
	negate_formulae(GoalVars, ExpVars, I_Tmp, Dimp, Rimp, DRexp, SolC),
%	debug_msg(1, 'negate_subfrontier_aux :: SolC', SolC),
	!.

% split_subfrontier_into_I_D_R(C,Goal,I,D,R) 
% returns from the subgoals of C:
% in I the equalities.
% in D the disequalities.
% in R whatever that is not an equality or an inequality.
split_subfrontier_into_I_D_R((Head, BodyList), Goal_Copy, I, D, R):-
	copy_term((Head, BodyList), (Head_Copy, BodyList_Copy)),
	unify_goal_structure_into_head(Goal_Copy, Head_Copy),
	split_body_into_I_D_R([Goal_Copy = Head_Copy | BodyList_Copy], [], [], [], I, D, R).
%	debug_msg(1, 'split_body_into_I_D_R', ([Goal_Copy = Head_Copy | BodyList_Copy], I, D, R)).

% unify_goal_structure_into_head(Goal_Copy, Head_Copy)
unify_goal_structure_into_head(Goal_Copy, Head_Copy) :-
	copy_term(Goal_Copy, Goal_Structure),
	unify_goal_structure_into_head_aux(Head_Copy, Goal_Structure).
unify_goal_structure_into_head_aux(X, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_body_into_I_D_R([], I, D, R, I, D, R) :- !.
split_body_into_I_D_R([SubGoal], I_In, D_In, R_In, I_Out, D_Out, R_Out) :- !,
	split_subgoal_into_I_D_R(SubGoal, I_In, D_In, R_In, I_Out, D_Out, R_Out).
split_body_into_I_D_R([SubGoal|SubGoal_L], I_In, D_In, R_In, I_Out, D_Out, R_Out) :- !,
	split_subgoal_into_I_D_R(SubGoal, I_In, D_In, R_In, I_Aux, D_Aux, R_Aux), !,
	split_body_into_I_D_R(SubGoal_L, I_Aux, D_Aux, R_Aux, I_Out, D_Out, R_Out), !.

% split_subgoal_into_I_D_R(SubGoal, I, D, R) :-
split_subgoal_into_I_D_R(Subgoal, I, D, R, [NewSubgoal | I], D, R) :- 
	goal_is_equality(Subgoal, Term1, Term2, UQV), !,
	equality(NewSubgoal, (Term1 = Term2), UQV).

split_subgoal_into_I_D_R(Subgoal, I, D, R, I, [NewSubgoal | D], R) :- 
	goal_is_disequality(Subgoal, Term1, Term2, FreeVars), !,
	equality(NewSubgoal, disequality(Term1, Term2, FreeVars), []).

split_subgoal_into_I_D_R(SubGoal, I, D, R, I, D, [SubGoal | R]) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% normalize_I_D_R(I,D,R,GoalVars, In,Dn,Rn,ImpVars,ExVars) 
% returns In and Dn that are the equalities of I and the disequalities
% of D but after normalizating them.
normalize_I_D_R(I_In, D_In, R_In, GoalVars, I_Out, D_Out, R_Out, ImpVars, ExpVars):-
	remove_from_I_irrelevant_equalities(I_In,D_In,R_In,GoalVars,[],I_Tmp,D_Tmp,R_Out),  
	remove_from_I_duplicates(I_Tmp, I_Out),
	varsbag(I_Out, [], GoalVars, ImpVars), % ImpVars = vars(I) + GoalVars
	varsbag(R_Out, GoalVars, [], RelVars), % RelVars = vars(R) - GoalVars
	difference(RelVars, ImpVars, ExpVars), % ExpVars = vars(R) - ImpVars = RelVars - ImpVars
	remove_from_D_irrelevant_disequalities(D_Tmp, ImpVars, RelVars, D_Out). 
 
% difference(Vars,NFVars,FreeVars) retuns in FreeVars the sublist of elements
% of Vars that do not appear in NFVars
difference([],_NFVars,[]).
difference([Var|Vars],NFVars,FreeVars):-
	memberchk(Var,NFVars),!,
	difference(Vars,NFVars,FreeVars).
difference([Var|Vars],NFVars,[Var|FreeVars]):-
	difference(Vars,NFVars,FreeVars).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% remove_from_I_irrelevant_equalities(I,D,R,GoalVars,Iac,Iv,Dv,Rv) 
% removes from I the equalities X=Y that contain a variable, 
% X or Y,that is not in GoalVars and 
% makes the unification of the corresponding value in D and R.
% Iac is use to acumulate the equalities not removed untill the process ends,
% and update the value of I.
remove_from_I_irrelevant_equalities([],D,R,_GoalVars,Iac,Iac,D,R) :- !.
remove_from_I_irrelevant_equalities([(Var=Value)|I],D,R,GoalVars,Iac,Iv,Dv,Rv):-
	var(Var),
	\+ memberchk(Var,GoalVars),!,
	replace_in_term_var_by_value((Iac,I,D,R),Var,Value,(Iac1,I1,D1,R1)),
	remove_from_I_irrelevant_equalities(I1,D1,R1,GoalVars,Iac1,Iv,Dv,Rv).
remove_from_I_irrelevant_equalities([(Var=Value)|I],D,R,GoalVars,Iac,Iv,Dv,Rv):-
	var(Value),
	\+ memberchk(Value,GoalVars),!,
	replace_in_term_var_by_value((Iac,I,D,R),Value,Var,(Iac1,I1,D1,R1)),
	remove_from_I_irrelevant_equalities(I1,D1,R1,GoalVars,Iac1,Iv,Dv,Rv).
remove_from_I_irrelevant_equalities([(Var=Value)|I],D,R,GoalVars,Iac,Iv,Dv,Rv):-
	remove_from_I_irrelevant_equalities(I,D,R,GoalVars,[(Var=Value)|Iac],Iv,Dv,Rv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% remove_from_I_duplicates(L_In,L_Out) removes from I duplicates
% or equalities between the same variable, X=X.
remove_from_I_duplicates([],[]).
remove_from_I_duplicates([X=Y|L_In],L_Out):-
	X==Y,!, 
	remove_from_I_duplicates(L_In,L_Out).
remove_from_I_duplicates([X=Y|L_In],L_Out):-
	memberchk(X=Y,L_In),!,
	remove_from_I_duplicates(L_In,L_Out).
remove_from_I_duplicates([X=Y|L_In],L_Out):-
	memberchk(Y=X,L_In),!, %% Different order
	remove_from_I_duplicates(L_In,L_Out).
remove_from_I_duplicates([E|L_In],[E|L_Out]):-
	remove_from_I_duplicates(L_In,L_Out).

% remove_from_D_irrelevant_disequalities(D,ImpVars,RelVars,D1) returns D1
% that is D but without disequalities that contains any variable that
% is not in ImpVars neither RelVars
% We need to assure that the inequalities here are atomic because 
% non-atomic ones result into a disjunction that can not be simplified
% by using this procedure.
remove_from_D_irrelevant_disequalities([],_ImpVars,_RelVars,[]).
remove_from_D_irrelevant_disequalities([Diseq|D],ImpVars,RelVars,D1):-
	varsbag(Diseq, [], [], Vars),
	retrieve_element_from_list(Vars, V),
	\+ memberchk(V,ImpVars),
	\+ memberchk(V,RelVars),!,
	remove_from_D_irrelevant_disequalities(D,ImpVars,RelVars,D1).
remove_from_D_irrelevant_disequalities([Diseq|D],ImpVars,RelVars,[Diseq|D1]):-
	remove_from_D_irrelevant_disequalities(D,ImpVars,RelVars,D1).

% split_D_R_into_imp_and_exp(I,D,R,GoalVars,ImpVars,ExpVars,SolC) returns SolC
% that is one of the solutions of the conjunction that is divided in 
% I, D and R (equalities, disequalities and rest of subgoals).
% GoalVars, ImpVars and ExpVars are set of useful variables 
split_D_R_into_imp_and_exp(D, R, ImpVars, ExpVars, Dimp, Rimp, DRexp):-
%	debug_msg(1, 'split_D_R_into_imp_and_exp :: ExpVars', ExpVars),
%	debug_msg(1, 'split_D_R_into_imp_and_exp :: D', D),
%	debug_msg(1, 'split_D_R_into_imp_and_exp :: R', R),
	split_formula_between_imp_and_exp(D, ImpVars, ExpVars, [], Dimp, [], Dexp),
	split_formula_between_imp_and_exp(R, ImpVars, ExpVars, [], Rimp, Dexp, DRexp).

% Las 2 siguientes NO estan claras.
%	copy_term(UnivVars,ExistVars),
%	replace_in_term_variables_by_values((Dexp_Tmp,Rexp_Tmp), UnivVars, ExistVars, (Dexp,Rexp)).

% split_formula_between_imp_and_exp(F,ExpVars,Fimp,Fexp) divide F between Fimp and Fexp.
% In Fexp are the elements of F with any variables of ExpVars and
% the rest of elements of F will be in Fimp
split_formula_between_imp_and_exp([], _ImpVars, _ExpVars, Fimp, Fimp, Fexp, Fexp) :- !.
split_formula_between_imp_and_exp([Term|F], ImpVars, ExpVars, Fimp_In, Fimp_Out, Fexp_In, Fexp_Out):-
	varsbag(Term, ImpVars, [], Vars), % Retrieve only vars not in ImpVars.
%	debug_msg(1, 'split_formula_between_imp_and_exp :: Vars', Vars),
	retrieve_element_from_list(Vars, V),
	memberchk(V, ExpVars),!,
	split_formula_between_imp_and_exp(F, ImpVars, ExpVars, Fimp_In, Fimp_Out, [Term|Fexp_In], Fexp_Out).
split_formula_between_imp_and_exp([Term|F], ImpVars, ExpVars, Fimp_In, Fimp_Out, Fexp_In, Fexp_Out):-
	split_formula_between_imp_and_exp(F, ImpVars, ExpVars, [Term|Fimp_In], Fimp_Out, Fexp_In, Fexp_Out).

% negate_formulae(GoalVars, ImpVars, ExpVars, I, Dimp, Dexp, Rimp, Rexp, R_naf, Sol)
% returns SolC that is one of the solutions of the conjunction that is divided
% I, Dimp,Dexp,Rimp and Rexp (equalities, disequalities and rest of subgoals).
% GoalVars, ImpVars and ExpVars are set of useful variables 
negate_formulae(GoalVars, ExpVars, I, Dimp, Rimp, DRexp, Sol):-
	negate_Dexp_Rexp(DRexp, ExpVars, fail, Tmp_Sol_1), 
%	debug_msg(1, 'negate_formulae', negate_Dexp_Rexp(DRexp, ExpVars, fail, Tmp_Sol_1)), 
	negate_Rimp(Rimp, Tmp_Sol_1, Tmp_Sol_2),
%	debug_msg(1, 'negate_formulae', negate_Rimp(Rimp, Tmp_Sol_1, Tmp_Sol_2)),
	negate_Dimp(Dimp, Tmp_Sol_2, Tmp_Sol_3),
%	debug_msg(1, 'negate_formulae', negate_Dimp(Dimp, Tmp_Sol_2, Tmp_Sol_3)),
	negate_I(I, GoalVars, Tmp_Sol_3, Sol), 
%	debug_msg(1, 'negate_formulae', negate_I(I, GoalVars, Tmp_Sol_3, Sol)), 
	!.

% 'fail' is just our terminator, so we remove it (if possible).
combine_negated_element(Neg_Sol, _Keep_Sol, fail, (Neg_Sol)) :- !.
combine_negated_element(Neg_Sol, Keep_Sol, Next_Sol, (Neg_Sol ; (Keep_Sol, Next_Sol))) :- !.

% negate_I(I,GoalVars,Sol) obtains in Sol a solution of negating I
negate_I([], _GoalVars, Prev_Solution, Prev_Solution) :- !.
negate_I([Eq|I], GoalVars, Prev_Solution, Solution):-
	negate_eq(Eq, GoalVars, Neg_Eq, Valid_Eq), 
	combine_negated_element(Neg_Eq, Valid_Eq, Prev_Solution, Tmp_Solution),
	negate_I(I, GoalVars, Tmp_Solution, Solution).

% negate_eq(Eq,GoalVars,Sol) returns in Sol a solution of the 
% negation of the equality Eq 
negate_eq(Goal, GoalVars, disequality(T1,T2, FreeVars), equality(T1,T2, UQV)):-
	goal_is_equality(Goal, T1, T2, UQV),
 	varsbag(equality(T1,T2), GoalVars, [], FreeVars).

% new_Vars(FreeVars,UnivVars) returns in UnivVars so many new variables
% as the number of elements of FreeVars
new_Vars([],[]).
new_Vars([_Term|R],[_New|N]):-
	new_Vars(R,N).

% negate_Dimp(Dimp,SolC) obtain in SolC a solution of negating Dimp.
negate_Dimp([], Prev_Solution, Prev_Solution) :- !.
negate_Dimp([disequality(T1, T2, FreeVars)|Dimp], Prev_Solution, Solution) :-
	combine_negated_element(equality(T1,T2, []), disequality(T1, T2, FreeVars), Prev_Solution, Tmp_Solution),
	negate_Dimp(Dimp, Tmp_Solution, Solution).

% negate_Rimp(Rimp,SolC) obtain in SolC a solution of negating Rimp.
negate_Rimp([], Prev_Solution, Prev_Solution) :- !.
negate_Rimp([R|Rimp], Prev_Solution, Solution) :-
	combine_negated_element(cneg_rt(R, []), R, Prev_Solution, Tmp_Solution),
	negate_Rimp(Rimp, Tmp_Solution, Solution).

% negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,SolC) obtain in
% SolC a solution of negating Dexp y Rexp juntos.
negate_Dexp_Rexp([], _ExpVars, Prev_Solution, Prev_Solution):- !.
negate_Dexp_Rexp(DRexp, ExpVars, Prev_Solution, Solution):-
	get_conjunction(DRexp,DR),
	combine_negated_element((cneg_rt(DR, ExpVars)), DRexp, Prev_Solution, Solution).

% get_conjunction(List,Conj) obtain the conjunction Conj with the
% same elements that List
get_conjunction([X],X).
get_conjunction([X|L],(X,C)):-
	get_conjunction(L,C).

% call_combined_solutions(LSolutions) combines all its elements that are the solutions
% of the negation of the conjunctions of the Frontier to obtain a 
% solution of the negation of the main Goal. The rest of solutions will
% be obtained by backtracking.
call_combined_solutions(X) :-
%	debug_msg(1, 'call_combined_solutions', X),
	call_combined_solutions_conjunction(X).

%call_combined_solutions_conjunction(Any) :-
%	debug_msg(1, 'call_combined_solutions_conjunction', Any),
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
% in the main program. For what?
perform_a_call_to(Goal) :- 
	goal_clean_up(Goal, NewGoal), !,
	perform_a_call_to(NewGoal).

perform_a_call_to(Goal) :-
	goal_is_disequality(Goal, X, Y, FreeVars), !,
	disequality(X, Y, FreeVars).

perform_a_call_to(Goal) :-
	goal_is_equality(Goal, X, Y, UQV), !,
	equality(X, Y, UQV).

% Qualify the predicate as needed.
perform_a_call_to(Goal) :-
	debug_msg(1, 'perform_a_call_to(Goal)', Goal),
	functor_local(Goal, Goal_Name, Arity, Arguments),
	cneg_pre_frontier(Goal_Name, Arity, SourceFileName, _Head_Aux, _Body_Aux, _FrontierTest_Aux),
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


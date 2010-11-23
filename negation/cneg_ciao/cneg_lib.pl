%
% From Susana modified by VPC (started 29/06/2010)
%
:- module(cneg_lib, [cneg_lib_aux/3, cneg_eq/2,	negate_subfrontier/4]).
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
	simplify_frontier(Front, (NewG1;NewG2), Frontier).

% Now go for the conjunctions.
frontier(Goal, Frontier, (NewG1, NewG2)):- 
	goal_is_conjunction(Goal, G1, G2), !,
	frontier(G1, F1, NewG1),
	frontier(G2, F2, NewG2),
	lists_distributive_conjunction(F1, F2, Front),
	simplify_frontier(Front, (NewG1,NewG2), Frontier).

% Now go for the functors for equality and disequality.
frontier(Goal, [(cneg_diseq(X, Y, FreeVars),[cneg_diseq(X, Y, FreeVars)])], cneg_diseq(X, Y, FreeVars)):- 
	goal_is_disequality(Goal, X, Y, FreeVars), !.
frontier(Goal, [(cneg_eq(X,Y),[cneg_eq(X,Y)])], cneg_eq(X,Y)):- 
	goal_is_equality(Goal, X, Y), !.

%frontier((X=_Y), [(X=X,[])], (X=_Y)):- !.
%frontier((X==Y),[(X==Y,[X==Y])]):- !.
%frontier((X\==Y),[(X\==Y,[X\==Y])]):- !.

% Now go for other functors stored in our database.
frontier(Goal, Front, Goal):-
	look_for_the_relevant_clauses(Goal, Front_Tmp),
	%debug(frontier(Goal), Front_Tmp),
	simplify_frontier(Front_Tmp, Goal, Front),
%	debug('frontier :: Front_3 (simplified cls)', Front), 
	!. % Frontier is uniquely determine if this clause is used.

% And at last report an error if it was impossible to found a valid entry.
frontier(Goal, [], Goal) :-
	debug('ERROR: frontier can not be evaluated for', Goal), 
	nl, nl, !, fail.

% simplify_frontier(Front,Frontier) simplifies the frontier Front.
simplify_frontier([], _G, []) :- !.
simplify_frontier([(Head, Body, FrontierTest)|Frontier_In], G, [(Head, Body)|Frontier_Out]):-
	test_frontier_is_valid(Head, FrontierTest, G), !,
	simplify_frontier(Frontier_In, G, Frontier_Out).
simplify_frontier([(_Head, _Body)|Frontier_In], G, Frontier_Out):-
	simplify_frontier(Frontier_In, G, Frontier_Out).

% simplify_frontier_unifying_variables(H, Body_In, G, Body_Out) 
% returns in Body_Out the elements of Body whose head unifies with G.
test_frontier_is_valid(Head, FrontierTest, Goal):-
        copy_term((Head, FrontierTest), (H_Tmp, FrontierTest_Tmp)), 
        copy_term(Goal, G_Tmp),
        cneg_eq(H_Tmp, G_Tmp), 
	call_combined_solutions(FrontierTest_Tmp), 
	!.

% lists_distributive_conjunction(F1,F2,F3) returns F3 that is the lists_distributive_conjunction of the list 
% of lists F1 and F2 after using the distributive rule
lists_distributive_conjunction([],_F2,[]):-!.
lists_distributive_conjunction([Conj|F1],F2,F5):-
        lists_distributive_conjunction_aux_1(F2,Conj,F3),
        lists_distributive_conjunction(F1,F2,F4),
        cneg_aux:append(F3,F4,F5).

% lists_distributive_conjunction_aux_1(L1,E,L2) returns L2 that is 
% the list obtained after using the distributive rule to 
% the list of list L1 with respect the list E. 
% L1 and L2 are disjunction of conjunctions and E is a conjunction.
lists_distributive_conjunction_aux_1([],_Conj,[]).
lists_distributive_conjunction_aux_1([Conj2|L],Conj1,[Conj3|L3]):-
        lists_distributive_conjunction_aux_2(Conj1,Conj2,Conj3),
        lists_distributive_conjunction_aux_1(L,Conj1,L3).

% lists_distributive_conjunction_aux_2(C1,C2,C3) joins in C3 clauses C1 and C2
lists_distributive_conjunction_aux_2((H1,B1),(H2,B2),((H1,H2),B3)):-
	cneg_aux:append(B1,B2,B3).
 
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
negate_frontier_aux([Subfr|Frontier], Goal, GoalVars, Sol):-
	debug('negate_frontier_aux: (Subfr, Goal, GoalVars)', (Subfr, Goal, GoalVars)),
	negate_subfrontier(Subfr, Goal, GoalVars, Sol_Subfr),
	debug('negate_frontier_aux: Sol_Subfr', Sol_Subfr),
	!, % Reduce the stack's memory.
	negate_frontier_aux(Frontier, Goal, GoalVars, Sol_More_Subfr),
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
%	debug('negate_subfrontier :: (Head, Body)', (C)),
	split_subfrontier_into_I_D_R(C, Goal, I, D, R),
	!, % Reduce the stack's memory.
%	debug('negate_subfrontier', split_subfrontier_into_I_D_R(C,Goal,GoalVars,I,D,R)),
	negate_subfrontier_aux(GoalVars, I, D, R, SolC),
%	debug('negate_subfrontier :: ((Head, Body), SolC)', ((C), SolC)),
	!.

% negate_subfrontier_aux(C, G, GoalVars, I, D, R, SolC)
%negate_subfrontier_aux(_C, _Goal, _GoalVars, [fail], _D, _R, []) :-
%	debug('negate_subfrontier_aux', 'I = [fail] so the negation is always true.'),
%	!. % Backtracking is not allowed.
negate_subfrontier_aux(_GoalVars, [], [], [], fail) :-
	debug('negate_subfrontier_aux', 'I, D and R are empty lists'),
	!. % Backtracking is not allowed.
negate_subfrontier_aux(GoalVars, I_In, D_In, R_In, SolC) :-
%	debug('negate_subfrontier_aux :: (GoalVars)', (GoalVars)),
	normalize_I_D_R(I_In, D_In, R_In, GoalVars, I_Tmp, D_Tmp, R_Tmp, ImpVars, ExpVars), 
%	debug('negate_subfrontier_aux :: (I_Tmp, D_Tmp, R_Tmp)', (I_Tmp, D_Tmp, R_Tmp)),
%	debug('negate_subfrontier_aux :: ImpVars (vars(I) + GoalVars)', ImpVars),
%	debug('negate_subfrontier_aux :: ExpVars (vars(R) - ImpVars)', ExpVars),
	split_D_R_into_imp_and_exp(D_Tmp, R_Tmp, ImpVars, ExpVars, Dimp, Rimp, DRexp),
	!,

	% Final process
%	debug('negate_subfrontier_aux :: (I, Dimp, Rimp, DRexp)', (I_Tmp, Dimp, Rimp, DRexp)), 
	negate_formulae(GoalVars, ExpVars, I_Tmp, Dimp, Rimp, DRexp, SolC),
%	debug('negate_subfrontier_aux :: SolC', SolC),
	!.

% split_subfrontier_into_I_D_R(C,Goal,I,D,R) 
% returns from the subgoals of C:
% in I the equalities.
% in D the disequalities.
% in R whatever that is not an equality or an inequality.
split_subfrontier_into_I_D_R((Head, BodyList), Goal_Copy, I, D, R):-
	copy_term((Head, BodyList), (Head_Copy, BodyList_Copy)),
	unify_goal_structure_into_head(Goal_Copy, Head_Copy),
	split_body_into_I_D_R([Goal_Copy = Head_Copy | BodyList_Copy], I, D, R),
	debug('split_body_into_I_D_R', ([Goal_Copy = Head_Copy | BodyList_Copy], I, D, R)).

% unify_goal_structure_into_head(Goal_Copy, Head_Copy)
unify_goal_structure_into_head(Goal_Copy, Head_Copy) :-
	copy_term(Goal_Copy, Goal_Structure),
	unify_goal_structure_into_head_aux(Head_Copy, Goal_Structure).
unify_goal_structure_into_head_aux(X, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_body_into_I_D_R([], [], [], []) :- !.
split_body_into_I_D_R([SubGoal], I, D, R) :- !,
	split_subgoal_into_I_D_R(SubGoal, I, D, R).
split_body_into_I_D_R([SubGoal|SubGoal_L], I, D, R) :- !,
	split_subgoal_into_I_D_R(SubGoal, I_1, D_1, R_1),
	split_body_into_I_D_R(SubGoal_L, I_2, D_2, R_2), !,
	cneg_aux:append(I_1, I_2, I), 
	cneg_aux:append(D_1, D_2, D),
	cneg_aux:append(R_1, R_2, R).

% split_subgoal_into_I_D_R(SubGoal, I, D, R) :-
split_subgoal_into_I_D_R(Subgoal, I, [], []) :- 
	goal_is_equality(Subgoal, Term1, Term2), !,
	get_equalities_from_unifying_terms(Term1, Term2, I).
split_subgoal_into_I_D_R(cneg_diseq(T1, T2, FreeVars), [], [cneg_diseq(T1, T2, FreeVars)], []) :- !.
split_subgoal_into_I_D_R(SubGoal, [], [], [SubGoal]) :- !.

% get_equalities_from_head(Term1, Term2, Head_I) 
get_equalities_from_unifying_terms(Term1, Term2, [Term1 = Term2]) :-
	var(Term1),
	var(Term2), !.

get_equalities_from_unifying_terms(Term1, Term2, [Term1 = Term2]) :-
	var(Term1), !.

get_equalities_from_unifying_terms(Term1, Term2, [Term2  = Term1]) :-
	var(Term2), !.

get_equalities_from_unifying_terms(Term1, Term2, I) :-
	functor(Term1, Name, Arity), 
	functor(Term2, Name, Arity), 
	Term1=..[Name|Args_Term1],
	Term2=..[Name|Args_Term2], !,
	get_equalities_from_unifying_terms_args(Args_Term1, Args_Term2, I).

get_equalities_from_unifying_terms_args([], [], []) :- !.
get_equalities_from_unifying_terms_args([Term1], [Term2], I) :- !,
	get_equalities_from_unifying_terms(Term1, Term2, I).
get_equalities_from_unifying_terms_args([Term1|Term1_L], [Term2|Term2_L], I) :- !,
	get_equalities_from_unifying_terms(Term1, Term2, I_1),
	get_equalities_from_unifying_terms_args(Term1_L, Term2_L, I_2),
	!, % Background not allowed.
	cneg_aux:append(I_1, I_2, I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% normalize_I_D_R(I,D,R,GoalVars, In,Dn,Rn,ImpVars,ExVars) 
% returns In and Dn that are the equalities of I and the disequalities
% of D but after normalizating them.
normalize_I_D_R(I_In, D_In, R_In, GoalVars, I_Out, D_Out, R_Out, ImpVars, ExpVars):-
	remove_from_I_irrelevant_equalities(I_In,D_In,R_In,GoalVars,[],I_Tmp,D_Tmp,R_Out),  
	remove_from_I_duplicates(I_Tmp, I_Out),
	varsbag_local(I_Out, [], GoalVars, ImpVars), % ImpVars = vars(I) + GoalVars
	varsbag_local(R_Out, GoalVars, [], RelVars), % RelVars = vars(R) - GoalVars
	difference(RelVars, ImpVars, ExpVars), % ExpVars = vars(R) - ImpVars = RelVars - ImpVars
	remove_from_D_irrelevant_disequalities(D_Tmp, ImpVars, RelVars, D_Out). 
 
% difference(Vars,NFVars,FreeVars) retuns in FreeVars the sublist of elements
% of Vars that do not appear in NFVars
difference([],_NFVars,[]).
difference([Var|Vars],NFVars,FreeVars):-
	memberchk_local(Var,NFVars),!,
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
	\+ memberchk_local(Var,GoalVars),!,
	replace_in_term_var_by_value((Iac,I,D,R),Var,Value,(Iac1,I1,D1,R1)),
	remove_from_I_irrelevant_equalities(I1,D1,R1,GoalVars,Iac1,Iv,Dv,Rv).
remove_from_I_irrelevant_equalities([(Var=Value)|I],D,R,GoalVars,Iac,Iv,Dv,Rv):-
	var(Value),
	\+ memberchk_local(Value,GoalVars),!,
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
	memberchk_local(X=Y,L_In),!,
	remove_from_I_duplicates(L_In,L_Out).
remove_from_I_duplicates([X=Y|L_In],L_Out):-
	memberchk_local(Y=X,L_In),!, %% Different order
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
	varsbag_local(Diseq, [], [], Vars),
	retrieve_element_from_list(Vars, V),
	\+ memberchk_local(V,ImpVars),
	\+ memberchk_local(V,RelVars),!,
	remove_from_D_irrelevant_disequalities(D,ImpVars,RelVars,D1).
remove_from_D_irrelevant_disequalities([Diseq|D],ImpVars,RelVars,[Diseq|D1]):-
	remove_from_D_irrelevant_disequalities(D,ImpVars,RelVars,D1).

% split_D_R_into_imp_and_exp(I,D,R,GoalVars,ImpVars,ExpVars,SolC) returns SolC
% that is one of the solutions of the conjunction that is divided in 
% I, D and R (equalities, disequalities and rest of subgoals).
% GoalVars, ImpVars and ExpVars are set of useful variables 
split_D_R_into_imp_and_exp(D, R, ImpVars, ExpVars, Dimp, Rimp, DRexp):-
%	debug('split_D_R_into_imp_and_exp :: ExpVars', ExpVars),
%	debug('split_D_R_into_imp_and_exp :: D', D),
%	debug('split_D_R_into_imp_and_exp :: R', R),
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
	varsbag_local(Term, ImpVars, [], Vars), % Retrieve only vars not in ImpVars.
%	debug('split_formula_between_imp_and_exp :: Vars', Vars),
	retrieve_element_from_list(Vars, V),
	memberchk_local(V, ExpVars),!,
	split_formula_between_imp_and_exp(F, ImpVars, ExpVars, Fimp_In, Fimp_Out, [Term|Fexp_In], Fexp_Out).
split_formula_between_imp_and_exp([Term|F], ImpVars, ExpVars, Fimp_In, Fimp_Out, Fexp_In, Fexp_Out):-
	split_formula_between_imp_and_exp(F, ImpVars, ExpVars, [Term|Fimp_In], Fimp_Out, Fexp_In, Fexp_Out).

% negate_formulae(GoalVars, ImpVars, ExpVars, I, Dimp, Dexp, Rimp, Rexp, R_naf, Sol)
% returns SolC that is one of the solutions of the conjunction that is divided
% I, Dimp,Dexp,Rimp and Rexp (equalities, disequalities and rest of subgoals).
% GoalVars, ImpVars and ExpVars are set of useful variables 
negate_formulae(GoalVars, ExpVars, I, Dimp, Rimp, DRexp, Sol):-
	negate_Dexp_Rexp(DRexp, ExpVars, fail, Tmp_Sol_1), 
%	debug('negate_formulae', negate_Dexp_Rexp(DRexp, ExpVars, fail, Tmp_Sol_1)), 
	negate_Rimp(Rimp, Tmp_Sol_1, Tmp_Sol_2),
%	debug('negate_formulae', negate_Rimp(Rimp, Tmp_Sol_1, Tmp_Sol_2)),
	negate_Dimp(Dimp, Tmp_Sol_2, Tmp_Sol_3),
%	debug('negate_formulae', negate_Dimp(Dimp, Tmp_Sol_2, Tmp_Sol_3)),
	negate_I(I, GoalVars, Tmp_Sol_3, Sol), 
%	debug('negate_formulae', negate_I(I, GoalVars, Tmp_Sol_3, Sol)), 
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
negate_eq(Goal, GoalVars, cneg_diseq(T1,T2, FreeVars), cneg_eq(T1,T2)):-
	goal_is_equality(Goal, T1, T2),
 	varsbag_local(cneg_eq(T1,T2), GoalVars, [], FreeVars).

% new_Vars(FreeVars,UnivVars) returns in UnivVars so many new variables
% as the number of elements of FreeVars
new_Vars([],[]).
new_Vars([_Term|R],[_New|N]):-
	new_Vars(R,N).

% negate_Dimp(Dimp,SolC) obtain in SolC a solution of negating Dimp.
negate_Dimp([], Prev_Solution, Prev_Solution) :- !.
negate_Dimp([cneg_diseq(T1, T2, FreeVars)|Dimp], Prev_Solution, Solution) :-
	combine_negated_element(cneg_eq(T1,T2), cneg_diseq(T1, T2, FreeVars), Prev_Solution, Tmp_Solution),
	negate_Dimp(Dimp, Tmp_Solution, Solution).

% negate_Rimp(Rimp,SolC) obtain in SolC a solution of negating Rimp.
negate_Rimp([], Prev_Solution, Prev_Solution) :- !.
negate_Rimp([R|Rimp], Prev_Solution, Solution) :-
	combine_negated_element(cneg_aux(R, []), R, Prev_Solution, Tmp_Solution),
	negate_Rimp(Rimp, Tmp_Solution, Solution).

% negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,SolC) obtain in
% SolC a solution of negating Dexp y Rexp juntos.
negate_Dexp_Rexp([], _ExpVars, Prev_Solution, Prev_Solution):- !.
negate_Dexp_Rexp(DRexp, ExpVars, Prev_Solution, Solution):-
	get_conjunction(DRexp,DR),
	combine_negated_element((cneg_aux(DR, ExpVars)), DRexp, Prev_Solution, Solution).

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


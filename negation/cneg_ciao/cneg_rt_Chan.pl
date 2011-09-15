%
% From Susana modified by VPC.
%
:- module(cneg_rt_Chan, [cneg_rt_Chan/2], [assertions]).
% NOT NEEDED:  perform_a_call_to/1
:- meta_predicate cneg(goal).
%:- meta_predicate cneg_processed_pred(goal,?). 

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.
:- multifile call_to/1.

:- use_module(cneg_aux, _).
:- use_module(cneg_diseq, [diseq_uqv/3, eq_uqv/3, diseq_eqv/3, eq_eqv/3, 
	cneg_diseq_uqv/4, cneg_eq_uqv/4, cneg_diseq_eqv/4, cneg_eq_eqv/4]).
:- use_module(library(aggregates),[setof/3]).
%:- use_module(library(cneg_diseq),[cneg_diseq/3]).
% Esta linea para cuando cneg sea una libreria.

:- comment(title, "Contructive Negation Runtime Library - Chan's Proposal").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module implements Chan's proposal of Constructive Negation.").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_rt_Chan(Goal, UQV):-
	debug_msg(1, 'cneg_rt :: (Goal, UQV)', (Goal, UQV)),
	varsbag(Goal, UQV, [], GoalVars),
	compute_frontier(Goal, Frontier),
	negate_set_of_frontiers(Frontier, Goal, GoalVars, Solutions),
	call(Solutions).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% compute_neg_frontier(Goal,Frontier) 
% obtains in Frontier the frontier of the  goal Goal.
% It is a list of list that represent the disjunction of its
% elements where each element is a conjunction of subgoals.

% Just to debug.
compute_frontier(Goal, _Frontier) :-
	debug_msg(1, '--------------------------------------------------------------------------------------------------------------', ' '),
	debug_msg(1, 'compute_frontier :: (Goal)', (Goal)),	
	fail. % Just debug and use backtracking to continue.

% First remove $ and qualification from the goal's name.
compute_frontier(Goal, Frontier) :-
	goal_clean_up(Goal, Tmp_Goal), !,
	compute_frontier(Tmp_Goal, Frontier).

% Manage true and fail ...
compute_frontier('true', ['true']) :- !.
compute_frontier('fail', ['fail']) :- !.

% Now go for the disjunctions.
compute_frontier(Goal, Frontier):- 
	goal_is_disjunction(Goal, G1, G2), !,
	(
	    compute_frontier(G1, Frontier)
	;
	    compute_frontier(G2, Frontier)
	).

% Now go for the conjunctions.
compute_frontier(Goal, Frontier):- 
	goal_is_conjunction(Goal, G1, G2), !,
	compute_frontier(G1, Frontier_G1),
	compute_frontier(G2, Frontier_G2),
	combine_frontiers(Frontier_G1, Frontier_G2, Frontier).

% Now go for the functors for equality and disequality.
compute_frontier(Goal, Goal):- 
	goal_is_disequality(Goal, _T1, _T2, _UQV), !.

compute_frontier(Goal, Goal):- 
	goal_is_equality(Goal, _T1, _T2, _UQV), !.

% Double negation is not managed yet. Forget it.
compute_frontier(Goal, Goal):- 
	goal_is_negation(Goal, _SubGoal, _UQV), !.

% Now go for other functors stored in our database.
compute_frontier(Goal, Frontier_Out) :-
	goal_is_not_conj_disj_eq_diseq_dneg(Goal),
	debug_msg(1, 'compute_frontier :: Goal', Goal),
	look_for_the_relevant_clauses(Goal, Frontier_Tmp_1),
	debug_msg(1, 'compute_neg_frontier :: format', '(Head, Body, FrontierTest)'),
	debug_msg_list(1, 'Frontier_Tmp_1', Frontier_Tmp_1),
	simplify_frontier(Frontier_Tmp_1, Goal, Frontier_Out),
	debug_msg(1, 'Frontier_Out', Frontier_Out), 
	!. % Backtracking is forbidden.

% And at last report an error if it was impossible to found a valid entry.
compute_frontier(Goal, [true]) :-
	debug_msg(1, 'ERROR: compute_neg_frontier :: (Goal)', (Goal)), 
	nl, !. % Backtracking is forbidden.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Structure to manage all the info about the frontier in an easy way.
frontier_contents(frontier(Head, Body, FrontierTest), Head, Body, FrontierTest).

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
simplify_frontier([], _Goal, []) :- !.
simplify_frontier([Frontier | More_Frontier_In], Goal, [Body | More_Bodies]):-
	frontier_contents(Frontier, Head, Body, Frontier_Test),
	test_frontier_is_valid(Goal, Head, Frontier_Test), !,
	debug_msg(1, 'simplify_frontier :: valid: ', Frontier),
	simplify_frontier(More_Frontier_In, Goal, More_Bodies).
simplify_frontier([Frontier|More_Frontier_In], Goal, More_Bodies):-
	debug_msg(1, 'simplify_frontier :: not valid: ', Frontier),
	simplify_frontier(More_Frontier_In, Goal, More_Bodies).

% simplify_frontier_unifying_variables(H, Body_In, G, Body_Out) 
% returns in Body_Out the elements of Body whose head unifies with G.
test_frontier_is_valid(Goal, Head, Frontier_Test) :-
	debug_msg(1, 'test_frontier_is_valid :: (Goal, Head, FrontierTest)', (Goal, Head, Frontier_Test)),
        copy_term((Goal, Head, Frontier_Test), (Goal_Tmp, Head_Tmp, Frontier_Test_Tmp)), 
        eq_uqv(Head_Tmp, Goal_Tmp, []), 
	goal_is_equality(Frontier_Test_Tmp, Tmp_Left, Tmp_Right, Tmp_UQV), % It must be an equality.
	eq_uqv(Tmp_Left, Tmp_Right, Tmp_UQV), % Note that UQV = [].
%	call_combined_solutions(FrontierTest_Tmp), 
	!, % Backtracking forbidden.
	eq_uqv(Goal, Head, []). % Unify them definitely
%	functor_local(Goal, _Name, _Arity, Goal_Args), 
%	goal_is_equality(Frontier_Test, Test_Left, _Test_Right, _Test_UQV),
%	eq_uqv(Test_Left, Goal_Args, []). % Note that UQV = [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
combine_frontier_aux(F1_1, [F2_1 | More_F2], [(F1_1, F2_1) | More_F3]):-
        combine_frontier_aux(F1_1, More_F2, More_F3).

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
negate_set_of_frontiers(Frontier, Goal, GoalVars, Solutions) :-
	debug_msg_list(1, 'negate_frontier :: Frontier (list)', Frontier), 
	debug_msg(1, 'negate_frontier :: (Goal, GoalVars)', (Goal, GoalVars)), 
	!,
	negate_set_of_frontiers_aux(Frontier, Goal, GoalVars, Solutions),
	!.
%	debug_msg(1, 'negate_frontier :: Solutions', Solutions).

negate_set_of_frontiers_aux([], _Goal, _GoalVars, true) :- !.
negate_set_of_frontiers_aux([Frontier | More_Frontier], Goal, GoalVars, Sol):-
	debug_msg(1, 'negate_frontier_aux: (Frontier, Goal, GoalVars)', (Frontier, Goal, GoalVars)),
	negate_frontier(Frontier, Goal, GoalVars, Sol_Subfr),
	debug_msg(1, 'negate_frontier_aux: Sol_Subfr', Sol_Subfr),
	!, % Reduce the stack's memory.
	negate_set_of_frontiers_aux(More_Frontier, Goal, GoalVars, Sol_More_Subfr),
	combine_negated_frontiers(Sol_Subfr, Sol_More_Subfr, Sol), 
	!. % Reduce the stack's memory.
	

% combine_negated_subfrontiers(Sol_Subfr, Sol_More_Subfr, Sol_Tmp),
combine_negated_frontiers(fail, _Sol_More_Subfr, fail) :- !.
combine_negated_frontiers(_Sol_Subfr, fail, fail) :- !.
combine_negated_frontiers(true, Sol_More_Subfr, Sol_More_Subfr) :- !.
combine_negated_frontiers(Sol_Subfr, true, Sol_Subfr) :- !.
combine_negated_frontiers(Sol_Subfr, Sol_More_Subfr, (Sol_Subfr, Sol_More_Subfr)) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% negate_frontier((Head, BodyList),Goal,GoalVars,SolSubfr) 
% returns in SolSubfr a solution of the negation of the conjunction of subgoals 
% (Head, BodyList) of the goal Goal.
% A conjunction is a pair (Head, BodyList).
% It fails if the negation of the conjunction has no solutions
% negate_frontier((_Head, [fail]), _G, _GoalVars, true):- !.
negate_frontier(Frontier, _Goal, GoalVars, Result):-
	debug_msg(1, 'negate_frontier :: Frontier', (Frontier)),
	split_frontier_into_E_IE_NIE(Frontier, [], [], [], E, IE, NIE),
	debug_msg(1, 'negate_frontier :: (E, IE, NIE)', (E, IE, NIE)),
	!, % Reduce the stack's memory.
	normalize_E_IE_NIE(E, IE, NIE, GoalVars, E_Aux, IE_Aux, NIE_Aux, ImpVars, ExpVars), 
	split_E_IE_between_imp_and_exp(IE_Aux, NIE_Aux, ImpVars, ExpVars, IE_imp, NIE_imp, IE_NIE_exp),
	negate_formulae(GoalVars, ExpVars, E_Aux, IE_imp, NIE_imp, IE_NIE_exp, Result),
	debug_msg(1, 'negate_frontier :: (Result)', (Result)),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_frontier_into_E_IE_NIE(Frontier, E, IE, NIE, E, IE, NIE) :-
	goal_is_disjunction(Frontier, _G1, _G2), !, 
	debug_msg(1, 'ERROR: split_frontier_into_E_IE_NIE can not deal with disjunctions. Frontier', Frontier),
	fail.

split_frontier_into_E_IE_NIE(Frontier, E_In, IE_In, NIE_In, E_Out, IE_Out, NIE_Out) :-
	goal_is_conjunction(Frontier, G1, G2), !,
	split_frontier_into_E_IE_NIE(G1, E_In, IE_In, NIE_In, E_Aux, IE_Aux, NIE_Aux),
	split_frontier_into_E_IE_NIE(G2, E_Aux, IE_Aux, NIE_Aux, E_Out, IE_Out, NIE_Out).

split_frontier_into_E_IE_NIE(Frontier, E_In, IE, NIE, E_Out, IE, NIE) :- 
	goal_is_equality(Frontier, Term1, Term2, UQV), !,
	add_goal_to_conjunction(eq_uqv(Term1, Term2, UQV), E_In, E_Out).

split_frontier_into_E_IE_NIE(Frontier, E, IE_In, NIE, E, IE_Out, NIE) :- 
	goal_is_disequality(Frontier, Term1, Term2, FreeVars), !,
	add_goal_to_conjunction(disequality(Term1, Term2, FreeVars), IE_In, IE_Out).

split_frontier_into_E_IE_NIE(Frontier, E, IE, NIE_In, E, IE, NIE_Out) :- 
	goal_is_not_conj_disj_eq_diseq_dneg(Frontier), !,
	add_goal_to_conjunction(Frontier, NIE_In, NIE_Out).

split_frontier_into_E_IE_NIE(Frontier, E, IE, NIE, E, IE, NIE) :- 
	debug_msg(1, 'ERROR: split_frontier_into_E_IE_NIE can not deal with frontier. Frontier', Frontier),
	fail.

add_goal_to_conjunction(Frontier, [], Frontier) :- !.
add_goal_to_conjunction(Frontier_1, Frontier_2, (Frontier_2, Frontier_1)) :- 
	Frontier_2 \== [], !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	formulae_contents(formulae(E_In, IE_In, NIE_In), E_In, IE_In, NIE_In),

% normalize_I_D_R(I,D,R,GoalVars, In,Dn,Rn,ImpVars,ExVars) 
% returns In and Dn that are the equalities of I and the disequalities
% of D but after normalizating them.
normalize_E_IE_NIE(E_In, IE_In, NIE_In, GoalVars, E_Out, IE_Out, NIE_Out, ImpVars, ExpVars):-
	formulae_contents(Formulae_In, E_In, IE_In, NIE_In),
	varsbag(GoalVars, [], [], Real_GoalVars), % Sometimes we have non vars in GoalVars.
	varsbag(E_In, Real_GoalVars, [], Vars_EnoG), % Vars_EnoG = vars(E) - GoalVars
	remove_from_E_irrelevant_equalities(Formulae_In, [], Vars_EnoG, Formulae_Aux),  
	remove_from_E_duplicates(E_Aux, E_Out),
	varsbag(E_Out, [], Real_GoalVars, ImpVars), % ImpVars = vars(E) + GoalVars
	varsbag(NIE_Out, Real_GoalVars, [], RelVars), % RelVars = vars(NIE) - GoalVars
	varsbag_difference(RelVars, ImpVars, ExpVars), % ExpVars = vars(NIE) - ImpVars = RelVars - ImpVars
	remove_from_IE_irrelevant_disequalities(IE_Aux, ImpVars, RelVars, IE_Out). 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% remove_from_I_irrelevant_equalities(I,D,R,GoalVars,Iac,Iv,Dv,Rv) 
% removes from I the equalities X=Y that contain a variable, 
% X or Y,that is not in GoalVars and 
% makes the unification of the corresponding value in D and R.
% Iac is use to acumulate the equalities not removed untill the process ends,
% and update the value of I.
remove_from_E_irrelevant_equalities(Formulae_In, _E_Acc, _Vars_EnoG, Formulae_In) :- 
	formulae_contents(Formulae_In, [], _IE_In, _NIE_In), !. % There are no equalities.

remove_from_E_irrelevant_equalities(Formulae_In, E_Acc, _Vars_EnoG, Formulae_In) :-
	formulae_contents(Formulae_In, E_In, IE_In, NIE_In), 
[(Var=Value)|E],E_Acc,IE,NIE,Vars_EnoG, E_Out, IE_Out, NIE_Out):-
	
	goal_is_equality(
	(
	    
	var(Var),
	memberchk(Var, Vars_EnoG), !, % Vars_EnoG = vars(E) - GoalVars
	replace_in_term_var_by_value((E,E_Acc,IE,NIE), Var, Value, (E_Aux,E_Acc_Aux,IE_Aux,NIE_Aux)),
	remove_from_E_irrelevant_equalities(E_Aux,E_Acc_Aux,IE_Aux,NIE_Aux,Vars_EnoG,E_Out, IE_Out, NIE_Out).
remove_from_E_irrelevant_equalities([(Var=Value)|E],E_Acc,IE,NIE,Vars_EnoG,E_Out, IE_Out, NIE_Out):-
	var(Value),
	memberchk(Value, Vars_EnoG), !, % Vars_EnoG = vars(E) - GoalVars
	replace_in_term_var_by_value((E,E_Acc,IE,NIE), Value, Var, (E_Aux,E_Acc_Aux,IE_Aux,NIE_Aux)),
	remove_from_E_irrelevant_equalities(E_Aux,E_Acc_Aux,IE_Aux,NIE_Aux,Vars_EnoG,E_Out, IE_Out, NIE_Out).
remove_from_E_irrelevant_equalities([(Var=Value)|E],E_Acc,IE,NIE,Vars_EnoG,E_Out,IE_Out, NIE_Out):-
	remove_from_E_irrelevant_equalities(E,[(Var=Value)|E_Acc],IE,NIE,Vars_EnoG,E_Out,IE_Out, NIE_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% remove_from_I_duplicates(L_In,L_Out) removes from I duplicates
% or equalities between the same variable, X=X.
remove_from_E_duplicates([],[]).
remove_from_E_duplicates([X=Y|L_In],L_Out):-
	X==Y,!, 
	remove_from_E_duplicates(L_In,L_Out).
remove_from_E_duplicates([X=Y|L_In],L_Out):-
	memberchk(X=Y,L_In),!,
	remove_from_E_duplicates(L_In,L_Out).
remove_from_E_duplicates([X=Y|L_In],L_Out):-
	memberchk(Y=X,L_In),!, %% Different order
	remove_from_E_duplicates(L_In,L_Out).
remove_from_E_duplicates([E|L_In],[E|L_Out]):-
	remove_from_E_duplicates(L_In,L_Out).

% remove_from_D_irrelevant_disequalities(D,ImpVars,RelVars,D1) returns D1
% that is D but without disequalities that contains any variable that
% is not in ImpVars neither RelVars
% We need to assure that the inequalities here are atomic because 
% non-atomic ones result into a disjunction that can not be simplified
% by using this procedure.
remove_from_IE_irrelevant_disequalities([],_ImpVars,_RelVars,[]).
remove_from_IE_irrelevant_disequalities([Diseq|D],ImpVars,RelVars,D1):-
	varsbag(Diseq, [], [], Vars),
	retrieve_element_from_list(Vars, V),
	\+ memberchk(V,ImpVars),
	\+ memberchk(V,RelVars),!,
	remove_from_IE_irrelevant_disequalities(D,ImpVars,RelVars,D1).
remove_from_IE_irrelevant_disequalities([Diseq|D],ImpVars,RelVars,[Diseq|D1]):-
	remove_from_IE_irrelevant_disequalities(D,ImpVars,RelVars,D1).

% split_D_R_into_imp_and_exp(I,D,R,GoalVars,ImpVars,ExpVars,SolC) returns SolC
% that is one of the solutions of the conjunction that is divided in 
% I, D and R (equalities, disequalities and rest of subgoals).
% GoalVars, ImpVars and ExpVars are set of useful variables 
split_E_IE_between_imp_and_exp(D, R, ImpVars, ExpVars, Dimp, Rimp, DRexp):-
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
	diseq_uqv(X, Y, FreeVars).

perform_a_call_to(Goal) :-
	goal_is_equality(Goal, X, Y, UQV), !,
	eq_uqv(X, Y, UQV).

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


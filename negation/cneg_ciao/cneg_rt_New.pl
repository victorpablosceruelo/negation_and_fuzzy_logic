
:- module(cneg_rt_New, [negate_subfrontier_new/4], [assertions]).
:- meta_predicate cneg(goal).
%:- meta_predicate cneg_processed_pred(goal,?). 

:- use_module(cneg_aux, _).
:- use_module(cneg_rt_aux_frontiers, 
	[  subfrontier_contents/5, 
	   subfrontier_E_IE_NIE_contents/4,
	   subfrontier_E_IE_NIE_ie_contents/6,
	   split_subfrontier_into_E_IE_NIE/2,
	   rebuild_conjunction_of_goals/3,
	   split_IE_NIE_between_imp_and_exp/3
	]).
:- use_module(cneg_diseq, 
	[ 
% 	    equality/3, disequality/3,
	    diseq_geuqv/5, eq_geuqv/5,
	    diseq_geuqv_adv/6, eq_geuqv_adv/6,
 	    prepare_attributes_for_printing/2,
	    cneg_diseq_echo/4
	]).


%:- use_module(library(cneg_diseq),[cneg_diseq/3]).
% Esta linea para cuando cneg sea una libreria.

:- comment(title, "Contructive Negation Runtime Library - New Proposal").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module implements our new proposal for Constructive Negation.").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% negate_subfrontier(SubFrontier, GoalVars, Proposal, Result_Frontier)
% returns in Result_Frontier the negation of the subfrontier in SubFrontier

% negate_subfrontier_new(SubFrontier_In, GoalVars, Proposal, (Result)):-
negate_subfrontier_new(SubFrontier_In, GoalVars, 'cneg_rt_New', (Result)):-
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier :: SubFrontier_In', (SubFrontier_In)),
	split_subfrontier_into_E_IE_NIE(SubFrontier_In, SubFrontier_Aux_1),
	!, % Reduce the stack's memory by forbidding backtracking.
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier', SubFrontier_Aux_1),
	compute_expvars(SubFrontier_Aux_1, GoalVars, ExpVars),
	split_IE_NIE_between_imp_and_exp(SubFrontier_Aux_1, ExpVars, SubFrontier_Aux_3),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier', SubFrontier_Aux_3),
	!, % Reduce the stack's memory by forbidding backtracking.
	negate_formula(SubFrontier_Aux_3, GoalVars, Result),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier :: (Result)', (Result)),
	!. % Reduce the stack's memory by forbidding backtracking.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% In our proposal the variables in ExpVars are those which can be grounded in the future, 
% so we need to wait for them to be ground. This are the variables in literals not evaluated yet
% and the variables in equalities (this ones will be grounded for sure, but we prefer to wait).
compute_expvars(SubFrontier_In, GoalVars_In, ExpVars) :-
	varsbag(GoalVars_In, [], [], GoalVars), % Only vars, please.
	subfrontier_E_IE_NIE_contents(SubFrontier_In, E, IE, NIE),

	identify_closed_vars(E, [], Closed_Vars_E),
	identify_closed_vars(IE, Closed_Vars_E, Closed_Vars_E_IE),
	identify_closed_vars(NIE, Closed_Vars_E_IE, Closed_Vars_E_IE_NIE), 
	varsbag(GoalVars, [], Closed_Vars_E_IE_NIE, Closed_Vars), % This are the non-local variables.

	varsbag(E, Closed_Vars, [], Local_Vars_E), % Vars_E = vars(E) - Closed_Vars
	% varsbag(IE, Closed_Vars, [], Local_Vars_IE), % Vars_IE = vars(IE) - Closed_Vars
	varsbag(NIE, Closed_Vars, [], Local_Vars_NIE),  % Vars_NIE = vars(NIE) - Closed_Vars

	varsbag_union(Local_Vars_E, Local_Vars_NIE, ExpVars).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

identify_closed_vars([], Closed_Vars, Closed_Vars) :- !. % It can be empty.
identify_closed_vars(Frontier, Closed_Vars_In, Closed_Vars_Out) :- % Conjunctions
	goal_is_conjunction(Frontier, Frontier_Left, Frontier_Right), !,
	identify_closed_vars(Frontier_Left, Closed_Vars_In, Closed_Vars_Aux),
	identify_closed_vars(Frontier_Right, Closed_Vars_Aux, Closed_Vars_Out).

identify_closed_vars(Frontier, Closed_Vars_In, Closed_Vars_Out) :- % Equalities
	goal_is_equality(Frontier, _Value_1, _Value_2, _GV, _EQV, UQV_In),
	varsbag_clean_up(UQV_In, UQV),
	varsbag_union(UQV, Closed_Vars_In, Closed_Vars_Out).

identify_closed_vars(Frontier, Closed_Vars_In, Closed_Vars_Out) :- % Disequalities
	goal_is_disequality(Frontier, _Term1, _Term2, _GV, _EQV, UQV_In), !,
	varsbag_clean_up(UQV_In, UQV),
	varsbag_union(UQV, Closed_Vars_In, Closed_Vars_Out).

identify_closed_vars(Frontier, Closed_Vars_In, Closed_Vars_Out) :- % Negations
	goal_is_cneg_rt(Frontier, UQV_In, _GoalVars_In, _SubGoal, _Negation_Proposal), !,
	varsbag_clean_up(UQV_In, UQV),
	varsbag_union(UQV, Closed_Vars_In, Closed_Vars_Out).

identify_closed_vars(Frontier, Closed_Vars_In, Closed_Vars_In) :- % Other subgoals
	goal_is_not_conj_disj_eq_diseq_dneg(Frontier), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% negate_formula(Frontier, Proposal, GoalVars, UQV, ImpVars, ExpVars, UQ_to_EQ_Vars, Dumb_Vars, Result)
% returns Result that is the result from negating the frontier.
negate_formula(Frontier, _GoalVars, true) :- 
	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	subfrontier_E_IE_NIE_ie_contents(Frontier, [], [], [], [], []),
	!. % Optimization

negate_formula(Frontier, GoalVars, Neg_E_IE_NIE) :-
	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	subfrontier_E_IE_NIE_ie_contents(Frontier, E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp),

	rebuild_conjunction_of_goals(IE_Exp, NIE_Exp, IE_NIE_Exp), 
	rebuild_conjunction_of_goals(E, IE_Imp, E_IE_Imp),
	rebuild_conjunction_of_goals(E_IE_Imp, NIE_Imp, Formulae_Imp),

	echo_msg(2, '', 'cneg_rt', 'negate_formula :: Formulae_Imp', Formulae_Imp),
	echo_msg(2, '', 'cneg_rt', 'negate_formula :: IE_NIE_Exp', IE_NIE_Exp),
 	negate_IE_NIE_exp(IE_NIE_Exp, GoalVars, Neg_IE_NIE_Exp),
	negate_imp_form(Formulae_Imp, GoalVars, Neg_IE_NIE_Exp, Neg_E_IE_NIE),
	!. % Backtracking forbidden.

% negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,SolC) obtain in
% SolC a solution of negating Dexp y Rexp juntos.
negate_IE_NIE_exp([], _GoalVars, []):- !.
negate_IE_NIE_exp(IE_NIE_exp, GoalVars, Neg_IE_NIE_exp) :-
	IE_NIE_exp \== [],
	functor_local(Neg_IE_NIE_exp, 'cneg_rt_gv', 3, [IE_NIE_exp |[ GoalVars |[ 'cneg_rt_New' ]]]), !.

negate_imp_form([], _GoalVars, [], []) :- !. % Optimization.
negate_imp_form(Formula, _GoalVars, _Next_Formula, _Neg_Formula) :-
	goal_is_disjunction(Formula, _Formula_1, _Formula_2), !,
	echo_msg(1, '', 'cneg_rt', 'ERROR: negate_imp_form can not deal with disjunctions. Formula', Formula),
	fail.

negate_imp_form(Formula, GoalVars, Next_Formula, Neg_Formula) :-
	goal_is_conjunction(Formula, Formula_1, Formula_2), !,
 	negate_imp_form(Formula_2, GoalVars, Next_Formula, Next_Formula_Aux),
 	negate_imp_form(Formula_1, GoalVars, Next_Formula_Aux, Neg_Formula).

negate_imp_form(Formula, GoalVars, Next_Formula, Neg_Formula) :-
	negate_imp_atom(Formula, GoalVars, Neg_Atom, Keep_Atom),
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
negate_imp_atom([], _GoalVars, [], []) :- !. % Obvious.
negate_imp_atom(true, _GoalVars, fail, true):- !. % Trivial predicates
negate_imp_atom(fail, _GoalVars, true, fail):- !. % Trivial predicates

% Negation of equalities. We need to take care of converting UQV to EQV and vice-versa.
% Since EQV are not used in equality basic predicates, we assume that EQV variables
% are the result from applying double negation to a disequality. 
negate_imp_atom(Formula, GoalVars_In, Neg_Atom, Keep_Atom) :-
	goal_is_equality(Formula, T1, T2, _Eq_GV_In, Eq_EQV_In, Eq_UQV_In), !,

	varsbag((T1, T2), [], [], Vars_Eq), % Just variables
	varsbag(GoalVars_In, [], [], GoalVars), % Just variables
	varsbag_clean_up(Eq_EQV_In, Eq_EQV), % Just variables
	varsbag_clean_up(Eq_UQV_In, Eq_UQV), % Just variables
	varsbag_difference(Vars_Eq, GoalVars, Diseq_UQV_Tmp), % GoalVars are not free vars.
	varsbag_difference(Diseq_UQV_Tmp, Eq_UQV, Diseq_UQV_Aux), % Previous UQV are not free vars.

	% Ensure UQV do not clash with EQV.
	Neg_Atom_Aux = (diseq_geuqv(T1, T2, GoalVars, Eq_UQV, Diseq_UQV)),
	copy_term(Diseq_UQV_Aux, Diseq_UQV),
	replace_in_term_vars_by_values(Neg_Atom_Aux, Diseq_UQV_Aux, Diseq_UQV, Neg_Atom),
	Keep_Atom = (eq_geuqv(T1, T2, GoalVars, Eq_EQV, Eq_UQV)),
 	!. % Clean up backtracking stack.

% Idem for disequalities.
negate_imp_atom(Formula, GoalVars_In, Neg_Atom, Keep_Atom) :-
	goal_is_disequality(Formula, T1, T2, _Diseq_GV_In, Diseq_EQV_In, Diseq_UQV_In), !,

	varsbag((T1, T2), [], [], Vars_Diseq), % Just variables
	varsbag(GoalVars_In, [], [], GoalVars), % Just variables
	varsbag_clean_up(Diseq_EQV_In, Diseq_EQV), % Just variables
	varsbag_clean_up(Diseq_UQV_In, Diseq_UQV), % Just variables
	varsbag_difference(Vars_Diseq, GoalVars, Eq_UQV_Tmp), % GoalVars are not free vars.
	varsbag_difference(Eq_UQV_Tmp, Diseq_UQV, Eq_UQV_Aux), % Previous UQV are not free vars.

	% Ensure UQV do not clash with EQV.
	Neg_Atom_Aux = (eq_geuqv(T1,T2, GoalVars, Diseq_UQV, Eq_UQV_Aux)),
	copy_term(Eq_UQV_Aux, Eq_UQV),
	replace_in_term_vars_by_values(Neg_Atom_Aux, Eq_UQV_Aux, Eq_UQV, Neg_Atom),
	Keep_Atom = (diseq_geuqv(T1, T2, GoalVars, Diseq_EQV, Diseq_UQV)),
 	!. % Clean up backtracking stack.

negate_imp_atom(Formula, GoalVars, Neg_Atom, Keep_Atom) :-
	functor_local(Neg_Atom, 'cneg_rt_gv', 3, [Formula |[ GoalVars |[ 'cneg_rt_New' ]]]),
	Keep_Atom = (Formula),
 	!. % Clean up backtracking stack.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


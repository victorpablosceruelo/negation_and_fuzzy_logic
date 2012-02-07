
:- module(cneg_rt_Chan, [negate_subfrontier_chan/4], [assertions]).
:- meta_predicate cneg(goal).
%:- meta_predicate cneg_processed_pred(goal,?). 

:- multifile negate_subfrontier/4.

:- use_module(cneg_aux, _).
:- use_module(cneg_rt_aux_frontiers, 
	[  subfrontier_contents/5, 
	   subfrontier_E_IE_NIE_contents/4,
	   subfrontier_E_IE_NIE_ie_contents/6,
	   split_subfrontier_into_E_IE_NIE/2,
	   rebuild_conjunction_of_goals/3,
	   split_IE_NIE_between_imp_and_exp/3
	]).
:- use_module(cneg_diseq, [ 
	portray_attributes_in_term_vars/3,
	get_attributes_in_term_vars/3,
	diseq_geuqv/5, eq_geuqv/5,
	diseq_geuqv_adv/6, eq_geuqv_adv/6]).
:- use_module(cneg_rt_aux_Chan, [normalize_E_IE_NIE/4]).

%:- use_module(library(cneg_diseq),[cneg_diseq/3]).
% Esta linea para cuando cneg sea una libreria.

:- comment(title, "Contructive Negation Runtime Library - Chan's Proposal").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module implements Chan's proposal of Constructive Negation.").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% negate_subfrontier(SubFrontier, GoalVars, Proposal, Result_Frontier)
% returns in Result_Frontier the negation of the subfrontier in SubFrontier

negate_subfrontier_chan(SubFrontier_In, GoalVars, Proposal, (Result)):-
	Proposal = 'cneg_rt_Chan',
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier :: SubFrontier_In', (SubFrontier_In)),
	split_subfrontier_into_E_IE_NIE(SubFrontier_In, SubFrontier_Aux_1),
	!, % Reduce the stack's memory by forbidding backtracking.
	normalize_E_IE_NIE(Proposal, SubFrontier_Aux_1, GoalVars, SubFrontier_Aux_2),
	!, % Reduce the stack's memory by forbidding backtracking.
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier', SubFrontier_Aux_2),
	split_IE_NIE_between_imp_and_exp(SubFrontier_Aux_2, GoalVars, SubFrontier_Aux_3),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier', SubFrontier_Aux_3),
	!, % Reduce the stack's memory by forbidding backtracking.
	negate_formula(SubFrontier_Aux_3, Proposal, GoalVars, Result),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier :: (Result)', (Result)),
	!. % Reduce the stack's memory by forbidding backtracking.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% negate_formula(Frontier, Proposal, GoalVars, UQV, ImpVars, ExpVars, UQ_to_EQ_Vars, Dumb_Vars, Result)
% returns Result that is the result from negating the frontier.
negate_formula(Frontier, _Proposal, _GoalVars, true) :- 
	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	subfrontier_E_IE_NIE_ie_contents(Frontier, [], [], [], [], []),
	!. % Optimization

negate_formula(Frontier, Proposal, GoalVars, Neg_E_IE_NIE) :-
	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	subfrontier_E_IE_NIE_ie_contents(Frontier, E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp),

	rebuild_conjunction_of_goals(IE_Exp, NIE_Exp, IE_NIE_Exp), 
	rebuild_conjunction_of_goals(E, IE_Imp, E_IE_Imp),
	rebuild_conjunction_of_goals(E_IE_Imp, NIE_Imp, Formulae_Imp),

	echo_msg(2, '', 'cneg_rt', 'negate_formula :: Formulae_Imp', Formulae_Imp),
	echo_msg(2, '', 'cneg_rt', 'negate_formula :: IE_NIE_Exp', IE_NIE_Exp),
 	negate_IE_NIE_exp(IE_NIE_Exp, Proposal, GoalVars, Neg_IE_NIE_Exp),
	negate_imp_form(Formulae_Imp, Proposal, GoalVars, Neg_IE_NIE_Exp, Neg_E_IE_NIE),
	!. % Backtracking forbidden.

% negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,SolC) obtain in
% SolC a solution of negating Dexp y Rexp juntos.
negate_IE_NIE_exp([], _Proposal, _GoalVars, []):- !.
negate_IE_NIE_exp(IE_NIE_exp, Proposal, GoalVars, Neg_IE_NIE_exp) :-
	IE_NIE_exp \== [],
	functor_local(Neg_IE_NIE_exp, 'cneg_rt_gv', 3, [IE_NIE_exp |[ GoalVars |[ Proposal]]]), !.

negate_imp_form([], _Proposal, _GoalVars, [], []) :- !. % Optimization.
negate_imp_form(Formula, _Proposal, _GoalVars, _Next_Formula, _Neg_Formula) :-
	goal_is_disjunction(Formula, _Formula_1, _Formula_2), !,
	echo_msg(1, '', 'cneg_rt', 'ERROR: negate_imp_form can not deal with disjunctions. Formula', Formula),
	fail.

negate_imp_form(Formula, Proposal, GoalVars, Next_Formula, Neg_Formula) :-
	goal_is_conjunction(Formula, Formula_1, Formula_2), !,
 	negate_imp_form(Formula_2, Proposal, GoalVars, Next_Formula, Next_Formula_Aux),
 	negate_imp_form(Formula_1, Proposal, GoalVars, Next_Formula_Aux, Neg_Formula).

negate_imp_form(Formula, Proposal, GoalVars, Next_Formula, Neg_Formula) :-
	negate_imp_atom(Formula, Proposal, GoalVars, Neg_Atom, Keep_Atom),
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
negate_imp_atom([], _Proposal, _GoalVars, [], []) :- !. % Obvious.
negate_imp_atom(true, _Proposal, _GoalVars, fail, true):- !. % Trivial predicates
negate_imp_atom(fail, _Proposal, _GoalVars, true, fail):- !. % Trivial predicates

% Negation of equalities. We need to take care of converting UQV to EQV and vice-versa.
% Since EQV are not used in equality basic predicates, we assume that EQV variables
% are the result from applying double negation to a disequality. 
negate_imp_atom(Formula, _Proposal, GoalVars, Neg_Atom, Keep_Atom) :-
	goal_is_equality(Formula, T1, T2, _Eq_GV_In, Eq_EQV_In, Eq_UQV_In), !,

	varsbag((T1, T2), [], [], Vars_Eq), % Just variables
	varsbag_clean_up(Eq_EQV_In, Eq_EQV), % Just variables
	varsbag_clean_up(Eq_UQV_In, Eq_UQV), % Just variables
	varsbag_difference(Vars_Eq, GoalVars, Diseq_UQV_Tmp), % GoalVars are not free vars.
	varsbag_difference(Diseq_UQV_Tmp, Eq_UQV, Diseq_UQV), % Previous UQV are not free vars.

	Neg_Atom = (diseq_geuqv(T1, T2, GoalVars, Eq_UQV, Diseq_UQV)),
	Keep_Atom = (eq_geuqv(T1, T2, GoalVars, Eq_EQV, Eq_UQV)).

% Idem for disequalities.
negate_imp_atom(Formula, _Proposal, GoalVars, Neg_Atom, Keep_Atom) :-
	goal_is_disequality(Formula, T1_In, T2_In, _Diseq_GV_In, Diseq_EQV_In, Diseq_UQV_In), !,

	varsbag((T1_In, T2_In), [], [], Vars_Diseq), % Just variables
	varsbag_clean_up(Diseq_EQV_In, Diseq_EQV_Aux), % Just variables
	varsbag_clean_up(Diseq_UQV_In, Diseq_UQV_Aux), % Just variables
	varsbag_difference(Vars_Diseq, GoalVars, Eq_UQV_Tmp), % GoalVars are not free vars.
	varsbag_difference(Eq_UQV_Tmp, Diseq_UQV_Aux, Eq_UQV_Aux), % Previous UQV are not free vars.

	% Need to replace UQV by new fresh variables.
	% This is our real improvement to Chan's version.
	copy_term(Eq_UQV_Aux, Eq_UQV),
	replace_in_term_vars_by_values(T1_In, Eq_UQV_Aux, Eq_UQV, T1),
	replace_in_term_vars_by_values(T2_In, Eq_UQV_Aux, Eq_UQV, T2),
	replace_in_term_vars_by_values(Diseq_EQV_Aux, Eq_UQV_Aux, Eq_UQV, Diseq_EQV),
	replace_in_term_vars_by_values(Diseq_UQV_Aux, Eq_UQV_Aux, Eq_UQV, Diseq_UQV),
	!, % Clean up backtracking stack.

	Neg_Atom = (eq_geuqv(T1,T2, GoalVars, Diseq_UQV, Eq_UQV)),
	Keep_Atom = (diseq_geuqv(T1,T2, GoalVars, Diseq_EQV, Diseq_UQV)).

negate_imp_atom(Formula, Proposal, GoalVars, Neg_Atom, Keep_Atom) :-
	functor_local(Neg_Atom, 'cneg_rt_gv', 3, [Formula |[ GoalVars |[ Proposal]]]),
	Keep_Atom = (Formula). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


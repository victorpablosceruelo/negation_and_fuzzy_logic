%
% From Susana modified by VPC.
%
:- module(cneg_rt_Chan, [negate_frontier/4], [assertions]).
:- meta_predicate cneg(goal).
%:- meta_predicate cneg_processed_pred(goal,?). 

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.

:- use_module(cneg_aux, _).
:- use_module(cneg_rt_aux_frontiers, 
	[  subfrontier_contents/5, 
	   subfrontier_E_IE_NIE_contents/4,
	   subfrontier_E_IE_NIE_ie_contents/6,
	   split_subfrontier_into_E_IE_NIE/2,
	   rebuild_conjunction_of_goals/3 
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

% negate_frontier_list(Frontier, GoalVars, Proposal, Result_List),
% returns in Result_List a list with the negation of each subfrontier in Frontier.

negate_frontier([], _GoalVars, _Proposal, [true]) :- !. % Optimization.
negate_frontier(Frontier, GoalVars, Proposal, Negated_Frontier) :- 
	Frontier \== [], !,
	negate_each_subfrontier(Frontier, GoalVars, Proposal, Negated_Frontier),
	!.

negate_each_subfrontier([], _GoalVars, _Proposal, []) :- !.
negate_each_subfrontier([Frontier | More_Frontiers], GoalVars, Proposal, [Result_Frontier | Result_More_Frontiers]) :-
%	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier: (Frontier, GoalVars)', (Frontier, GoalVars)),
	negate_subfrontier(Frontier, GoalVars, Proposal, Result_Frontier),
%	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier: Result_Frontier', Result_Frontier),
	!, % Reduce the stack's memory by forbidding backtracking.
	negate_each_subfrontier(More_Frontiers, GoalVars, Proposal, Result_More_Frontiers).
%	combine_negated_frontiers(Result_Frontier, Result_More_Frontiers, Result), 
%	!. % Reduce the stack's memory by forbidding backtracking.
	

% combine_negated_subfrontiers(Result_Subfr, Result_More_Subfr, Result_Tmp),
%combine_negated_frontiers(fail, _Result_More_Subfr, fail) :- !.
%combine_negated_frontiers(_Result_Subfr, fail, fail) :- !.
%combine_negated_frontiers(true, Result_More_Subfr, Result_More_Subfr) :- !.
%combine_negated_frontiers(Result_Subfr, true, Result_Subfr) :- !.
%combine_negated_frontiers(Result_Subfr, Result_More_Subfr, (Result_Subfr, Result_More_Subfr)) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% negate_subfrontier(SubFrontier, GoalVars, Proposal, Result_Frontier)
% returns in Result_Frontier the negation of the subfrontier in SubFrontier

negate_subfrontier(SubFrontier_In, GoalVars, Proposal, (Result)):-
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
	
% split_IE_NIE_between_imp_and_exp(Frontier_In, ImpVars, ExpVars, UQ_to_EQ_Vars, Dumb_Vars, Frontier_Out)
% returns Frontier_Out that is the frontier divided betwen 
% ImpVars, ExpVars and UQ_Vars.
split_IE_NIE_between_imp_and_exp(Frontier_In, GoalVars, Frontier_Out):-
	subfrontier_E_IE_NIE_contents(Frontier_In, E, IE, NIE),
	echo_msg(2, '', 'cneg_rt', 'split_IE_NIE_between_imp_and_expw :: (E, IE, NIE)', (E, IE, NIE)),

	% Delayed diseqs are those ones which have a variable not in GoalVars but in E or NIE.
	% If all the variables of the diseq are GoalVars or do not appear in E or NIE then they are not delayed diseqs.
	compute_delayed_and_non_goalvars_variables(Frontier_In, GoalVars, Delayed_Vars, Non_GoalVars),

	split_ie_or_nie_between_imp_and_exp(IE, Delayed_Vars, IE_Imp, IE_Exp),
	echo_msg(2, '', 'cneg_rt', 'split_ie_or_nie_between_imp_and_exp :: (IE_Imp, IE_Exp)', (IE_Imp, IE_Exp)),

	split_ie_or_nie_between_imp_and_exp(NIE, Non_GoalVars, NIE_Imp, NIE_Exp),
	echo_msg(2, '', 'cneg_rt', 'split_ie_or_nie_between_imp_and_exp :: (NIE_Imp, NIE_Exp)', (NIE_Imp, NIE_Exp)),

	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	subfrontier_E_IE_NIE_ie_contents(Frontier_Out, E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp).

% split_formula_between_imp_and_exp(F,ExpVars,Fimp,Fexp) divide F between Fimp and Fexp.
% In Fexp are the elements of F with any variables of ExpVars and
% the rest of elements of F will be in Fimp
split_ie_or_nie_between_imp_and_exp([], _ExpVars, [], []) :- !. % Optimization.
split_ie_or_nie_between_imp_and_exp(Form, ExpVars, Form_imp, Form_exp) :-
	goal_is_conjunction(Form, Form_1, Form_2), !,
	split_ie_or_nie_between_imp_and_exp(Form_1, ExpVars, Form_imp_1, Form_exp_1),
	split_ie_or_nie_between_imp_and_exp(Form_2, ExpVars, Form_imp_2, Form_exp_2),
	rebuild_conjunction_of_goals(Form_imp_1, Form_imp_2, Form_imp),
	rebuild_conjunction_of_goals(Form_exp_1, Form_exp_2, Form_exp).

split_ie_or_nie_between_imp_and_exp(Form, _ExpVars, _Form_imp, _Form_exp) :-
	goal_is_disjunction(Form, _Form_1, _Form_2), !,
	echo_msg(1, '', 'cneg_rt', 'ERROR: split_ie_or_nie_between_imp_and_exp can not deal with disjunctions. Form', Form),
	fail.

split_ie_or_nie_between_imp_and_exp(Form, ExpVars, Form_imp, Form_exp) :-
	varsbag(Form, [], [], Form_Vars), 
	varsbag_intersection(Form_Vars, ExpVars, Intersection),
	(
	    (   % Form has some ExpVars
		Intersection \== [], !,
		Form_exp = Form, Form_imp = []
	    )
	;
	    (   % Form has no ExpVars 
		Intersection == [], !,
		Form_exp = [], Form_imp = Form
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Delayed diseqs are those ones which have a variable not in GoalVars but in E or NIE.
% If all the variables of the diseq are GoalVars or do not appear in E or NIE then they are not delayed diseqs.
% Closed variables are not taken into account for this process.
compute_delayed_and_non_goalvars_variables(SubFrontier_In, GoalVars, Delayed_Vars, Non_GoalVars) :-
	subfrontier_E_IE_NIE_contents(SubFrontier_In, E, IE, NIE),

	identify_closed_vars(E, [], Closed_Vars_E),
	identify_closed_vars(IE, Closed_Vars_E, Closed_Vars_E_IE),
	identify_closed_vars(NIE, Closed_Vars_E_IE, Closed_Vars_E_IE_NIE), % In Chan's proposal it is always empty.
	varsbag(GoalVars, [], Closed_Vars_E_IE_NIE, Closed_Vars), 

	varsbag(E, Closed_Vars, [], Unclosed_Vars_E), % Vars_E = vars(E) - Closed_Vars
	varsbag(IE, Closed_Vars, [], Unclosed_Vars_IE), % Vars_IE = vars(IE) - Closed_Vars
	varsbag(NIE, Closed_Vars, [], Unclosed_Vars_NIE),  % Vars_NIE = vars(NIE) - Closed_Vars

	varsbag_difference(Unclosed_Vars_IE, Unclosed_Vars_E, Delayed_Tmp_1),
	varsbag_difference(Unclosed_Vars_IE, Unclosed_Vars_NIE, Delayed_Tmp_2),
	varsbag_addition(Delayed_Tmp_1, Delayed_Tmp_2, Delayed_Vars),
	
	varsbag_addition(Unclosed_Vars_E, Unclosed_Vars_IE, Unclosed_Vars_E_IE),
	varsbag_addition(Unclosed_Vars_E_IE, Unclosed_Vars_NIE, Non_GoalVars).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

identify_closed_vars([], Closed_Vars, Closed_Vars) :- !. % It can be empty.
identify_closed_vars(Frontier, Closed_Vars_In, Closed_Vars_Out) :- % Conjunctions
	goal_is_conjunction(Frontier, Frontier_Left, Frontier_Right), !,
	identify_closed_vars(Frontier_Left, Closed_Vars_In, Closed_Vars_Aux),
	identify_closed_vars(Frontier_Right, Closed_Vars_Aux, Closed_Vars_Out).

identify_closed_vars(Frontier, Closed_Vars_In, Closed_Vars_Out) :- % Equalities
	goal_is_equality(Frontier, _Value_1, _Value_2, _GV, _EQV, UQV),
	varsbag(UQV, [], Closed_Vars_In, Closed_Vars_Out).

identify_closed_vars(Frontier, Closed_Vars_In, Closed_Vars_Out) :- % Disequalities
	goal_is_disequality(Frontier, _Term1, _Term2, _GV, _EQV, UQV), !,
	varsbag(UQV, [], Closed_Vars_In, Closed_Vars_Out).

identify_closed_vars(Frontier, Closed_Vars_In, Closed_Vars_Out) :- % Negations
	goal_is_negation_uqv(Frontier, UQV, _SubGoal, _Negation_Proposal), !,
	varsbag(UQV, [], Closed_Vars_In, Closed_Vars_Out).

identify_closed_vars(Frontier, Closed_Vars_In, Closed_Vars_In) :- % Other subgoals
	goal_is_not_conj_disj_eq_diseq_dneg(Frontier), !.

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


%
% From Susana modified by VPC.
%
:- module(cneg_rt_Chan, [negate_frontier_list/4], [assertions]).
:- meta_predicate cneg(goal).
%:- meta_predicate cneg_processed_pred(goal,?). 

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.
:- multifile call_to/3.

:- use_module(cneg_aux, _).
:- use_module(cneg_diseq, [ 
	portray_attributes_in_term_vars/3,
	get_attributes_in_term_vars/3,
	diseq_geuqv/5, eq_geuqv/5,
	diseq_geuqv_adv/6, eq_geuqv_adv/6]).

%:- use_module(library(cneg_diseq),[cneg_diseq/3]).
% Esta linea para cuando cneg sea una libreria.

:- comment(title, "Contructive Negation Runtime Library - Chan's Proposal").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module implements Chan's proposal of Constructive Negation.").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Structures to manage all the info about the frontier in an easy way.
frontier_E_IE_NIE_contents(frontier_E_IE_NIE(E, IE, NIE), E, IE, NIE).
frontier_E_IE_NIE_ie_contents(frontier_E_IE_NIE_ie(E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp), E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% negate_frontier_list(Frontier, GoalVars, Proposal, Result_List),
% returns in Result_List a list with the negation of each subfrontier in Frontier.

negate_frontier_list([], _GoalVars, _Proposal, [true]) :- !. % Optimization.
negate_frontier_list(Frontier, GoalVars, Proposal, Negated_Frontier) :- 
	Frontier \== [], !,
	negate_frontier_list_aux(Frontier, GoalVars, Proposal, Negated_Frontier),
	!.

negate_frontier_list_aux([], _GoalVars, _Proposal, []) :- !.
negate_frontier_list_aux([Frontier | More_Frontiers], GoalVars, Proposal, [Result_Frontier | Result_More_Frontiers]) :-
%	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier: (Frontier, GoalVars)', (Frontier, GoalVars)),
	negate_subfrontier(Frontier, GoalVars, Proposal, Result_Frontier),
%	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier: Result_Frontier', Result_Frontier),
	!, % Reduce the stack's memory by forbidding backtracking.
	negate_frontier_list_aux(More_Frontiers, GoalVars, Proposal, Result_More_Frontiers).
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
	split_frontier_into_E_IE_NIE(SubFrontier_In, SubFrontier_Aux_1),
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

rebuild_conjunction_of_goals([], [], []) :- !. % Empty elements when re-joining
rebuild_conjunction_of_goals(Goals, [], Goals) :- !. % Empty element when re-joining
rebuild_conjunction_of_goals([], Goals, Goals) :- !. % Empty element when re-joining
rebuild_conjunction_of_goals(Goals_1, Goals_2, (Goals_1, Goals_2)) :- % Non-empty elements.
	Goals_1 \== [], 
	Goals_2 \== [], !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_frontier_into_E_IE_NIE(Frontier_In, _Frontier_Out) :-
	goal_is_disjunction(Frontier_In, _G1, _G2), !, 
	echo_msg(1, '', 'cneg_rt', 'ERROR: split_frontier_into_E_IE_NIE can not deal with disjunctions. Frontier_In', Frontier_In),
	fail.

split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :-
	goal_is_conjunction(Frontier_In, G1, G2), !,
	split_frontier_into_E_IE_NIE(G1, Frontier_G1),
	split_frontier_into_E_IE_NIE(G2, Frontier_G2),
	frontier_E_IE_NIE_contents(Frontier_G1, E_G1, IE_G1, NIE_G1),
	frontier_E_IE_NIE_contents(Frontier_G2, E_G2, IE_G2, NIE_G2),
	rebuild_conjunction_of_goals(E_G1, E_G2, E_Out),
	rebuild_conjunction_of_goals(IE_G1, IE_G2, IE_Out),
	rebuild_conjunction_of_goals(NIE_G1, NIE_G2, NIE_Out),
	frontier_E_IE_NIE_contents(Frontier_Out, E_Out, IE_Out, NIE_Out).

split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_equality(Frontier_In, _Term1, _Term2, _GV, _EQV, _UQV), !,
	frontier_E_IE_NIE_contents(Frontier_Out, Frontier_In, [], []).

split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_disequality(Frontier_In, _Term1, _Term2, _GV, _EQV, _UQV), !,
	frontier_E_IE_NIE_contents(Frontier_Out, [], Frontier_In, []).

% This leads to infinite loops because double negation 
% sould be managed when generating the frontier.
% The way to fix this is remove cneg(cneg(...))
% when evaluating the frontier. To be done.
split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_negation(Frontier_In, _GoalVars, _UQV, _SubGoal, _Negation_Proposal), !,
	frontier_E_IE_NIE_contents(Frontier_Out, [], [], Frontier_In).

split_frontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_not_conj_disj_eq_diseq_dneg(Frontier_In), !,
	frontier_E_IE_NIE_contents(Frontier_Out, [], [], Frontier_In).

split_frontier_into_E_IE_NIE(Frontier_In, _Frontier_Out) :- 
	echo_msg(1, '', 'cneg_rt', 'ERROR: split_frontier_into_E_IE_NIE can not deal with frontier. Frontier_In', Frontier_In),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% normalize_I_D_R(I,D,R,GoalVars, In,Dn,Rn,ImpVars) 
% returns In and Dn that are the equalities of I and the disequalities
% of D but after normalizating them.
normalize_E_IE_NIE('cneg_rt_New', Formula_In, _GoalVars, Formula_In) :- !.
normalize_E_IE_NIE('cneg_rt_Chan', Formula_In, GoalVars, Formula_Out) :-
	remove_from_E_redundant_eqs_and_vars(Formula_In, GoalVars, Formula_Aux),  
	echo_msg(2, '', 'cneg_rt', 'normalize_E_IE_NIE :: Formula_Aux', Formula_Aux),
	remove_from_IE_irrelevant_disequalities(Formula_Aux, GoalVars, Formula_Out),
	echo_msg(2, '', 'cneg_rt', 'normalize_E_IE_NIE :: Formula_Out', Formula_Out), 
	!.
 
normalize_E_IE_NIE(Proposal, _Formula_In, _GoalVars, _Formula_Out) :-
	Proposal \== 'cneg_rt_New', 
	Proposal \== 'cneg_rt_Chan', !, 
	echo_msg(2, '', 'cneg_rt', 'normalize_E_IE_NIE :: Unknown proposal', Proposal), !, 
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% removes from E redundnant equalities and variables
remove_from_E_redundant_eqs_and_vars(Formulae_In, GoalVars, Formulae_Out) :- 
	frontier_E_IE_NIE_contents(Formulae_In, E_In, IE_In, NIE_In), 
	remove_from_E_redundant_vars(E_In, GoalVars, 'fail', Changes),
	remove_from_E_redundant_eqs(E_In, [], _Visited, E_Aux),
	frontier_E_IE_NIE_contents(Formulae_Aux, E_Aux, IE_In, NIE_In), 
	(
	    (   % If there are no changes then we have a fixed point.
		Changes == 'fail', % No redundant vars.
		E_In == E_Aux, !, % No redundant eqs.
		Formulae_Out = Formulae_Aux
	    )
	;
	    (   % Else, we need to iterate again
		remove_from_E_redundant_eqs_and_vars(Formulae_Aux, GoalVars, Formulae_Out)	 
	    )
	).


remove_from_E_redundant_eqs(E_In, Visited_In, Visited_Out, E_Out) :- 
	goal_is_conjunction(E_In, E_In_Left, E_In_Right), !,
	remove_from_E_redundant_eqs(E_In_Left, Visited_In, Visited_Out, E_Out_Left),
	remove_from_E_redundant_eqs(E_In_Right, Visited_In, Visited_Out, E_Out_Right),
	(
	    ( E_Out_Left == [], E_Out_Right == [], !, E_Out = [] )
	;
	    ( E_Out_Left == [], !, E_Out = E_Out_Right )
	;
	    ( E_Out_Right == [], !, E_Out = E_Out_Left )
	;
	    ( E_Out = (E_Out_Left, E_Out_Right) )
	).

remove_from_E_redundant_eqs(E_In, Visited_In, Visited_In, []) :- 
	goal_is_equality(E_In, _Value_1, _Value_2, _GV, _EQV, _UQV),
	memberchk(E_In, Visited_In), !. % Eq has been seen before. Redundant.

remove_from_E_redundant_eqs(E_In, Visited_In, Visited_In, []) :- 
	goal_is_equality(E_In, Value_1, Value_2, GV, EQV, UQV),
	goal_is_equality(E_Tmp, Value_2, Value_1, GV, EQV, UQV), % Args interchanged.
	memberchk(E_Tmp, Visited_In), !. % Eq has been seen before. Redundant.

remove_from_E_redundant_eqs(E_In, Visited_In, [ E_In | Visited_In ], E_In) :- 
	goal_is_equality(E_In, _Value_1, _Value_2, _GV, _EQV, _UQV).
		
remove_from_E_redundant_vars(E_In, GoalVars, Changes_In, Changes_Out) :- 
	goal_is_conjunction(E_In, E_In_Left, E_In_Right), !,
	remove_from_E_redundant_vars(E_In_Left, GoalVars, Changes_In, Changes_Aux),
	remove_from_E_redundant_vars(E_In_Right, GoalVars, Changes_Aux, Changes_Out),
	!.

remove_from_E_redundant_vars(E_In, GoalVars, Changes_In, Changes_Out) :- 
	goal_is_equality(E_In, Value_1, Value_2, _GV, _EQV, _UQV),
	remove_from_E_redundant_vars_aux(Value_1, Value_2, GoalVars, Changes_In, Changes_Out).

remove_from_E_redundant_vars_aux(Value_1, Value_2, GoalVars, _Changes_In, 'true') :-
	var(Value_1), 
	varsbag(GoalVars, [], [], Real_GoalVars), % Sometimes we have non vars in GoalVars.
	varsbag((Value_1, Value_2), Real_GoalVars, [], Non_GoalVars),
	(
	    (   % Value_1 is a var in Non_GoalVars
		memberchk(Value_1, Non_GoalVars), !,
		Value_1 = Value_2
	    )
	;
	    (   % Value_2 is a var in Non_GoalVars
		memberchk(Value_2, Non_GoalVars), !,
		Value_2 = Value_1
	    )
	).

remove_from_E_redundant_vars_aux(Value_1, Value_2, _GoalVars, Changes_In, Changes_In) :-
	(   var(Value_1) ; var(Value_2)   ),
	!, fail.

remove_from_E_redundant_vars_aux(Value_1, Value_2, GoalVars, Changes_In, Changes_Out) :-
	functor(Value_1, Name, Arity),
	functor(Value_2, Name, Arity), !,
	Value_1=..[Name|Args1],
	Value_2=..[Name|Args2],
	remove_from_E_redundant_vars_aux_list(Args1, Args2, GoalVars, Changes_In, Changes_Out).

remove_from_E_redundant_vars_aux(Value_1, Value_2, _GoalVars, Changes_In, Changes_In) :-
	functor(Value_1, Name1, Arity1),
	functor(Value_2, Name2, Arity2), 
	(
	    ( Name1 \== Name2) ;
	    ( Arity1 \== Arity2)
	).

remove_from_E_redundant_vars_aux_list([], [], _GoalVars, Changes_In, Changes_In) :- !.
remove_from_E_redundant_vars_aux_list([Arg1|Args1], [Arg2|Args2], GoalVars, Changes_In, Changes_Out) :-
	remove_from_E_redundant_vars_aux(Arg1, Arg2, GoalVars, Changes_In, Changes_Aux), 
	remove_from_E_redundant_vars_aux_list(Args1, Args2, GoalVars, Changes_Aux, Changes_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% remove_from_IE_irrelevant_disequalities(Formula_In, Vars_Info, Formula_Out) :-
% returns IE_Out that is IE_In but without disequalities 
% whose variables are not in ImpVars nor in RelVars (So they are in Dumb_Vars).
% In practice we unify each var in Dumb_Vars to avoid a result after its negation.

remove_from_IE_irrelevant_disequalities(Formula_In, GoalVars, Formula_Out) :-
	frontier_E_IE_NIE_contents(Formula_In, E_In, IE_In, NIE_In),

	varsbag(GoalVars, [], [], Real_GoalVars), % Sometimes we have non vars in GoalVars.
	varsbag(NIE_In, Real_GoalVars, [], RelVars), % RelVars = vars(NIE) - GoalVars
	varsbag(E_In, Real_GoalVars, [], Vars_E),
	varsbag_addition(Real_GoalVars, Vars_E, Valid_Vars_Tmp),
	varsbag_addition(Valid_Vars_Tmp, RelVars, Valid_Vars),
	varsbag(IE_In, Valid_Vars, [], Dumb_Vars), % Dumb_Vars = vars(IE) - (GoalVars + vars(E) + RelVars).

	remove_from_IE_irrelevant_disequalities_aux(IE_In, Dumb_Vars, IE_Out),
	frontier_E_IE_NIE_contents(Formula_Out, E_In, IE_Out, NIE_In). 

remove_from_IE_irrelevant_disequalities_aux(IE, [], IE) :- !. % Optimization.
remove_from_IE_irrelevant_disequalities_aux([], _Dumb_Vars, []) :- !.
remove_from_IE_irrelevant_disequalities_aux(IE_In, Dumb_Vars, IE_Out) :-
	goal_is_conjunction(IE_In, IE_In_1, IE_In_2), !,
	remove_from_IE_irrelevant_disequalities_aux(IE_In_1, Dumb_Vars, IE_Out_1),
	remove_from_IE_irrelevant_disequalities_aux(IE_In_2, Dumb_Vars, IE_Out_2),
	rebuild_conjunction_of_goals(IE_Out_1, IE_Out_2, IE_Out).

remove_from_IE_irrelevant_disequalities_aux(IE_In, Dumb_Vars, IE_Out):-
	goal_is_disequality(IE_In, _Term1, _Term2, _GV_IE_In, _EQV_IE_In, _UQV_IE_In), !,

	varsbag(IE_In, [], [], Vars_IE_In),
	varsbag_intersection(Vars_IE_In, Dumb_Vars, Problematic_Vars),
	(
	    (   % No problematic vars
		Problematic_Vars == [], !,
		IE_Out = IE_In 
	    )
	;
	    (   % At least one problematic var.
		Problematic_Vars \== [], !,
		IE_Out = [] 
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
% split_IE_NIE_between_imp_and_exp(Frontier_In, ImpVars, ExpVars, UQ_to_EQ_Vars, Dumb_Vars, Frontier_Out)
% returns Frontier_Out that is the frontier divided betwen 
% ImpVars, ExpVars and UQ_Vars.
split_IE_NIE_between_imp_and_exp(Frontier_In, GoalVars, Frontier_Out):-
	frontier_E_IE_NIE_contents(Frontier_In, E, IE, NIE),
	echo_msg(2, '', 'cneg_rt', 'split_IE_NIE_between_imp_and_expw :: (E, IE, NIE)', (E, IE, NIE)),

	% Delayed diseqs are those ones which have a variable not in GoalVars but in E or NIE.
	% If all the variables of the diseq are GoalVars or do not appear in E or NIE then they are not delayed diseqs.
	compute_delayed_and_non_goalvars_variables(Frontier_In, GoalVars, Delayed_Vars, Non_GoalVars),

	split_ie_or_nie_between_imp_and_exp(IE, Delayed_Vars, IE_Imp, IE_Exp),
	echo_msg(2, '', 'cneg_rt', 'split_ie_or_nie_between_imp_and_exp :: (IE_Imp, IE_Exp)', (IE_Imp, IE_Exp)),

	split_ie_or_nie_between_imp_and_exp(NIE, Non_GoalVars, NIE_Imp, NIE_Exp),
	echo_msg(2, '', 'cneg_rt', 'split_ie_or_nie_between_imp_and_exp :: (NIE_Imp, NIE_Exp)', (NIE_Imp, NIE_Exp)),

	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	frontier_E_IE_NIE_ie_contents(Frontier_Out, E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp).

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
	frontier_E_IE_NIE_contents(SubFrontier_In, E, IE, NIE),

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
	frontier_E_IE_NIE_ie_contents(Frontier, [], [], [], [], []),
	!. % Optimization

negate_formula(Frontier, Proposal, GoalVars, Neg_E_IE_NIE) :-
	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	frontier_E_IE_NIE_ie_contents(Frontier, E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp),

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


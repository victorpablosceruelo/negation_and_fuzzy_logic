
:- module(cneg_negate_frontier, [negate_frontier/3], [assertions]).

:- comment(title, "Contructive Negation Runtime Library - Negate Frontier").
:- comment(author, "V@'{i}ctor Pablos Ceruelo").
:- comment(summary, "This module implements negation predicates for runtime evaluation.").

:- use_module(library('cneg/cneg_aux')).
%:- use_module(library('cneg/cneg_diseq')).
:- use_module(library('cneg/cneg_frontier')).

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/9.
% To evaluate predicates only from the top package.
:- multifile call_to_predicate/1.
% Debugging or not ... that is the question.
:- multifile file_debug_is_activated/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% negate_frontier(Frontier, GoalVars, Negated_Frontier),
% returns in Negated_Frontier a list with the negation of each subfrontier in Frontier.

negate_frontier([], _GoalVars, [true]) :- !. % Optimization.
negate_frontier(Frontier, GoalVars, Negated_Frontier) :- 
	Frontier \== [], !,
	negate_each_subfrontier(Frontier, GoalVars, Negated_Frontier),
	!.

negate_each_subfrontier([], _GoalVars, []) :- !.
negate_each_subfrontier([Frontier | More_Frontiers], GoalVars, [Result_Frontier | Result_More_Frontiers]) :-
	print_msg(3, 3, '', 'negate_subfrontier: (Frontier, GoalVars)', (Frontier, GoalVars)),
	negate_subfrontier(Frontier, GoalVars, Result_Frontier),
	print_msg(3, 3, '', 'negate_subfrontier: Result_Frontier', Result_Frontier),
	!, % Reduce the stack's memory by forbidding backtracking.
	negate_each_subfrontier(More_Frontiers, GoalVars, Result_More_Frontiers).

negate_each_subfrontier([Frontier | More_Frontiers], GoalVars, Result_More_Frontiers) :-
	print_msg(1, 3, '', 'negate_each_subfrontier :: ERROR negating Frontier. Frontier', Frontier), !,
	negate_each_subfrontier(More_Frontiers, GoalVars, Result_More_Frontiers).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% negate_subfrontier(SubFrontier, GoalVars, Proposal, Result_Frontier)
% returns in Result_Frontier the negation of the subfrontier in SubFrontier

% negate_subfrontier(SubFrontier_In, GoalVars_In, Proposal, (Result)) :-
negate_subfrontier(SubFrontier_In, GoalVars_In, (Result)) :-
	print_msg(3, 3, 'nl', '', ''),
	print_msg(3, 3, '', 'negate_subfrontier :: SubFrontier_In', (SubFrontier_In)),
	!, % Reduce the stack's memory by forbidding backtracking.
	varsbag(GoalVars_In, [], [], GoalVars),
	normalize_E_IE_NIE(SubFrontier_In, GoalVars, SubFrontier_Aux_2),
	subfrontier_E_IE_NIE_contents(SubFrontier_Aux_2, E, _IE, NIE),
	varsbag(E, [], GoalVars, ImpVars),
	varsbag(NIE, ImpVars, [], ExpVars),
	print_msg(3, 3, '', 'negate_subfrontier :: ExpVars', ExpVars),
	!, % Reduce the stack's memory by forbidding backtracking.
	print_msg(3, 3, '', 'negate_subfrontier', SubFrontier_Aux_2),
	split_IE_NIE_between_imp_and_exp(SubFrontier_Aux_2, ExpVars, SubFrontier_Aux_3),
	print_msg(3, 3, '', 'negate_subfrontier', SubFrontier_Aux_3),
	!, % Reduce the stack's memory by forbidding backtracking.
	negate_formula(SubFrontier_Aux_3, GoalVars, Result),
	print_msg(3, 3, '', 'negate_subfrontier :: (Result)', (Result)),
	!. % Reduce the stack's memory by forbidding backtracking.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	

% normalize_I_D_R(I,D,R,GoalVars, In,Dn,Rn,ImpVars) 
% returns In and Dn that are the equalities of I and the disequalities
% of D but after normalizating them.
normalize_E_IE_NIE(Formula_In, GoalVars, Formula_Out) :-
	print_msg(3, 3, '', 'normalize_E_IE_NIE :: Formula_In', Formula_In),
	remove_from_E_redundant_eqs_and_vars(Formula_In, GoalVars, Formula_Aux),  
	print_msg(3, 3, '', 'normalize_E_IE_NIE :: Formula_Aux', Formula_Aux),
	remove_from_IE_irrelevant_disequalities(Formula_Aux, GoalVars, Formula_Out),
	print_msg(3, 3, '', 'normalize_E_IE_NIE :: Formula_Out', Formula_Out), 
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% removes from E redundnant equalities and variables
remove_from_E_redundant_eqs_and_vars(Formula_In, GoalVars, Formula_Out) :- 
	print_msg(3, 3, '', 'remove_from_E_redundant_eqs_and_vars :: Formula_In', Formula_In),
	print_msg(3, 3, '', 'remove_from_E_redundant_eqs_and_vars :: GoalVars', GoalVars),
	subfrontier_E_IE_NIE_contents(Formula_In, E_In, IE_In, NIE_In), 
	subfrontier_E_IE_NIE_contents(Formula_Aux, E_Aux, IE_In, NIE_In), 
	(
	    (
		remove_from_E_redundant_eqs_and_vars_aux(E_In, GoalVars, E_Aux), !,
		remove_from_E_redundant_eqs_and_vars(Formula_Aux, GoalVars, Formula_Out), !
	    )
	;
	    (   % If there are no changes then we have a fixed point.
		print_msg(3, 3, '', 'remove_from_E_redundant_eqs_and_vars', 'END'),
		E_Aux = E_In, !, % No redundant eqs.
		Formula_Out = Formula_Aux
	    )
	).

remove_from_E_redundant_eqs_and_vars_aux(E_In, GoalVars, E_Aux) :-
	(
	    (
		print_msg(3, 3, '', 'remove_from_E_redundant_vars :: (E_In, GoalVars)', (E_In, GoalVars)),
		remove_from_E_redundant_vars(E_In, GoalVars), !,
		E_Aux = E_In
	    )
	;
	    (
		print_msg(3, 3, '', 'remove_from_E_redundant_eqs :: (E_In)', (E_In)),
		remove_from_E_redundant_eqs(E_In, [], _Visited, E_Aux), 
		E_In \== E_Aux, !
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_from_E_redundant_vars(E_In, GoalVars) :- 
	goal_is_conjunction(E_In, E_In_Left, E_In_Right), !,
	(
	    remove_from_E_redundant_vars(E_In_Left, GoalVars)
	;
	    remove_from_E_redundant_vars(E_In_Right, GoalVars)
	), !.

remove_from_E_redundant_vars(E_In, GoalVars) :- 
	goal_is_equality(E_In, Value_1, Value_2, _GV, _EQV, _UQV),
	varsbag(GoalVars, [], [], Real_GoalVars), % Sometimes we have non vars in GoalVars.
	varsbag((Value_1, Value_2), Real_GoalVars, [], Non_GoalVars),
	(
	    (   % Value_1 is a var in Non_GoalVars
		var(Value_1),
		memberchk(Value_1, Non_GoalVars), !,
		print_msg(3, 3, '', 'remove_from_E_redundant_vars :: redundant (1st var not in GoalVars)', (Value_1, Value_2)),
		print_msg(3, 3, '', 'remove_from_E_redundant_vars :: GoalVars', GoalVars),
		Value_1 = Value_2
	    )
	;
	    (   % Value_2 is a var in Non_GoalVars
		var(Value_2),
		memberchk(Value_2, Non_GoalVars), !,
		print_msg(3, 3, '', 'remove_from_E_redundant_vars :: redundant (2nd var not in GoalVars)', (Value_1, Value_2)),
		print_msg(3, 3, '', 'remove_from_E_redundant_vars :: GoalVars', GoalVars),
		Value_2 = Value_1
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_from_E_redundant_eqs(true, Visited, Visited, true) :- !. % Empty E
remove_from_E_redundant_eqs(E_In, Visited_In, Visited_Out, E_Out) :- 
	goal_is_conjunction(E_In, E_In_Left, E_In_Right), !,
	remove_from_E_redundant_eqs(E_In_Left, Visited_In, Visited_Aux, E_Out_Left),
	remove_from_E_redundant_eqs(E_In_Right, Visited_Aux, Visited_Out, E_Out_Right),
	goals_join_by_conjunction(E_Out_Left, E_Out_Right, E_Out), !.

remove_from_E_redundant_eqs(E_In, Visited_In, Visited_In, true) :- 
	goal_is_equality(E_In, _Value_1, _Value_2, _GV, _EQV, _UQV),
	memberchk(E_In, Visited_In), !, % Eq has been seen before. Redundant.
	print_msg(3, 3, '', 'remove_from_E_redundant_eqs :: redundant (seen before)', E_In).

remove_from_E_redundant_eqs(E_In, Visited_In, Visited_In, true) :- 
	goal_is_equality(E_In, Value_1, Value_2, GV, EQV, UQV),
	goal_is_equality(E_Tmp, Value_2, Value_1, GV, EQV, UQV), % Args interchanged.
	memberchk(E_Tmp, Visited_In), !, % Eq has been seen before. Redundant.
	print_msg(3, 3, '', 'remove_from_E_redundant_eqs :: redundant (seen before)', E_In).

remove_from_E_redundant_eqs(E_In, Visited_In, Visited_In, true) :- 
	goal_is_equality(E_In, Value_1, Value_2, _GV, _EQV, _UQV),
	Value_1 == Value_2, !, % Equality between same terms.
	print_msg(3, 3, '', 'remove_from_E_redundant_eqs :: redundant (eq between identical terms)', E_In).

remove_from_E_redundant_eqs(E_In, Visited_In, [ E_In | Visited_In ], E_In) :- 
	goal_is_equality(E_In, _Value_1, _Value_2, _GV, _EQV, _UQV).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% remove_from_IE_irrelevant_disequalities(Formula_In, Vars_Info, Formula_Out) :-
% returns IE_Out that is IE_In but without disequalities 
% whose variables are not in ImpVars nor in RelVars (So they are in Dumb_Vars).
% In practice we unify each var in Dumb_Vars to avoid a result after its negation.

remove_from_IE_irrelevant_disequalities(Formula_In, GoalVars_In, Formula_Out) :-
	subfrontier_E_IE_NIE_contents(Formula_In, E_In, IE_In, NIE_In),

	varsbag(GoalVars_In, [], [], GoalVars), % Sometimes we have non vars in GoalVars.
	varsbag(NIE_In, GoalVars, [], RelVars), % RelVars = vars(NIE) - GoalVars
	varsbag(E_In, GoalVars, [], Vars_E), % Vars equality part
	varsbag_union(GoalVars, RelVars, Valid_Vars_Tmp), % GoalVars + RelVars
	varsbag_union(Valid_Vars_Tmp, Vars_E, Valid_Vars), % GoalVars + RelVars + vars(E)
	varsbag(IE_In, Valid_Vars, [], Irrelevant_Vars), % vars(IE) - (GoalVars + RelVars + vars(E)).

	remove_from_IE_irrelevant_disequalities_aux(IE_In, Irrelevant_Vars, IE_Out),
	subfrontier_E_IE_NIE_contents(Formula_Out, E_In, IE_Out, NIE_In). 

remove_from_IE_irrelevant_disequalities_aux(IE, [], IE) :- !. % Optimization.
remove_from_IE_irrelevant_disequalities_aux([], _Irrelevant_Vars, []) :- !.
remove_from_IE_irrelevant_disequalities_aux(IE_In, Irrelevant_Vars, IE_Out) :-
	goal_is_conjunction(IE_In, IE_In_1, IE_In_2), !,
	remove_from_IE_irrelevant_disequalities_aux(IE_In_1, Irrelevant_Vars, IE_Out_1),
	remove_from_IE_irrelevant_disequalities_aux(IE_In_2, Irrelevant_Vars, IE_Out_2),
	goals_join_by_conjunction(IE_Out_1, IE_Out_2, IE_Out).

remove_from_IE_irrelevant_disequalities_aux(IE_In, Irrelevant_Vars, IE_Out):-
	goal_is_disequality(IE_In, _Term1, _Term2, _GV_IE_In, _EQV_IE_In, _UQV_IE_In), !,

	varsbag(IE_In, [], [], Vars_IE_In),
	varsbag_intersection(Vars_IE_In, Irrelevant_Vars, Intersection),
	(
	    (   % No intersection with irrelevant vars. We keep it.
		Intersection == [], !,
		IE_Out = IE_In 
	    )
	;
	    (   % There is intersection with irrelevant vars. We remove it.
		Intersection \== [], !,
		IE_Out = [] 
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% negate_formula(Frontier, Proposal, GoalVars, UQV, ImpVars, ExpVars, UQ_to_EQ_Vars, Dumb_Vars, Result)
% returns Result that is the result from negating the frontier.
negate_formula(Frontier, _GoalVars, fail) :- 
	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	subfrontier_E_IE_NIE_ie_contents(Frontier, true, true, true, true, true), % Equivalent to true, nothing to negate.
	!. % Optimization

negate_formula(Frontier, GoalVars, Neg_E_IE_NIE) :-
	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	subfrontier_E_IE_NIE_ie_contents(Frontier, E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp),

	goals_join_by_conjunction(IE_Exp, NIE_Exp, IE_NIE_Exp), 
	goals_join_by_conjunction(E, IE_Imp, E_IE_Imp),
	goals_join_by_conjunction(E_IE_Imp, NIE_Imp, E_IE_NIE_Imp),

	print_msg(3, 3, '', 'negate_formula :: E_IE_NIE_Imp', E_IE_NIE_Imp),
	print_msg(3, 3, '', 'negate_formula :: IE_NIE_Exp', IE_NIE_Exp),
 	negate_IE_NIE_exp(IE_NIE_Exp, GoalVars, Neg_IE_NIE_Exp),
	negate_imp_form(E_IE_NIE_Imp, GoalVars, Neg_IE_NIE_Exp, Neg_E_IE_NIE),
	!. % Backtracking forbidden.

% negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,SolC) obtain in
% SolC a solution of negating Dexp y Rexp juntos.
negate_IE_NIE_exp(true, _GoalVars, true):- !.
negate_IE_NIE_exp(IE_NIE_exp, GoalVars_In, Neg_IE_NIE_exp) :-
	IE_NIE_exp \== [], IE_NIE_exp \== true,
	varsbag(GoalVars_In, [], [], GoalVars),
	functor_local(Neg_IE_NIE_exp, 'cneg_aux', 2, [ IE_NIE_exp |[ GoalVars ]]), !.

negate_imp_form(true, _GoalVars, true, true) :- !. % Optimization.
negate_imp_form(Formula, _GoalVars, _Next_Formula, _Neg_Formula) :-
	goal_is_disjunction(Formula, _Formula_1, _Formula_2), !,
	print_msg(1, 3, '', 'ERROR: negate_imp_form can not deal with disjunctions. Formula', Formula),
	fail.

negate_imp_form(Formula, GoalVars, Next_Formula, Neg_Formula) :-
	goal_is_conjunction(Formula, Formula_1, Formula_2), !,
 	negate_imp_form(Formula_2, GoalVars, Next_Formula, Next_Formula_Aux),
 	negate_imp_form(Formula_1, GoalVars, Next_Formula_Aux, Neg_Formula).

negate_imp_form(Formula, GoalVars, Next_Formula, Neg_Formula) :-
	negate_imp_atom(Formula, GoalVars, Neg_Atom, Keep_Atom),
	combine_negated(Neg_Atom, Keep_Atom, Next_Formula, Neg_Formula), 
	!.

combine_negated([], [], Next_Formula, Next_Formula) :- !.
combine_negated(fail, true, Next_Formula, Next_Formula) :- !.
combine_negated(Neg_Formula, _Keep_Formula, [], Neg_Formula) :- !.
combine_negated(Neg_Formula, _Keep_Formula, true, Neg_Formula) :- !.
combine_negated(Neg_Formula, Keep_Formula, Next_Formula, Neg_Formula_Out) :- 
	Neg_Formula \== [], 	Neg_Formula \== true,
	Keep_Formula \== [],	Keep_Formula \== fail,
	Next_Formula \== [],	Next_Formula \== [],
	!,
	Neg_Formula_Out = (Neg_Formula ; (Keep_Formula, Next_Formula)).

% negate_I(I,ImpVars,Sol) obtains in Sol a solution of negating I
negate_imp_atom([], _GoalVars, [], []) :- !. % Obvious.
negate_imp_atom(true, _GoalVars, fail, true):- !. % Trivial predicates
negate_imp_atom(fail, _GoalVars, true, fail):- !. % Trivial predicates

% Negation of equalities. We need to take care of converting UQV to EQV and vice-versa.
% Since EQV are not used in equality basic predicates, we assume that EQV variables
% are the result from applying double negation to a disequality. 
negate_imp_atom(Formula, GoalVars, Neg_Atom, Keep_Atom) :-
	goal_is_equality(Formula, T1, T2, _Eq_GV_In, Eq_EQV_In, Eq_UQV_In), !,

	varsbag((T1, T2), [], [], Vars_Eq), % Just variables
	varsbag_clean_up(Eq_EQV_In, Eq_EQV), % Just variables
	varsbag_clean_up(Eq_UQV_In, Eq_UQV), % Just variables
	varsbag_difference(Vars_Eq, GoalVars, Diseq_UQV_Tmp), % GoalVars are not free vars.
	varsbag_difference(Diseq_UQV_Tmp, Eq_UQV, Diseq_UQV), % Previous UQV are not free vars.

	(   (   Eq_UQV = [], !   )
	;
	    (
		print_msg(1, 3, '', 'WARNING: Chans proposal can not deal with the equality below, but ours can.', ''),
		print_msg(1, 3, '', 'WARNING: eq_geuqv(T1, T2, GoalVars, EQV, UQV)', Formula)
	    )
	),

	Neg_Atom = (diseq_geuqv(T1, T2, GoalVars, Eq_UQV, Diseq_UQV)),
	Keep_Atom = (eq_geuqv(T1, T2, GoalVars, Eq_EQV, Eq_UQV)),

	(   (   Diseq_UQV = [], !   )
	;
	    (
		print_msg(1, 3, '', 'WARNING: Chans proposal can not deal with the disequality below, but ours can.', ''),
		print_msg(1, 3, '', 'WARNING: diseq_geuqv(T1, T2, GoalVars, EQV, UQV)', Neg_Atom)
	    )
	).


% Idem for disequalities.
negate_imp_atom(Formula, GoalVars, Neg_Atom, Keep_Atom) :-
	goal_is_disequality(Formula, T1_In, T2_In, _Diseq_GV_In, Diseq_EQV_In, Diseq_UQV_In), !,

	varsbag((T1_In, T2_In), [], [], Vars_Diseq), % Just variables
	varsbag_clean_up(Diseq_EQV_In, Diseq_EQV_Aux), % Just variables
	varsbag_clean_up(Diseq_UQV_In, Diseq_UQV_Aux), % Just variables
	varsbag_difference(Vars_Diseq, GoalVars, Eq_UQV_Tmp), % GoalVars are not free vars.
	varsbag_difference(Eq_UQV_Tmp, Diseq_UQV_Aux, Eq_UQV_Aux), % Previous UQV are not free vars.

	(   (   Diseq_UQV_Aux = [], !   )
	;
	    (
		print_msg(1, 3, '', 'WARNING: Chans proposal can not deal with the disequality', ''),
		print_msg(1, 3, '', 'WARNING: diseq_geuqv(T1, T2, GoalVars, EQV, UQV)', Formula),
		print_msg(1, 3, '', 'WARNING: but the current implementation can.', '')
	    )
	),

	% Need to replace UQV by new fresh variables.
	% This is our real improvement to Chan's version.
	copy_term(Eq_UQV_Aux, Eq_UQV),
	replace_in_term_vars_by_values(T1_In, Eq_UQV_Aux, Eq_UQV, T1),
	replace_in_term_vars_by_values(T2_In, Eq_UQV_Aux, Eq_UQV, T2),
	replace_in_term_vars_by_values(Diseq_EQV_Aux, Eq_UQV_Aux, Eq_UQV, Diseq_EQV),
	replace_in_term_vars_by_values(Diseq_UQV_Aux, Eq_UQV_Aux, Eq_UQV, Diseq_UQV),
	!, % Clean up backtracking stack.

	Neg_Atom = (eq_geuqv(T1,T2, GoalVars, Diseq_UQV, Eq_UQV)),
	Keep_Atom = (diseq_geuqv(T1,T2, GoalVars, Diseq_EQV, Diseq_UQV)),

	(   (   Eq_UQV = [], !   )
	;
	    (
		print_msg(1, 3, '', 'WARNING: Chans proposal can not deal with the resultant equality', ''),
		print_msg(1, 3, '', 'WARNING: eq_geuqv(T1, T2, GoalVars, EQV, UQV)', Neg_Atom),
		print_msg(1, 3, '', 'WARNING: but the current implementation can.', '')
	    )
	).


negate_imp_atom(Formula, GoalVars_In, Neg_Atom, Keep_Atom) :-
	varsbag(GoalVars_In, [], [], GoalVars),
	varsbag(Formula, GoalVars, [], UQV),
	functor_local(Neg_Atom, 'cneg_rt', 4, [ UQV |[ GoalVars |[ Formula |[ 'cneg_rt_Chan' ]]]]),
	Keep_Atom = (Formula). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% split_IE_NIE_between_imp_and_exp(Frontier_In, ImpVars, ExpVars, UQ_to_EQ_Vars, Dumb_Vars, Frontier_Out)
% returns Frontier_Out that is the frontier divided betwen 
% ImpVars, ExpVars and UQ_Vars.
split_IE_NIE_between_imp_and_exp(Frontier_In, ExpVars, Frontier_Out):-
	subfrontier_E_IE_NIE_contents(Frontier_In, E, IE, NIE),
	print_msg(3, 3, '', 'split_IE_NIE_between_imp_and_exp :: (E, IE, NIE)', (E, IE, NIE)),
	print_msg(3, 3, '', 'split_IE_NIE_between_imp_and_exp :: ExpVars', ExpVars),

	split_ie_or_nie_between_imp_and_exp(IE, ExpVars, IE_Imp, IE_Exp),
	print_msg(3, 3, '', 'split_ie_or_nie_between_imp_and_exp :: IE_Imp', IE_Imp),
	print_msg(3, 3, '', 'split_ie_or_nie_between_imp_and_exp :: IE_Exp', IE_Exp),

	split_ie_or_nie_between_imp_and_exp(NIE, ExpVars, NIE_Imp, NIE_Exp),
	print_msg(3, 3, '', 'split_ie_or_nie_between_imp_and_exp :: NIE_Imp', NIE_Imp),
	print_msg(3, 3, '', 'split_ie_or_nie_between_imp_and_exp :: NIE_Exp', NIE_Exp),

	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	subfrontier_E_IE_NIE_ie_contents(Frontier_Out, E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp).

% split_formula_between_imp_and_exp(F,ExpVars,Fimp,Fexp) divide F between Fimp and Fexp.
% In Fexp are the elements of F with any variables of ExpVars and
% the rest of elements of F will be in Fimp
split_ie_or_nie_between_imp_and_exp(true, _ExpVars, true, true) :- !. % Optimization.
split_ie_or_nie_between_imp_and_exp(Form, ExpVars, Form_imp, Form_exp) :-
	goal_is_conjunction(Form, Form_1, Form_2), !,
	split_ie_or_nie_between_imp_and_exp(Form_1, ExpVars, Form_imp_1, Form_exp_1),
	split_ie_or_nie_between_imp_and_exp(Form_2, ExpVars, Form_imp_2, Form_exp_2),
	goals_join_by_conjunction(Form_imp_1, Form_imp_2, Form_imp),
	goals_join_by_conjunction(Form_exp_1, Form_exp_2, Form_exp).

split_ie_or_nie_between_imp_and_exp(Form, _ExpVars, _Form_imp, _Form_exp) :-
	goal_is_disjunction(Form, _Form_1, _Form_2), !,
	print_msg(1, 3, '', 'ERROR: split_ie_or_nie_between_imp_and_exp can not deal with disjunctions. Form', Form),
	fail.

split_ie_or_nie_between_imp_and_exp(Form, ExpVars, Form_imp, Form_exp) :-
	varsbag(Form, [], [], Form_Vars), 
	varsbag_intersection(Form_Vars, ExpVars, Intersection),
	(
	    (   % Form has some ExpVars
		Intersection \== [], !,
		Form_exp = Form, Form_imp = true
	    )
	;
	    (   % Form has no ExpVars 
		Intersection == [], !,
		Form_exp = true, Form_imp = Form
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

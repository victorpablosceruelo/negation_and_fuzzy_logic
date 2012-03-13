
:- module(cneg_rt_Chan, [negate_subfrontier_chan/4], [assertions]).
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
 	    % equality/3, disequality/3,
	    diseq_geuqv/5, eq_geuqv/5,
	    diseq_geuqv_adv/6, eq_geuqv_adv/6,
 	    prepare_attributes_for_printing/2,
	    cneg_diseq_echo/5
	]).

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

% negate_subfrontier_chan(SubFrontier_In, GoalVars_In, Proposal, (Result)) :-
negate_subfrontier_chan(SubFrontier_In, GoalVars_In, 'cneg_rt_Chan', (Result)) :-
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier :: SubFrontier_In', (SubFrontier_In)),
	split_subfrontier_into_E_IE_NIE(SubFrontier_In, SubFrontier_Aux_1),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier :: SubFrontier', (SubFrontier_Aux_1)),
	!, % Reduce the stack's memory by forbidding backtracking.
	varsbag(GoalVars_In, [], [], GoalVars),
	normalize_E_IE_NIE(SubFrontier_Aux_1, GoalVars, SubFrontier_Aux_2),
	subfrontier_E_IE_NIE_contents(SubFrontier_Aux_2, E, _IE, NIE),
	varsbag(E, [], GoalVars, ImpVars),
	varsbag(NIE, ImpVars, [], ExpVars),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier :: ExpVars', ExpVars),
	!, % Reduce the stack's memory by forbidding backtracking.
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier', SubFrontier_Aux_2),
	split_IE_NIE_between_imp_and_exp(SubFrontier_Aux_2, ExpVars, SubFrontier_Aux_3),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier', SubFrontier_Aux_3),
	!, % Reduce the stack's memory by forbidding backtracking.
	negate_formula(SubFrontier_Aux_3, GoalVars, Result),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier :: (Result)', (Result)),
	!. % Reduce the stack's memory by forbidding backtracking.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	

% normalize_I_D_R(I,D,R,GoalVars, In,Dn,Rn,ImpVars) 
% returns In and Dn that are the equalities of I and the disequalities
% of D but after normalizating them.
normalize_E_IE_NIE(Formula_In, GoalVars, Formula_Out) :-
	echo_msg(2, '', 'cneg_rt', 'normalize_E_IE_NIE :: Formula_In', Formula_In),
	remove_from_E_redundant_eqs_and_vars(Formula_In, GoalVars, Formula_Aux),  
	echo_msg(2, '', 'cneg_rt', 'normalize_E_IE_NIE :: Formula_Aux', Formula_Aux),
	remove_from_IE_irrelevant_disequalities(Formula_Aux, GoalVars, Formula_Out),
	echo_msg(2, '', 'cneg_rt', 'normalize_E_IE_NIE :: Formula_Out', Formula_Out), 
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% removes from E redundnant equalities and variables
remove_from_E_redundant_eqs_and_vars(Formula_In, GoalVars, Formula_Out) :- 
	echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_eqs_and_vars :: Formula_In', Formula_In),
	echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_eqs_and_vars :: GoalVars', GoalVars),
	subfrontier_E_IE_NIE_contents(Formula_In, E_In, IE_In, NIE_In), 
	remove_from_E_redundant_vars(E_In, GoalVars, 'fail', Changes),
	remove_from_E_redundant_eqs(E_In, [], _Visited, E_Aux),
	subfrontier_E_IE_NIE_contents(Formula_Aux, E_Aux, IE_In, NIE_In), 
	(
	    (   % There are changes. We need to iterate again.
		(
		    Changes == 'true' % There was redundant vars.
		;
		    E_In \== E_Aux % There was redundant eqs.
		), !,
		remove_from_E_redundant_eqs_and_vars(Formula_Aux, GoalVars, Formula_Out)	 
	    )
	;
	    (   % If there are no changes then we have a fixed point.
		Changes \== 'true', % No redundant vars.
		E_In == E_Aux, !, % No redundant eqs.
		Formula_Out = Formula_Aux
	    )
	).

remove_from_E_redundant_eqs([], Visited, Visited, []) :- !. % Empty E
remove_from_E_redundant_eqs(E_In, Visited_In, Visited_Out, E_Out) :- 
	goal_is_conjunction(E_In, E_In_Left, E_In_Right), !,
	remove_from_E_redundant_eqs(E_In_Left, Visited_In, Visited_Aux, E_Out_Left),
	remove_from_E_redundant_eqs(E_In_Right, Visited_Aux, Visited_Out, E_Out_Right),
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
	memberchk(E_In, Visited_In), !, % Eq has been seen before. Redundant.
	echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_eqs :: redundant (seen before)', E_In).

remove_from_E_redundant_eqs(E_In, Visited_In, Visited_In, []) :- 
	goal_is_equality(E_In, Value_1, Value_2, GV, EQV, UQV),
	goal_is_equality(E_Tmp, Value_2, Value_1, GV, EQV, UQV), % Args interchanged.
	memberchk(E_Tmp, Visited_In), !, % Eq has been seen before. Redundant.
	echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_eqs :: redundant (seen before)', E_In).

remove_from_E_redundant_eqs(E_In, Visited_In, Visited_In, []) :- 
	goal_is_equality(E_In, Value_1, Value_2, _GV, _EQV, _UQV),
	Value_1 == Value_2, !, % Equality between same terms.
	echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_eqs :: redundant (eq between identical terms)', E_In).

remove_from_E_redundant_eqs(E_In, Visited_In, [ E_In | Visited_In ], E_In) :- 
	goal_is_equality(E_In, _Value_1, _Value_2, _GV, _EQV, _UQV).

remove_from_E_redundant_vars([], _GoalVars, Changes, Changes) :- !. % Empty E
remove_from_E_redundant_vars(E_In, GoalVars, Changes_In, Changes_Out) :- 
	goal_is_conjunction(E_In, E_In_Left, E_In_Right), !,
	remove_from_E_redundant_vars(E_In_Left, GoalVars, Changes_In, Changes_Aux),
	remove_from_E_redundant_vars(E_In_Right, GoalVars, Changes_Aux, Changes_Out),
	!.

remove_from_E_redundant_vars(E_In, GoalVars, Changes_In, Changes_Out) :- 
	goal_is_equality(E_In, Value_1, Value_2, _GV, _EQV, _UQV),
	remove_from_E_redundant_vars_aux(Value_1, Value_2, GoalVars, Changes_In, Changes_Out).

remove_from_E_redundant_vars_aux(Value_1, Value_2, _GoalVars, Changes_In, Changes_In) :-
	var(Value_1), 
	var(Value_2),
	Value_1 == Value_2, % Same var. This can be a redundant eq.
	!.

remove_from_E_redundant_vars_aux(Value_1, Value_2, GoalVars, _Changes_In, 'true') :-
	(   var(Value_1)  ;  var(Value_2)   ), % One of them needs to be a variable.
	varsbag(GoalVars, [], [], Real_GoalVars), % Sometimes we have non vars in GoalVars.
	varsbag((Value_1, Value_2), Real_GoalVars, [], Non_GoalVars),
	(
	    (   % Value_1 is a var in Non_GoalVars
		var(Value_1),
		memberchk(Value_1, Non_GoalVars), !,
		echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_vars :: redundant (1st var not in GoalVars)', (Value_1, Value_2)),
		echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_vars :: GoalVars', GoalVars),
		Value_1 = Value_2
	    )
	;
	    (   % Value_2 is a var in Non_GoalVars
		var(Value_2),
		memberchk(Value_2, Non_GoalVars), !,
		echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_vars :: redundant (2nd var not in GoalVars)', (Value_1, Value_2)),
		echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_vars :: GoalVars', GoalVars),
		Value_2 = Value_1
	    )
	).

remove_from_E_redundant_vars_aux(Value_1, Value_2, _GoalVars, Changes_In, Changes_In) :-
	(   var(Value_1) ; var(Value_2)   ),
	!. 

remove_from_E_redundant_vars_aux(Value_1, Value_2, GoalVars, Changes_In, Changes_Out) :-
	functor_local(Value_1, Name, Arity, Args1),
	functor_local(Value_2, Name, Arity, Args2), !,
	echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_vars :: eq between functors :: erroneous frontier :: terms', (Value_1, Value_2)),
	echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_vars', 'ERROR computing the frontier.'),
	remove_from_E_redundant_vars_aux_list(Args1, Args2, GoalVars, Changes_In, Changes_Out).

remove_from_E_redundant_vars_aux(Value_1, Value_2, _GoalVars, Changes_In, Changes_In) :-
	functor(Value_1, Name1, Arity1),
	functor(Value_2, Name2, Arity2), 
	(
	    ( Name1 \== Name2) ;
	    ( Arity1 \== Arity2)
	),
	echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_vars :: eq between different terms :: failed frontier :: terms', (Value_1, Value_2)).

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
	rebuild_conjunction_of_goals(IE_Out_1, IE_Out_2, IE_Out).

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
	subfrontier_E_IE_NIE_ie_contents(Frontier, [], [], [], [], []), % Equivalent to true, nothing to negate.
	!. % Optimization

negate_formula(Frontier, GoalVars, Neg_E_IE_NIE) :-
	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	subfrontier_E_IE_NIE_ie_contents(Frontier, E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp),

	rebuild_conjunction_of_goals(IE_Exp, NIE_Exp, IE_NIE_Exp), 
	rebuild_conjunction_of_goals(E, IE_Imp, E_IE_Imp),
	rebuild_conjunction_of_goals(E_IE_Imp, NIE_Imp, E_IE_NIE_Imp),

	echo_msg(2, '', 'cneg_rt', 'negate_formula :: E_IE_NIE_Imp', E_IE_NIE_Imp),
	echo_msg(2, '', 'cneg_rt', 'negate_formula :: IE_NIE_Exp', IE_NIE_Exp),
 	negate_IE_NIE_exp(IE_NIE_Exp, GoalVars, Neg_IE_NIE_Exp),
	negate_imp_form(E_IE_NIE_Imp, GoalVars, Neg_IE_NIE_Exp, Neg_E_IE_NIE),
	!. % Backtracking forbidden.

% negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,SolC) obtain in
% SolC a solution of negating Dexp y Rexp juntos.
negate_IE_NIE_exp([], _GoalVars, []):- !.
negate_IE_NIE_exp(IE_NIE_exp, GoalVars_In, Neg_IE_NIE_exp) :-
	IE_NIE_exp \== [],
	varsbag(GoalVars_In, [], [], GoalVars),
	varsbag(IE_NIE_exp, GoalVars, [], UQV),
	functor_local(Neg_IE_NIE_exp, 'cneg_rt', 4, [ UQV |[ GoalVars |[ IE_NIE_exp |[ 'cneg_rt_Chan' ]]]]), !.

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
		echo_msg(1, '', 'cneg_rt', 'WARNING: Chans proposal can not deal with the equality below, but ours can.', ''),
		echo_msg(1, '', 'cneg_rt', 'WARNING: eq_geuqv(T1, T2, GoalVars, EQV, UQV)', Formula)
	    )
	),

	Neg_Atom = (diseq_geuqv(T1, T2, GoalVars, Eq_UQV, Diseq_UQV)),
	Keep_Atom = (eq_geuqv(T1, T2, GoalVars, Eq_EQV, Eq_UQV)),

	(   (   Diseq_UQV = [], !   )
	;
	    (
		echo_msg(1, '', 'cneg_rt', 'WARNING: Chans proposal can not deal with the disequality below, but ours can.', ''),
		echo_msg(1, '', 'cneg_rt', 'WARNING: diseq_geuqv(T1, T2, GoalVars, EQV, UQV)', Neg_Atom)
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
		echo_msg(1, '', 'cneg_rt', 'WARNING: Chans proposal can not deal with the disequality', ''),
		echo_msg(1, '', 'cneg_rt', 'WARNING: diseq_geuqv(T1, T2, GoalVars, EQV, UQV)', Formula),
		echo_msg(1, '', 'cneg_rt', 'WARNING: but the current implementation can.', '')
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
		echo_msg(1, '', 'cneg_rt', 'WARNING: Chans proposal can not deal with the resultant equality', ''),
		echo_msg(1, '', 'cneg_rt', 'WARNING: eq_geuqv(T1, T2, GoalVars, EQV, UQV)', Neg_Atom),
		echo_msg(1, '', 'cneg_rt', 'WARNING: but the current implementation can.', '')
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


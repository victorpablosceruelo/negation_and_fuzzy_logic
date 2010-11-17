:- module(hybrid_negation_lib, [portray_attributes_in_term/1,
%			var_info_var/2, var_info_ovar/2,
%			var_info_status/2, var_info_attribute/2,
%			var_info_index/2,
%			pred_info_index/2, pred_info_forall_vars/2,
%			pred_info_expl/2,
%			get_attribute/2,
			ineq_form_debug/2,
			negate_eq/2, negate_diseq/2,
			keep_eq/2, keep_diseq/2,
			quantify_universally/1
			]).

%:- import put_attr/3, get_attr/3, del_attr/2,
%	install_verify_attribute_handler/4,
%	install_attribute_portray_hook/3 % -- Do not use !!!
%	install_constraint_portray_hook/4
%	from machine.

:- use_module(library(lists),[append/3]).
% :- import append/3 from basics.

:- use_module(hybrid_negation_aux,[clause_head/4, remove_repeated_terms/2,
	var_is_in_term/2, term_vars/2,
	debug_formatted_cls/2, % debug_separation/0,
	memberchk/2,
	term_is_forall/1, term_is_not_forall/1]).

%:- import clause_head/4, remove_repeated_terms/2,
%	var_is_in_term/2, term_vars/2,
%	debug_formatted_cls/2, % debug_separation/0,
%	memberchk/2,
%	term_is_forall/1, term_is_not_forall/1 from intneg_aux.
%
%	clause_head/4, 


% Dynamic predicates.
% :- dynamic formulae/2.

% Porting predicates to XSB.
%detach_attribute(Var) :- del_attr(Var, dist).
%get_attribute(Var, Value) :-
%	get_attr(Var, dist, Value).
%	write(get_attr(Var, dist, Value)), nl.
put_attribute(Var, Value) :-
	write(put_attribute(Var, Value)),nl,
	attach_attribute(Var, Value).
%	put_attr(Var, dist, Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% CONSTRAINT PORTRAY %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% FROM: ../ciao/ciao-1.13/library/clpqr-common/clp_attr.pl
% :- multifile portray_attribute/2.
% 
% portray_attribute(float(F),_) :- print(F).
% portray_attribute(term_wrap(_,T), _) :-
%         normalize(T, I, H),
%         H = [],                   % only if ground
%         print(I).
%
% :- multifile portray/1.
% 
% portray(rat(N,D)) :-
%         print(N/D).
% portray(eqn_var(Self,A,B,R,Nl)) :-
%         print(eqn_var(Self,A,B,R,Nl)).


%:- install_constraint_portray_hook(dist,Contents,Vars,portray_constraints(Vars, Contents)).

:- multifile portray_attribute/2.
portray_attribute(Attr, Var) :-
	write(portray_attribute(Attr, Var)),
	portray_constraints(Var, Attr).

portray_constraints(Vars, Contents) :-
%	nl, write('% DBG: Vars: '), write(Vars), 
%	write(' Attributed vars: '), write(Contents),
%	write(' (Not attributed vars are not shown)' ), nl,
	portray_constraints_build_list(Vars, Contents, [], List),
	nl_if_not_empty_list(List),
	portray_constraints_show_list(List).

portray_constraints_build_list([], [], List, List) :- !.
portray_constraints_build_list([Var1|Vars], [Term1|Terms], List_In, List_Out) :- !,
	get_vars_with_attributes(Term1, Term1_Vars),
	portray_constraints_build_list_aux(Var1, Term1, Term1_Vars, List_In, List_Aux),
	portray_constraints_build_list(Vars, Terms, List_Aux, List_Out).

portray_constraints_build_list_aux(_Var1, _Term1, [], List_In, List_In) :- !.
portray_constraints_build_list_aux(Var1, Term1, Term1_Vars, List_In,
				   [(Var1, Term1, Term1_Vars)|List_In]) :- !.

get_vars_with_attributes(Term, [var_attrib(Term, Attribute)]) :-
	var(Term), 
%	get_attr(Term, dist, Attribute), !.
	get_attribute(Term, Attribute), !.
get_vars_with_attributes(Term, []) :-
	var(Term), !.

get_vars_with_attributes(Term, Vars) :-
	clause_head(Term, _Name, _Arity, Args), !,
	get_vars_with_attributes_list(Args, Vars).

get_vars_with_attributes_list([], []) :- !.
get_vars_with_attributes_list([X|L], Vars) :- !,
	get_vars_with_attributes(X, Vars1),
	get_vars_with_attributes_list(L, Vars2),
	append(Vars2, Vars1, Vars).

portray_constraints_show_list([]).
portray_constraints_show_list([(Var1, Term1, Term1_Vars)|Others]) :-
	portray_attributes(Var1, Term1, Term1_Vars), nl,
	portray_constraints_show_list(Others).

nl_if_not_empty_list([]) :- !.
nl_if_not_empty_list(_Any) :- !, nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% PORTRAY ATTRIBUTES IN TERM %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

portray_attributes_in_term(Term) :-
%	nl, write(Term), write(' has not attributes.'), nl, 
	get_vars_with_attributes(Term, []), !.
portray_attributes_in_term(Term) :-
%	nl, write(Term), write(' has attributes.'), nl,
	get_vars_with_attributes(Term, Term_Vars),
	Term_Vars \== [], 
	write(' -AND- [ '), !,
	portray_attributes_aux_1(Term_Vars),
	write(']'), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% PORTRAY ATTRIBUTE %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- install attribute portray hook(Module,Attribute,Handler)
% :- install_attribute_portray_hook(dist,Attribute,portray_attribute(Attribute)).



portray_attributes(_Var, _Term, Attributes) :-
%	write(Var), write(' = '),
%	write(Term), write(' : '), nl,
	portray_attributes_aux_1(Attributes).

portray_attributes_aux_1([]) :- !.
portray_attributes_aux_1([Attr1]) :- !,
	portray_attributes_aux_2(Attr1).
portray_attributes_aux_1([Attr1|Attrs]) :- !,
	portray_attributes_aux_2(Attr1), nl,
	portray_attributes_aux_1(Attrs).

portray_attributes_aux_2(var_attrib(Var, formulae(_VarX, Formulae))) :-
	write(Var), write(': '),
	write('[ '),
	portray_attributes_aux_3(Formulae),
	write(' ] ').

portray_attributes_aux_3([]):- !.
portray_attributes_aux_3([Formulae]) :- !,
	portray_attributes_aux_4(Formulae).
portray_attributes_aux_3([Formulae|Others]) :- !,
	portray_attributes_aux_4(Formulae),
	write(' AND '),
	portray_attributes_aux_3(Others).

portray_attributes_aux_4(Expr1/Expr2) :- !,
	write(Expr1),
	write(' =/= '),
	write(Expr2).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% CONSTRAINT VERIFICATION %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- install verify attribute handler(+Mod, −AttrValue, −Target, +Handler).
% :- install_verify_attribute_handler(dist, AttrValue, Target, verify_attribute(AttrValue, Target)).

verify_attribute(AttrValue,Target):-
%	write(verify_attribute(AttrValue,Target)), nl,
	verify_attribute_aux(AttrValue, Target, Inequalities),
	update_var_attributes(Inequalities).

verify_attribute_aux(formulae(Target, Inequalities), Target, Inequalities).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% An~ade la formula al atributo de las variables implicadas
% Por q tendriamos q tener en cuenta otros atributos?
% Como cada uno tiene su manejador, tratar de mezclar los atributos no aporta nada.

update_var_attributes(Formulae):-
%	write(update_var_attributes(Formulae)), nl, 
	term_vars_formulae(Formulae, Variables), !,
%	write(update_var_attributes_1(Variables, Formulae)),nl,
        update_var_attributes_1(Variables, Formulae), !.

term_vars_formulae([], []) :- !.
term_vars_formulae([Conj|Others], Variables) :-
	term_vars(Conj, Vars_1),
	term_vars_formulae(Others, Vars_2),
	append(Vars_1, Vars_2, Vars),
	remove_repeated_terms(Vars, Variables).


update_var_attributes_1([], New_Formulas) :- !,
	joint_and_simplify_formulas(New_Formulas, [], _Result).
update_var_attributes_1(Variables, New_Formulas) :-
	Variables \== [], !,
	update_var_attributes_2(Variables, Vars_Formulas), !,
	update_var_attributes_4(Variables, New_Formulas, Vars_Formulas).

update_var_attributes_2([], []) :- !. % Loop over vars list.
update_var_attributes_2([Var|Others], Vars_Formulas):- 
	var(Var), % It can be other things ...
	get_attribute(Var, formulae(Var, ThisVar_Formulas)), 
	detach_attribute(Var), !,
        update_var_attributes_2(Others, Other_Formulas),
	append(ThisVar_Formulas, Other_Formulas, Vars_Formulas).
update_var_attributes_2([_Var|Others], Formulas):- 
        update_var_attributes_2(Others, Formulas).

% Need old vars to play with them too.
update_var_attributes_4(Variables_1, New_Formulas, Vars_Formulas) :-
%	write(joint_and_simplify_formulas(New_Formulas, Vars_Formulas, unknown)), nl,
	joint_and_simplify_formulas(New_Formulas, Vars_Formulas, Joint_Formulas),
	term_vars_formulae(Joint_Formulas, Variables_2), !,
	append(Variables_1, Variables_2, Vars),
	remove_repeated_terms(Vars, Variables),
%	write(update_var_attributes_5(Variables, Joint_Formulas)), nl,
	update_var_attributes_5(Variables, Joint_Formulas).

update_var_attributes_5(_Vars, []) :- !. % No formulae.
update_var_attributes_5(Vars, Joint_Formulae) :-
	update_var_attributes_6(Vars, Joint_Formulae).

update_var_attributes_6([], _Joint_Formulae) :- !. % Finish
update_var_attributes_6([Var|Others], Joint_Formulae) :- !,
	update_var_attributes_7(Var, Joint_Formulae, Var_Formulae),
	update_var_attributes_8(Var, Var_Formulae),
	update_var_attributes_6(Others, Joint_Formulae).

update_var_attributes_7(_Var, [], []) :- !. % Empty formulae.
update_var_attributes_7(Var, [Conj|Others1], [Conj|Others2]) :-
	var_is_in_term(Var, Conj), !,
	update_var_attributes_7(Var, Others1, Others2).
update_var_attributes_7(Var, [_Conj|Others1], Others2) :- !,
	update_var_attributes_7(Var, Others1, Others2).

update_var_attributes_8(_Var, []):- !. % Is ok.

update_var_attributes_8(Var, Formulae) :- !,
	Formulae \== [],
	var(Var),
	put_attribute(Var, formulae(Var, Formulae)).

update_var_attributes_8(Var, Formulae) :- !,
	Formulae \== [],
	nl, % Write error to output.
	write('ERROR: Formulae of no variable: Variable: '),
	write(Var), write(' formulae: '),
	write(Formulae), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% joint_and_simplify_formulas(New_Formulae, Old_Formulae, Formulae).
joint_and_simplify_formulas(F1, F2, Formulae) :-
	joint_formulas(F1, F2, Joint_Formulae),
%	ineq_form_debug('joint_and_simplify_formulas: Joint_Formulae: ', Joint_Formulae),
	simplify_formulas(Joint_Formulae, Formulae).
%	ineq_form_debug('joint_and_simplify_formulas: Simplified Formulae: ', Formulae).

joint_formulas(L1, L2, L4) :-
	append(L1, L2, L3),
	remove_repeated_terms(L3, L4).

simplify_formulas([], []) :- !.
simplify_formulas([F1|Others], Formulae) :- !,
	simplify_1_formulae(F1, Simplified_F1),
%	write(simplify_1_formulae(F1, Simplified_F1)), nl,
	simplify_formulas(Others, Simplified_Others),
	joint_formulas(Simplified_F1, Simplified_Others, Formulae).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simplify_1_formulae(Term1/Term2, Ineq) :-
%	ineq_form_debug('simplify_1_formulae: Formulae: ', Term1/Term2),
	ineq_builder(Term1/Term2, Ineq_1), !,
	extract_1_inequality(Ineq_1, Ineq).
%	ineq_form_debug('simplify_1_formulae: Inequality: ', Ineq).

extract_1_inequality([], []) :- !, fail.
extract_1_inequality([Ineq_In], Ineq_Out) :- !,
	extract_1_inequality_aux(Ineq_In, Ineq_Out).
extract_1_inequality([Ineq_In|_Others], Ineq_Out) :-
	extract_1_inequality_aux(Ineq_In, Ineq_Out).
extract_1_inequality([_Ineq|Others], [OthersIneq]) :-
	extract_1_inequality(Others, [OthersIneq]).

extract_1_inequality_aux(Term1/Term2, [Term1/Term2]) :- !.
extract_1_inequality_aux(true, []) :- !. % True is empty constraints.
extract_1_inequality_aux(fail, []) :- !, fail.

ineq_builder(true, []) :- !. % True is empty constraints.
ineq_builder(fail, []) :- !, fail.
ineq_builder(Term1/Term2, Ineq_Out) :-
	ineq_builder_aux(Term1/Term2, [], Ineq_Out), !.

ineq_builder_aux(Term1/Term2, Ineq_In, Ineq_Out):- % Same var.
        var(Term1),                                     
        var(Term2),
        Term1==Term2, !,
	add_to_ineq_list(fail, Ineq_In, Ineq_Out).

ineq_builder_aux(Term1/Term2, Ineq_In, Ineq_Out):- % Different vars.
        var(Term1),
        var(Term2),
	ineq_builder_aux_history(Term1/Term2, Ineq_In, Ineq_Out).

ineq_builder_aux(Term1/Term2, Ineq_In, Ineq_Out):- % Term2=fA(_Var),
        var(Term1),
	term_is_forall(Term2), !,
	add_to_ineq_list(fail, Ineq_In, Ineq_Out).

ineq_builder_aux(Term1/Term2, Ineq, [Term1/Term2|Ineq]):- 
        var(Term1),
        functor(Term2, _Name, _Arity), !.

ineq_builder_aux(Term1/Term2, Ineq_In, Ineq_Out):-
        var(Term2),
	functor(Term1, _Name, _Arity), 
	ineq_builder_aux_history(Term1/Term2, Ineq_In, Ineq_Out).

ineq_builder_aux(Term1/Term2, Ineq_In, Ineq_Out):-  % Term=fA(_Var),
 	(   (term_is_forall(Term1)) ;
	    (term_is_forall(Term2))), !,
	add_to_ineq_list(fail, Ineq_In, Ineq_Out).

ineq_builder_aux(Term1/Term2, Ineq_In, Ineq_Out):-  % Functors that unify.
 	clause_head(Term1, Name, Arity, Args1),
	clause_head(Term2, Name, Arity, Args2),
	term_is_not_forall(Term1),
	term_is_not_forall(Term2), !,
        ineq_builder_aux_lists(Args1, Args2, Ineq_In, Ineq_Out).

ineq_builder_aux(Term1/Term2, Ineq_In, Ineq_Out):-  % Functors that do not unify.
	clause_head(Term1, Name1, Arity1, _Args1),
	clause_head(Term2, Name2, Arity2, _Args2),
	term_is_not_forall(Term1),
	term_is_not_forall(Term2),
	((Name1 \== Name2) ; (Arity1 \== Arity2)), !,
	add_to_ineq_list(true, Ineq_In, Ineq_Out).

% ineq_builder_aux_lists(Args1, Args2, Ineq).
ineq_builder_aux_lists([], [], Ineq, Ineq).
ineq_builder_aux_lists([Term1], [Term2], Ineq_In, Ineq_Out) :- !,
	ineq_builder_aux(Term1/Term2, Ineq_In, Ineq_Out).
ineq_builder_aux_lists([Term1|Others1], [Term2|Others2], Ineq_In, Ineq_Out) :- !,
	ineq_builder_aux(Term1/Term2, Ineq_In, Ineq_Aux),
	ineq_builder_aux_lists(Others1, Others2, Ineq_Aux, Ineq_Out).


ineq_builder_aux_history(Term1/Term2, Ineq_In, Ineq_Out) :-
	var(Term2), 
%	write(memberchk(Term2, For_Later)), nl,
	memberchk_Ineq(Term2, Ineq_In, History), !, % Recover unification.
	
	add_to_ineq_list(Term1/Term2, Ineq_In, Ineq_Aux),
	ineq_builder_aux_history_aux(History, Term1, Ineq_Aux, Ineq_Out).

ineq_builder_aux_history_aux([], _Term1, Ineq, Ineq) :- !.
ineq_builder_aux_history_aux([Term3/_Term2|Others], Term1, Ineq_In, Ineq_Out) :- !,
	ineq_builder_aux(Term1/Term3, [], Ineq_Aux_1),
	append(Ineq_Aux_1, Ineq_In, Ineq_Aux_2),
	ineq_builder_aux_history_aux(Others, Term1, Ineq_Aux_2, Ineq_Out).

memberchk_Ineq(_Term2, [], []) :- !.
memberchk_Ineq(Term2, [Term3/Term2|Others], [Term3/Term2|History]) :- !,
	memberchk_Ineq(Term2, Others, History).
memberchk_Ineq(Term2, [_Term4/_Term5|Others], History) :-
	memberchk_Ineq(Term2, Others, History).

add_to_ineq_list(true, _Any,   [true]) :- !.
add_to_ineq_list(_Any, [true], [true]) :- !.
add_to_ineq_list(fail, Any,    Any) :- !.
add_to_ineq_list(T1/T2,  List,   [T1/T2|List]).

ineq_form_debug(Msg1, Msg2) :-
	write('% DBG %  '),
	write(Msg1), write(Msg2), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     PREDICADO   DISTINTO                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Predicado que implementa mediante variables con atributo
% la desigualdad entre terminos y como expresarlo a
% traves de disyuncion de conjunciones de desigualdades
% entre terminos que debe satisfacer cada variable.
% Esta implementacion sirve para variables con dominios
% de valores finitos.

% Incluye una desigualdad en las formulas de las 
% variables implicadas

disequality(T1,T2, _Type):- 
%	nl, write(intneg_dist(T1,T2)), nl,
        update_var_attributes([T1/T2]), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Keep the equality between 2 universally quantified variables.
equality(Term1, Term2, 'keep') :-
	var(Term1), var(Term2),
	get_attribute(Term1, universally_quantified(Term1, Term1_E, Term1_IE)),
	get_attribute(Term2, universally_quantified(Term2, Term2_E, Term2_IE)), 
	!, % Don't look for more solutions.
	remove_attribute(Term1),
	remove_attribute(Term2),
	append(Term1_E, Term2_E, Term3_E),
	append(Term1_IE, Term2_IE, Term3_IE),
	Term1 = Term2,
	put_attribute(Term1, universally_quantified(Term1, Term3_E, Term3_IE)).

% Negate a disequality between 2 universally quantified variables.
equality(Term1, Term2, 'negate') :-
	var(Term1), var(Term2),
	get_attribute(Term1, universally_quantified(Term1, Term1_E, Term1_IE)),
	get_attribute(Term2, universally_quantified(Term2, Term2_E, Term2_IE)), 
	!, % Don't look for more solutions.
	remove_attribute(Term1),
	remove_attribute(Term2),
	append(Term1_E, Term2_E, Term3_E),
	append(Term1_IE, Term2_IE, Term3_IE),
	test_for_identicals_e_and_ie(Term3_E, Term3_IE),
	test_for_two_different_e(Term3_E),
	Term1 = Term2,
	put_attribute(Term1, universally_quantified(Term1, Term3_E, Term3_IE)).

	(   get_attribute(Term1, universally_quantified(Term1, Term1_E, Term1_IE)) ;
	    get_attribute(Term2, universally_quantified(Term2, Term2_E, Term2_IE))
	),
	
	(   equality_variables(Term1, ) ;
	    equality_variables(Term2, 
	),
	Term2, Type).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We can not implement their functionality  at a high level since 
% we need to manage in a different way ground and non-ground 
% variables. We'll need to split them before playing with them.

negate_eq(Term1, Term2) :- disequality(Term1, Term2, 'negate').
keep_eq(Term1, Term2) :- equality(Term1, Term2, 'keep').
negate_diseq(Term1, Term2) :- equality(Term1, Term2, 'negate').
keep_diseq(Term1, Term2) :- disequality(Term1, Term2, 'keep').
quantify_universally(Term) :- put_attribute(Term, universally_quantified(Term, [], [])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(cneg_diseq, 
	[
 	    equality/3, disequality/3,
 	    prepare_attributes_for_printing/2,
	    print_vars_diseqs/3,

	    get_disequalities_from_constraints/2,
	    get_disequalities_from_constraints_and_remove_them/2, 
	    remove_all_constraints_in_variables/1,
	    attributes_difference/3,	    
	    attribute_diseq_to_executable_diseq/2
	], 
	[assertions]).

:- use_module(library('cneg/cneg_aux')).
:- use_package(attr).
:- use_package(dcg).
%:- use_module(engine(attributes)).

%:- use_package(debug).
%:- use_package(trace).
%:- use_package(nodebug).

% For Ciao Prolog:
%:- multifile 
%        verify_attribute/2,
%        combine_attributes/2,
%	portray_attribute/2,
%	portray/1.

:- comment(title, "Disequality Management Library").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module is capable of evaluating any disequality between terms 
	generated by the program transformation.").


% For XSB:
%:- import put_attr/3, get_attr/3, del_attr/2,
%	install_verify_attribute_handler/4,
%	install_attribute_portray_hook/3 % -- Do not use !!!
%	install_constraint_portray_hook/4
%	from machine.

% For XSB to verify attributes.
% :- install verify attribute handler(+Mod, −AttrValue, −Target, +Handler).
% :- install_verify_attribute_handler(dist, AttrValue, Target, verify_attribute(AttrValue, Target)).

% For XSB to portray results.
%:- install_constraint_portray_hook(dist,Contents,Vars,portray_constraints(Vars, Contents)).

% For XSB to portray results (at a very low level) :: Do not use !!!
% :- install attribute portray hook(Module,Attribute,Handler)
% :- install_attribute_portray_hook(dist,Attribute,portray_attribute(Attribute)).


% Local predicates used to easy migration between prologs. 
remove_attribute_local(Var) :- 
	print_msg(3, 4, 'logo', '', ''),
	print_msg(3, 4, 'aux', 'remove_attribute_local :: Var :: ', Var),
	del_attr_local(Var),
%	detach_attribute(Var),
	print_msg(3, 4, 'aux', '  -->> Var :: ', Var),
	print_msg(3, 4, 'nl', '', '').
%	detach_attribute(Var).
% XSB:	del_attr(Var, dist).

get_attribute_local(Var, Attribute) :-
	get_attr_local(Var, Attribute),
%	get_attribute(Var, Attribute),
% XSB:	get_attr(Var, dist, Attribute),
 	print_msg(3, 4, '', 'get_attribute_local :: Attribute', Attribute).

put_attribute_local(Var, Attribute) :-
	print_msg(3, 4, '', 'put_attribute_local :: Attribute', Attribute),
	print_msg(3, 4, 'logo', '', ''),
	print_msg(3, 4, 'aux', 'put_attribute_local', ''),
	print_msg(3, 4, 'aux', ' :: Var :: ', Var), 
%	get_attribute_if_any(Var), !,
	put_attr_local(Var, Attribute),
%	attach_attribute(Var, Attribute),
	print_msg(3, 4, 'aux', '  -->> Var :: ', Var),
	print_msg(3, 4, 'nl', '', '').
%	attach_attribute(Var, Attribute).
%	put_attr(Var, dist, Attribute).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Attributes contents are encapsulated via the following structure.

%:- dynamic var_attribute/2.
attribute_contents(var_attribute(Target, Disequalities), Target, Disequalities).
attribute_disequality_contents(disequality(Diseq_1, Diseq_2, EQ_Vars, UQ_Vars), Diseq_1, Diseq_2, EQ_Vars, UQ_Vars).
% equality_contents(equality(T1, T2), T1, T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


attribute_goals(Term) --> 
	[Term : Attributes_For_Printing_Conj],
	 { prepare_attributes_for_printing(Term, Attributes_For_Printing_Conj) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% FROM: ../ciao/ciao-1.13/library/clpqr-common/clp_attr.pl
%
% :- multifile portray_attribute/2, portray/1.
% 
% portray_attribute(float(F),_) :- print(F).
% portray_attribute(term_wrap(_,T), _) :-
%         normalize(T, I, H),
%         H = [],                   % only if ground
%         print(I).
%
% portray(rat(N,D)) :- print(N/D).
% portray(eqn_var(Self,A,B,R,Nl)) :- print(eqn_var(Self,A,B,R,Nl)).

%attr_portray_hook(_Attribute, Var) :- 
%	print_vars_diseqs(2, '', 'attr_portray_hook(_Attribute, Var)'),
%	print_vars_diseqs(1, 'aux', Var).

%portray_attribute(_Attribute, Var) :-
%	print_vars_diseqs(2, '', 'portray_attribute(_Attribute, Var)'),
%	print_vars_diseqs(1, 'aux', Var).

%portray(Attribute) :-
%	print_vars_diseqs(2, '', 'portray(Attribute)'),
%	attribute_contents(Attribute, Target, _Disequalities), !,
%	portray(Target).

%portray(Term) :-
%	print_vars_diseqs(2, '', 'portray(Term)'),
%	print_vars_diseqs(1, 'aux', Term).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%print_vars_diseqs(Echo_Level, Mode, File_Name, Term) :-
%	print_msg(Echo_Level, Mode, File_Name, Term, ' '). % Space for reading.

print_vars_diseqs(_FI, '', []) :- !.
print_vars_diseqs(FI, Pre_Msg, []) :- 
	Pre_Msg \== '', !,
	print_msg(FI, 3, '', Pre_Msg, '[]'), !.

print_vars_diseqs(FI, '', [Var | Vars]) :- !,
	prepare_attributes_for_printing(Var, Attributes_For_Printing_Conj),
	print_msg(FI, 3, 'aux', Var, ': '),
	print_msg(FI, 3, 'aux', Attributes_For_Printing_Conj, '  '), !,
	print_vars_diseqs(FI, '', Vars), !.
print_vars_diseqs(FI, Pre_Msg, [Var | Vars]) :-
	Pre_Msg \== '',
	prepare_attributes_for_printing(Var, Attributes_For_Printing_Conj),
	print_msg(FI, 3, 'nl', '', ''),
	print_msg(FI, 3, 'aux', Pre_Msg, ''),
	print_msg(FI, 3, 'aux', Var, ': '),
	print_msg(FI, 3, 'aux', Attributes_For_Printing_Conj, '  '), !,
	print_vars_diseqs(FI, '', Vars), !.

print_vars_diseqs(FI, Msg, Term) :- !,
	varsbag(Term, [], [], Vars), !,
	print_vars_diseqs(FI, Msg, Vars), !.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	    
prepare_attributes_for_printing(Term, Attributes_For_Printing_Conj) :-
	print_msg(3, 4, '', 'attribute_goals :: Term', Term),
	get_attributes_in_term_vars(Term, _All_Vars, Vars_With_Attrs, _Vars_Without_Attrs), !,
	format_attributes_for_printing(Vars_With_Attrs, Attributes_For_Printing), !,
	attrs_list_to_conj(Attributes_For_Printing, Attributes_For_Printing_Conj), !,
	print_msg(3, 4, '', 'attribute_goals :: Attrs', Attributes_For_Printing_Conj), 
	!. % Backtracking forbidden.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_attributes_in_term_vars(Term, All_Vars, Vars_With_Attrs, Vars_Without_Attrs) :-
	cneg_aux:varsbag(Term, [], [], Vars),
	get_attributes_in_term_vars_aux(Vars, [], All_Vars, [], Vars_With_Attrs, [], Vars_Without_Attrs).
%	print_msg(3, 4, '', 'Vars :: (visited, with attributes, without attributes)', (All_Vars, Vars_With_Attrs, Vars_Without_Attrs)). 

% get_attributes_in_term_vars_aux(Vars_In, Visited_In, Visited_Out, VWA_In, VWA_Out, VWOA_In, VWOA_Out)
% Vars_In -> Variables to check for attributes.
% Visited_In -> The ones we have checked before.
% Visited_Out -> The ones checked when this pred ends.
% VWA_In, VWA_Out, -> The vars WITH attributes.
% VWOA_In, VWOA_Out -> The vars WITHOUT attributes.
%
get_attributes_in_term_vars_aux([], Visited, Visited, VWA, VWA, VWOA, VWOA) :- !.
get_attributes_in_term_vars_aux([Var | Vars_In], Visited_In, Visited_Out, VWA_In, VWA_Out, VWOA_In, VWOA_Out) :-
	get_attribute_local(Var, Attr), !,
	varsbag(Attr, [Var | Visited_In], Vars_In, Vars_Out), !,
	get_attributes_in_term_vars_aux(Vars_Out, [Var | Visited_In], Visited_Out, [Attr | VWA_In], VWA_Out, VWOA_In, VWOA_Out).
get_attributes_in_term_vars_aux([Var | Vars], Visited_In, Visited_Out, VWA_In, VWA_Out, VWOA_In, VWOA_Out) :-
	get_attributes_in_term_vars_aux(Vars, [Var | Visited_In], Visited_Out, VWA_In, VWA_Out, [ Var | VWOA_In ], VWOA_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_attributes_for_printing([], []) :- !.
format_attributes_for_printing([Attribute|Attributes], [Printing_Attribute|Printing_Attributes]) :-
%	print_msg(3, 4, '', 'format_attribute_for_printing :: Attribute', Attribute), 
	format_attribute_for_printing(Attribute, Printing_Attribute),
%	print_msg(3, 4, '', 'format_attribute_for_printing :: Printing_Attribute', Printing_Attribute),
	format_attributes_for_printing(Attributes, Printing_Attributes).

format_attribute_for_printing(Attribute, Printing_Attribute) :-
	attribute_contents(Attribute, _Target, Disequalities),
	format_diseqs_list_for_printing(Disequalities, Printing_Attribute).

format_diseqs_list_for_printing([], []) :- !.
format_diseqs_list_for_printing([Disequality | Disequalities], [Print_Disequality | Print_Disequalities]) :-
%	print_msg(3, 4, '', 'format_diseq_for_printing :: Disequality', Disequality), 
	format_diseq_for_printing(Disequality, Print_Disequality),
%	print_msg(3, 4, '', 'format_diseq_for_printing :: Print_Disequality', Print_Disequality), 
	format_diseqs_list_for_printing(Disequalities, Print_Disequalities).

% Need to convert to a single term everything.
% This predicate is not working as expected.
format_diseq_for_printing(Disequality, Print_Disequality) :-
	attribute_disequality_contents(Disequality, T1, T2, _EQ_Vars_In, UQ_Vars_In),
	varsbag_clean_up(UQ_Vars_In, UQ_Vars),
	varsbag((T1, T2), [], [], Terms_Vars), 
	varsbag_intersection(UQ_Vars, Terms_Vars, Real_UQ_Vars),
	functor(Print_Disequality, 'disequality', 3), 
	arg(1, Print_Disequality, T1), 
	arg(2, Print_Disequality, T2), 
	arg(3, Print_Disequality, Real_UQ_Vars).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

attrs_list_to_conj([], []) :- !. % No attributes.
attrs_list_to_conj(Attrs_List, Attrs_Conj) :- % One or more attributes.
	Attrs_List \== [], !,
	attrs_list_to_conj_aux(Attrs_List, Attrs_Conj).

attrs_list_to_conj_aux([Elto], Real_Elto) :- !,
	attrs_list_to_conj_aux(Elto, Real_Elto).
attrs_list_to_conj_aux([Elto | List], New_Elto) :- !,
	functor(New_Elto, '/\\', 2),
	attrs_list_to_conj_aux(Elto, Real_Elto),
	arg(1, New_Elto, Real_Elto), 
	arg(2, New_Elto, More_Eltos), 
	attrs_list_to_conj(List, More_Eltos).
attrs_list_to_conj_aux(Elto, Elto) :- 
	Elto \== [], !. % An empty list is not an individual.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% CONSTRAINT VERIFICATION %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

attr_unify_hook(Attribute, Value) :-
	print_msg(3, 4, 'nl', '', ''),
	print_msg(3, 4, '', 'attr_unify_hook :: Attr_Fomat', 'var_attribute(Target, [disequality(Diseq_1, Diseq_2, EQ_Vars, UQ_Vars)])'),
	print_msg(3, 4, '', 'attr_unify_hook :: (Attribute, Value)', (Attribute, Value)),
	attr_unify_hook_aux(Attribute, Value).

attr_unify_hook_aux(Attribute, Value) :-
	var(Value),
	get_attribute_local(Value, Attribute_Value), !,
	remove_attribute_local(Value), 
	print_msg(3, 4, '', 'attr_unify_hook :: Attribute_Value', Attribute_Value),
	attribute_contents(Attribute, _OldTarget_Var_1, Diseqs_Var_1), 
	attribute_contents(Attribute_Value, _OldTarget_Var_2, Diseqs_Var_2), 
	!,
	cneg_aux:append(Diseqs_Var_1, Diseqs_Var_2, Diseqs),
	test_and_update_vars_attributes(Diseqs).

attr_unify_hook_aux(Attribute, Value) :-
	var(Value), !,
	print_msg(3, 4, '', 'attr_unify_hook :: Var_Value (no attr)', Value),
	attribute_contents(Attribute, _OldTarget_Var_1, Diseqs), 
	print_msg(3, 4, '', 'test_and_update_vars_attributes :: (Disequalities)', (Diseqs)), 
	test_and_update_vars_attributes(Diseqs).

attr_unify_hook_aux(Attribute, Value) :-
	!,
	print_msg(3, 4, '', 'attr_unify_hook :: Nonvar_Value (no attr)', Value),
	attribute_contents(Attribute, _OldTarget_Var_1, Diseqs), 
	print_msg(3, 4, '', 'test_and_update_vars_attributes :: (Disequalities)', (Diseqs)), 
	test_and_update_vars_attributes(Diseqs).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%perform_substitutions([], _EQV, _UQV) :- !.
%perform_substitutions([(OldTarget, NewTarget) | MoreSubst], EQV, UQV) :-
%	varsbag(UQV, [], [], UQV_Aux), !, % Only vars, please.
%	varsbag(EQV, UQV_Aux, [], EQV_Aux), !, % Only vars, please.
%	varsbag((OldTarget, NewTarget), [], [], Vars_Targets), !,
%	varsbag_intersection(Vars_Targets, UQV_Aux, Intersection), !,
%	(
%	    (
%		Intersection == [], !,
%		OldTarget = NewTarget,
%		perform_substitutions(MoreSubst, EQV_Aux, UQV_Aux)
%	    )
%	;
%	    (
%		Intersection \== [], !,
%		print_msg(3, 4, '', 'perform_substitutions :: Impossible :: (OldTarget, NewTarget, EQV, UQV)', (OldTarget, NewTarget, EQV_Aux, UQV_Aux)),
%		!, fail
%	    )
%	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_and_remove_eqv_and_uqv_from_diseqs([], EQV_In, EQV_In, UQV_In, UQV_In, []) :- !.
get_and_remove_eqv_and_uqv_from_diseqs([Diseq | Diseqs], EQV_In, EQV_Out, UQV_In, UQV_Out, [(T1, T2) | More_Ts]) :-
	attribute_disequality_contents(Diseq, T1, T2, Diseq_EQV_In, Diseq_UQV_In),
	varsbag_clean_up(Diseq_EQV_In, Diseq_EQV),
	varsbag_clean_up(Diseq_UQV_In, Diseq_UQV),
	varsbag(Diseq_EQV, [], EQV_In, EQV_Aux),
	varsbag(Diseq_UQV, [], UQV_In, UQV_Aux),
	get_and_remove_eqv_and_uqv_from_diseqs(Diseqs, EQV_Aux, EQV_Out, UQV_Aux, UQV_Out, More_Ts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% An~ade la formula al atributo de las variables implicadas
% Por q tendriamos q tener en cuenta otros atributos?
% Como cada uno tiene su manejador, tratar de mezclar los atributos no aporta nada.

test_and_update_vars_attributes(New_Diseqs) :-
	print_msg(3, 4, '', 'test_and_update_vars_attributes :: New_Diseqs', New_Diseqs),  

	cneg_aux:varsbag(New_Diseqs, [], [], New_Diseqs_Vars), !,
	retrieve_affected_disequalities(New_Diseqs_Vars, [], [], Old_Diseqs), !,
	print_msg(3, 4, '', 'test_and_update_vars_attributes :: Old_Diseqs', Old_Diseqs),

	% Get which variables are EQV so we distinguish them from UQV.
	get_and_remove_eqv_and_uqv_from_diseqs(New_Diseqs, [], ND_EQV, [], ND_UQV, New_Diseqs_Aux),
	get_and_remove_eqv_and_uqv_from_diseqs(Old_Diseqs, [], OD_EQV, [], OD_UQV, Old_Diseqs_Aux),
	
	test_vars_sets_are_exclusive(ND_EQV, ND_UQV, OD_EQV, OD_UQV, All_EQV, All_UQV), % The sets must be exclusive.
	print_msg(3, 4, '', 'test_and_update_vars_attributes :: (All_EQV, All_UQV)', (All_EQV, All_UQV)),
	print_msg(3, 4, '', 'test_and_update_vars_attributes :: New_Diseqs_Aux', New_Diseqs_Aux),
	print_msg(3, 4, '', 'test_and_update_vars_attributes :: Old_Diseqs_Aux', Old_Diseqs_Aux), !,

	% At first we check that the new disequalities can be added to the old ones.
	set_of_diseqs_to_constraints(New_Diseqs_Aux, Old_Diseqs_Aux, [], Simplified_Diseqs, All_EQV),

	print_msg(3, 4, '', 'test_and_update_vars_attributes :: Simplified_Diseqs', Simplified_Diseqs),
	restore_attributes(Simplified_Diseqs, All_EQV, All_UQV).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
retrieve_affected_disequalities([], _Visited_Vars, Diseq_Acc_Out, Diseq_Acc_Out) :- !. % Loop over vars list.
retrieve_affected_disequalities([Var|Vars], Visited_Vars, Diseq_Acc_In, Diseq_Acc_Out):- 
	var(Var), % It cannot be other things ...
	get_attribute_local(Var, Attribute), !,
	attribute_contents(Attribute, Var, Disequalities), 
	remove_attribute_local(Var), 

	cneg_aux:varsbag(Disequalities, [Var|Visited_Vars], Vars, New_Vars),
	cneg_aux:append(Disequalities, Diseq_Acc_In, Diseq_Acc_Aux),
        retrieve_affected_disequalities(New_Vars, [Var|Visited_Vars], Diseq_Acc_Aux, Diseq_Acc_Out).

retrieve_affected_disequalities([Var|Vars_In], Visited_Vars, Diseq_Acc_In, Diseq_Acc_Out) :- 
        retrieve_affected_disequalities(Vars_In, [Var|Visited_Vars], Diseq_Acc_In, Diseq_Acc_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The sets must be exclusive.
test_vars_sets_are_exclusive(ND_EQV, ND_UQV, OD_EQV, OD_UQV, All_EQV, All_UQV) :-
	(
	    (varsbag_intersection(ND_EQV, ND_UQV, []), !)
	;
	    print_msg(3, 4, '', 'Error: common var in (ND_EQV, ND_UQV)', (ND_EQV, ND_UQV))
	),
	(
	    (varsbag_intersection(OD_EQV, OD_UQV, []), !)
	;
	    print_msg(3, 4, '', 'Error: common var in (OD_EQV, OD_UQV)', (OD_EQV, OD_UQV))
	),
	(
	    (varsbag_intersection(OD_EQV, ND_UQV, []), !)
	;
	    print_msg(3, 4, '', 'Error: common var in (OD_EQV, ND_UQV)', (OD_EQV, ND_UQV))
	),
	(
	    (varsbag_intersection(ND_EQV, OD_UQV, []), !)
	;
	    print_msg(3, 4, '', 'Error: common var in (ND_EQV, OD_UQV)', (ND_EQV, OD_UQV))
	),
	varsbag_union(ND_EQV, OD_EQV, All_EQV),
	varsbag_union(ND_UQV, OD_UQV, All_UQV),
	(
	    (varsbag_intersection(All_EQV, All_UQV, []), !)
	;
	    print_msg(3, 4, '', 'Error: common var in (All_EQV, All_UQV)', (All_EQV, All_UQV))
	), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

restore_attributes(Diseqs_In, All_EQV, All_UQV) :- 
	cneg_aux:varsbag(All_EQV, [], [], EQV),
	cneg_aux:varsbag(All_UQV, EQV, [], UQV), % Exhaustive sets, please.
	prepare_diseqs_for_restore(Diseqs_In, Diseqs, EQV, UQV, [], Affected_Vars),
%	print_msg(1, 3, '', 'restore_attributes_vars(Affected_Vars, Diseqs)', (Affected_Vars, Diseqs)),
	restore_attributes_vars(Affected_Vars, Diseqs).

prepare_diseqs_for_restore([], [], _EQV, _UQV, Affected_Vars, Affected_Vars) :- !.
prepare_diseqs_for_restore([(T1, T2) | Diseqs_In], [(Diseq, Vars) | Diseqs_Out], EQV, UQV, Aff_Vars_In, Aff_Vars_Out) :-
	cneg_aux:varsbag((T1, T2), [], [], Vars), 
	cneg_aux:varsbag(Vars, [], Aff_Vars_In, Aff_Vars_Aux), !,
	attribute_disequality_contents(Diseq, T1, T2, EQV, UQV),
	prepare_diseqs_for_restore(Diseqs_In, Diseqs_Out, EQV, UQV, Aff_Vars_Aux, Aff_Vars_Out).

restore_attributes_vars([], _Diseqs) :- !.
restore_attributes_vars([Var | Affected_Vars], Diseqs) :-
	affected_diseqs(Var, Diseqs, Affected_Diseqs),
	restore_attributes_var(Var, Affected_Diseqs),
	restore_attributes_vars(Affected_Vars, Diseqs).

affected_diseqs(_Var, [], []) :- !.
affected_diseqs(Var, [(Diseq, Diseq_Vars) | Diseqs], [Diseq | Affected_Diseqs]) :-
	cneg_aux:memberchk(Var, Diseq_Vars), !,
	affected_diseqs(Var, Diseqs, Affected_Diseqs).
affected_diseqs(Var, [_Diseq | Diseqs], Affected_Diseqs) :-
	affected_diseqs(Var, Diseqs, Affected_Diseqs).

restore_attributes_var(Var, _Diseqs) :-
	var(Var),
	get_attribute_local(Var, Attribute), !,
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, '', 'ERROR: var has an attribute. Attribute: ', Attribute),
	print_msg(1, 3, 'nl', '', ''),
	fail.

restore_attributes_var(Var, Diseqs) :-
	var(Var),
	Diseqs == [],
	!, % We do not want empty attributes.
	fail.

restore_attributes_var(Var, Diseqs) :-
	Diseqs \== [],
	var(Var),

	attribute_contents(Attribute, Var, Diseqs),
	put_attribute_local(Var, Attribute).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% set_of_diseqs_to_constraints(New_Diseqs, Old_Diseqs, Constraints_In, Constraints_Out, All_EQV),
set_of_diseqs_to_constraints([], [], Constraints, Constraints, _All_EQV) :- !.
set_of_diseqs_to_constraints([], [Diseq|Diseqs_List], Constraints_In, Constraints_Out, All_EQV) :- !,
	set_of_diseqs_to_constraints([Diseq|Diseqs_List], [], Constraints_In, Constraints_Out, All_EQV).
set_of_diseqs_to_constraints([Diseq|Diseqs_List], Old_Diseqs, Constraints_In, Constraints_Out, All_EQV) :- !,
	print_msg(0, 0, '', '', ''),
	print_msg(0, 0, '', 'diseqs_to_constraints :: (Diseqs, ---, EQV)', ([Diseq], '---', All_EQV)),
	diseqs_to_constraints([Diseq], Constraints, All_EQV),
	print_msg(0, 0, '', 'diseqs_to_constraints :: Constraints', Constraints),
	print_msg(0, 0, '', '', ''),

	constraints_sets_append(Constraints, Constraints_In, Constraints_Aux),
	set_of_diseqs_to_constraints(Diseqs_List, Old_Diseqs, Constraints_Aux, Constraints_Out, All_EQV).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constraints_sets_append(Diseqs_In, Diseqs_Acc, Diseqs_Out) :-
	print_msg(0, 0, '', 'accumulate_disequations :: Diseqs_In', Diseqs_In),
	print_msg(0, 0, '', 'accumulate_disequations :: Diseqs_Acc', Diseqs_Acc),
	constraints_sets_append_aux(Diseqs_In, Diseqs_Acc, Diseqs_Out),
	print_msg(0, 0, '', 'accumulate_disequations :: Diseqs_Out', Diseqs_Out).

constraints_sets_append_aux([], Constraints_Out, Constraints_Out) :- !.
constraints_sets_append_aux([Diseq | Diseq_List], Constraints_In, Constraints_Out) :-
	cneg_aux:memberchk(Diseq, Constraints_In), !, % It is there.
	constraints_sets_append_aux(Diseq_List, Constraints_In, Constraints_Out).
constraints_sets_append_aux([(T1, T2) | Diseq_List], Constraints_In, Constraints_Out) :-
%	attribute_disequality_contents(Diseq, T1, T2, EQV, UQV),
%	attribute_disequality_contents(Diseq_Aux, T2, T1, EQV, UQV), % Order inversion.
	cneg_aux:memberchk((T2, T1), Constraints_In), !, % It is there.
	constraints_sets_append_aux(Diseq_List, Constraints_In, Constraints_Out).
constraints_sets_append_aux([Diseq | Diseq_List], Constraints_In, Constraints_Out) :-
	constraints_sets_append_aux(Diseq_List, [Diseq | Constraints_In], Constraints_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For the case we do not have a disequality to simplify.
% It is obvious that we must fail.
diseqs_to_constraints([], [], _EQV) :- 
	!, 
	print_msg(3, 4, '', 'diseqs_to_constraints :: Diseqs = [] ---- FAIL ', ''),
	!, fail.
	% Result = 'fail'. % We have failed.

diseqs_to_constraints([(T1, T2) | More_Diseqs], Constraints, EQV) :- % Same var.
        var(T1),
        var(T2), % Both are variables.
        T1==T2, !, % Both are the same variable.
	print_msg(3, 4, '', 'diseqs_to_constraints :: SAME VAR, T1 == T2', (T1, T2)),
	diseqs_to_constraints(More_Diseqs, Constraints, EQV).

diseqs_to_constraints([(T1, T2) | More_Diseqs], Constraints, EQV_In) :- % Different vars.
        var(T1),
        var(T2), !, % Both are variables, but not the same one.
	T1 \== T2, % Not the same variable.
	varsbag(EQV_In, [], [], EQV), % Remove anything there not a variable.
	varsbag((T1, T2), EQV, [], UQV), % Compute UQ vars.
%	print_msg(3, 4, '', 'diseqs_to_constraints :: var(T1) and var(T2)', (T1, T2)),
	(
	    (   % Both are UQ vars.
		cneg_aux:memberchk(T1, UQV), 
		cneg_aux:memberchk(T2, UQV), !,
		print_msg(3, 4, '', 'diseqs_to_constraints :: UNIFY UQV(T1) and UQV(T2)', (T1, T2)),
		cneg_diseq_unify(T1, T2), % They can not be disunified, and they are still UQ vars.
		diseqs_to_constraints(More_Diseqs, Constraints, EQV)
	    )
	;
	    (   % T1 is a UQ var, T2 is not a UQ var.
		cneg_aux:memberchk(T1, UQV), !,
%		cneg_aux:memberchk(T2, EQV), 
		print_msg(3, 4, '', 'diseqs_to_constraints :: UQV(T1) and var(T2)', (T1, T2)),
		diseqs_to_constraints_uqvar_t1_var_t2([(T1, T2) | More_Diseqs], Constraints, EQV)
	    )
	;
	    (   % T2 is a UQ var, T1 is not a UQ var.
%		cneg_aux:memberchk(T1, EQV),
		cneg_aux:memberchk(T2, UQV), !,
		print_msg(3, 4, '', 'diseqs_to_constraints :: UQV(T2) and var(T1)', (T1, T2)),
		diseqs_to_constraints_uqvar_t1_var_t2([(T2, T1) | More_Diseqs], Constraints, EQV)
	    )
	;
	    (   % T1 and T2 are NOT UQ vars. 2 solutions. 
		cneg_aux:memberchk(T1, EQV),
		cneg_aux:memberchk(T2, EQV), !,
		( 
		    (   % First solution: T1 =/= T2.
			print_msg(3, 4, '', 'diseqs_to_constraints :: var(T1) =/= var(T2)', (T1, T2)),
			Constraints = [(T1, T2)]
		    )
		;
		    (   % T1 and T2 can not be disunified. We test if we can fail.
			print_msg(3, 4, '', 'diseqs_to_constraints :: UNIFY var(T1) and var(T2)', (T1, T2)),
			cneg_diseq_unify(T1, T2), % Since they can not be disunified, unify them.
			diseqs_to_constraints(More_Diseqs, Constraints, EQV)
		    )
		)
	    )	
	).

diseqs_to_constraints([(T1, T2) | More_Diseqs], Constraints, EQV) :- % var and nonvar.
	(
	    (   % T1 is a VAR. T2 is not a var.
		var(T1), 
		nonvar(T2), !,
		print_msg(3, 4, '', 'diseqs_to_constraints :: var(T1) and nonvar(T2) ', (T1, T2)),
		diseqs_to_constraints_var_nonvar([(T1, T2) | More_Diseqs], Constraints, EQV)
	    )
	;
	    (   % T2 is a VAR. T1 is not a var.
		var(T2), 
		nonvar(T1), !,
		print_msg(3, 4, '', 'diseqs_to_constraints :: var(T2) and nonvar(T1) ', (T1, T2)),
		diseqs_to_constraints_var_nonvar([(T2, T1) | More_Diseqs], Constraints, EQV)
	    )
	).

diseqs_to_constraints([(T1, T2) | More_Diseqs], Constraints, EQV):- 
	nonvar(T1), 
	nonvar(T2), !,
 	functor_local(T1, Name_1, Arity_1, Args_1),
	functor_local(T2, Name_2, Arity_2, Args_2), 
	(
	    (   % Functors that unify.
		Name_1 == Name_2, 
		Arity_1 == Arity_2, !,
		print_msg(3, 4, '', 'diseqs_to_constraints :: functor(T1) == functor(T2)', (T1, T2)),
		disequalities_lists_product(Args_1, Args_2, Diseq_List),
		cneg_aux:append(Diseq_List, More_Diseqs, New_More_Diseqs),
		diseqs_to_constraints(New_More_Diseqs, Constraints, EQV)
	    )
	;
	    (   % Functors that do not unify.
		(
		    (Name_1 \== Name_2) ; (Arity_1 \== Arity_2)
		), !,
%		print_msg(3, 4, '', 'diseqs_to_constraints :: functor(T1) =/= functor(T2)', (T1, T2)),
%		Result = 'true', % Result is completely valid.
		Constraints = [] % No constraints.
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Esto NO es producto cartesiano.
disequalities_lists_product([], [], []) :- !.
disequalities_lists_product([T1], [T2], [(T1, T2)]) :- !.
disequalities_lists_product([T1 | Args_1], [T2 | Args_2], [(T1, T2) | More_Diseqs]) :- !,
	disequalities_lists_product(Args_1, Args_2, More_Diseqs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

diseqs_to_constraints_uqvar_t1_var_t2([(T1, T2) | More_Diseqs], Constraints, EQV_In) :-
        var(T1),
        var(T2), 
	varsbag(EQV_In, [], [], EQV), % Remove anything there not a variable.
	varsbag((T1, T2), EQV, [], UQV), % Compute UQ vars.
	cneg_aux:memberchk(T1, UQV), % T1 is a uq var, T2 is not a uqvar.
	cneg_aux:memberchk(T2, EQV), !,
	print_msg(3, 4, '', 'diseqs_to_constraints :: UQV(T1) and var(T2) ', (T1, T2)),

	% T1 can not be different from T2. We unify them (failing) and continue.
	cneg_diseq_unify(T1, T2),
	diseqs_to_constraints(More_Diseqs, Constraints, EQV).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

diseqs_to_constraints_var_nonvar([(T1, T2) | More_Diseqs], Constraints, EQV_In):- 
        var(T1),
	nonvar(T2),
        functor_local(T2, Name, Arity, _Args_T2), 
	varsbag(EQV_In, [], [], EQV), % Remove anything there not a variable.
	varsbag((T1, T2), EQV, [], UQV), % Compute UQ vars.
	(
	    (   % A variable is always different from a functor making use of it.
		cneg_aux:varsbag(T2, [], [], Vars_T2),
		cneg_aux:memberchk(T1, Vars_T2), !, % e.g. X =/= s(s(X)).
		print_msg(3, 4, '', 'diseqs_to_constraints :: var(T1) and functor(T2) and T1 in vars(T2)', (T1, T2)),
		Constraints = [] % No constraints.
	    )
	;
	    (   % T1 is a UQ var. Impossible to disunify. Unify !!
		cneg_aux:memberchk(T1, UQV), !,
		print_msg(3, 4, '', 'diseqs_to_constraints :: UQV(T1) and functor(T2)', (T1, T2)),
		(
		    (
			T2 = Functor_Name/Functor_Arity, !,
			functor_local(New_T2, Functor_Name, Functor_Arity, _Args_New_T2),
			cneg_diseq_unify(T1, New_T2)
		    )
		;
		    (
			T2 \== Functor_Name/Functor_Arity, 
			cneg_diseq_unify(T1, T2)
		    )
		),
		diseqs_to_constraints(More_Diseqs, Constraints, EQV)
	    )
	;
	    (   % The variable must not be the functor (use attributed variables).
		cneg_aux:memberchk(T1, EQV), !,
		print_msg(3, 4, '', 'diseqs_to_constraints :: var(T1) =/= functor(T2)', (T1, T2)),
		(
		    (
			T2 = Functor_Name/Functor_Arity, 
			Constraints = [(T1, T2)]
		    )
		;
		    (
			% functor_local(New_T2, Name, Arity, _UQ_Vars_New_T2), 
			% cneg_diseq_unify(Result, 'true'), % Correct result if attr. var. satisfied.
			% Constraints = [(T1, New_T2)] % Constraints is (T1, T2).
			T2 \== Functor_Name/Functor_Arity, 
			Constraints = [(T1, Name/Arity)] % Constraints is (T1, functorT2/arityFunctorT2).
		    )
		;
		    (   % Keep the functor but diseq between the arguments.
			print_msg(3, 4, '', 'diseqs_to_constraints :: UNIFY var(T1) and functor(T2)', (T1, T2)),
			(
			    (
				T2 = Functor_Name/Functor_Arity, 
				functor_local(T1, Functor_Name, Functor_Arity, _Args_T1) % T1 = functor 
			    )
			;
			    (
				T2 \== Functor_Name/Functor_Arity, 
				functor_local(T1, Name, Arity, _Args_T1) % T1 = functor 
			    )
			),
			diseqs_to_constraints([(T1, T2) | More_Diseqs], Constraints, EQV)
		    )
		)
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%remove_vars_with_attributes([], []) :- !.
%remove_vars_with_attributes([Var|List_In], List_Out) :-   % If variables have attributes, remove them from the bag.
%	get_attribute_local(Var, _Attribute), !,
%	remove_vars_with_attributes(List_In, List_Out).
%remove_vars_with_attributes([Var|List_In], [Var|List_Out]) :- % Keep only vars without attributes.
%	remove_vars_with_attributes(List_In, List_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_diseq_unify(T, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% diseq_eq(X,Y) unify X and Y
cneg_diseq_eq(T1, T2, _UQV) :-
	var(T1),
	var(T2),
	T1 == T2, !, % Both are the same var. This is valid if they are in UQV too.
	print_msg(3, 4, '', 'cneg_diseq_eq: same var. ok.', (T1, T2)),
	!.

cneg_diseq_eq(T1, _T2, UQV) :-
	var(T1),
	memberchk(T1, UQV), !,
	print_msg(3, 4, '', 'cneg_diseq_eq: T1 in UQV. fail.', (T1, UQV)),
	!, fail.

cneg_diseq_eq(_T1, T2, UQV) :-
	var(T2),
	memberchk(T2, UQV), !,
	print_msg(3, 4, '', 'cneg_diseq_eq: T2 in UQV. fail.', (T2, UQV)),
	!, fail.

cneg_diseq_eq(T1, T2, _UQV) :-
	var(T1),
	var(T2), !, % Both vars and not in UQV.
	print_msg(3, 4, '', 'cneg_diseq_eq: T1 and T2 not in UQV. ok.', (T1, T2)),
	cneg_diseq_unify(T1, T2), !.

cneg_diseq_eq(T1, T2, _UQV) :-
	var(T1), % Var vs functor using var.
	varsbag(T2, [], [], Vars),
	memberchk(T1, Vars), !,
	print_msg(3, 4, '', 'cneg_diseq_eq: T1 appears in T2. fail.', (T1, T2)),
	!, fail.

cneg_diseq_eq(T1, T2, UQV) :-
	var(T1), !, % Var vs functor definition.
	functor_local(T2, '/', 2, _Args_T2),
	arg(1, T2, Name),
	arg(2, T2, Arity),
	functor_local(T1, Name, Arity, _Args_T1), !,
	print_msg(3, 4, '', 'T1 var, T2 functor definition. ok.', (T1, T2, UQV)),
	!.

cneg_diseq_eq(T1, T2, UQV) :-
	var(T1), !, % Var vs functor
	functor_local(T2, Name, Arity, Args_T2),
	functor_local(T1, Name, Arity, Args_T1), !,
	print_msg(3, 4, '', 'cneg_diseq_eq: T1 var, T2 functor. continue.', (T1, T2, UQV)),
	!, 
	cneg_diseq_eq_args(Args_T1, Args_T2, UQV), !.

cneg_diseq_eq(T1, T2, UQV) :-
	var(T2), !, % Var vs something. Order inversion.
	print_msg(3, 4, '', 'cneg_diseq_eq: T2 var. order inversion. continue.', (T1, T2)), !,
	cneg_diseq_eq(T2, T1, UQV),
	!.

cneg_diseq_eq(T1, T2, UQV) :-
	functor_local(T1, Name, Arity, Args_T1),
	functor_local(T2, Name, Arity, Args_T2), !,
	print_msg(3, 4, '', 'cneg_diseq_eq: T1 and T2 functors. continue.', (T1, T2, UQV)),
	!, % functor vs functor.
	cneg_diseq_eq_args(Args_T1, Args_T2, UQV), !.

cneg_diseq_eq(T1, T2, UQV) :-
	functor_local(T1, Name_T1, Arity_T1, _Args_T1),
	functor_local(T2, Name_T2, Arity_T2, _Args_T2), 
	(
	    (   Name_T1 \== Name_T2  )
	;
	    (   Arity_T1 \== Arity_T2  )
	),
	print_msg(3, 4, '', 'cneg_diseq_eq: T1 and T2 different functors. fail.', (T1, T2, UQV)),
	!, % functor vs functor.
	fail.


cneg_diseq_eq_args([], [], _UQV) :- !.
cneg_diseq_eq_args([Arg_T1 | Args_T1], [Arg_T2 | Args_T2], UQV) :-
	cneg_diseq_eq(Arg_T1, Arg_T2, UQV), !,
	cneg_diseq_eq_args(Args_T1, Args_T2, UQV), !.

% eq(X,Y):-
 %       X=Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_disequalities_from_constraints(Anything, Vars_With_Attrs) :-
	get_attributes_in_term_vars(Anything, _All_Vars, Vars_With_Attrs, _Vars_Without_Attrs), !,
	print_msg(3, 4, '', 'get_disequalities_from_constraints :: Vars_With_Attrs', Vars_With_Attrs).

get_disequalities_from_constraints_and_remove_them([], []) :- !.
get_disequalities_from_constraints_and_remove_them(Anything, Vars_With_Attrs) :-
	get_disequalities_from_constraints(Anything, Vars_With_Attrs),
	varsbag(Vars_With_Attrs, [], [], Vars),
	print_msg(3, 4, '', 'get_disequalities_from_constraints_and_remove_them :: Vars', Vars),
	remove_all_constraints_in_variables(Vars), !.

remove_all_constraints_in_variables(Anything) :-
	get_disequalities_from_constraints(Anything, Vars_With_Attrs),
	remove_this_constraints_from_affected_variables(Vars_With_Attrs).

remove_this_constraints_from_affected_variables([]) :- !.
remove_this_constraints_from_affected_variables([Var_With_Attrs | Vars_With_Attrs]) :-
	remove_this_constraints_from_affected_variables_aux(Var_With_Attrs), !,
	remove_this_constraints_from_affected_variables(Vars_With_Attrs).

remove_this_constraints_from_affected_variables_aux(Var_With_Attrs) :-
	varsbag(Var_With_Attrs, [], [], Vars),
	remove_this_constraints_from_variables(Var_With_Attrs, Vars).

remove_this_constraints_from_variables(_Var_With_Attrs, []) :- !.
remove_this_constraints_from_variables(Var_With_Attrs, [Var | Vars]) :-
	get_attribute_local(Var, Attribute), !,
	remove_attribute_local(Var), !,
	attributes_difference(Attribute, Var_With_Attrs, Difference), !,
	put_attribute_local(Var, Difference), !,
	remove_this_constraints_from_variables(Var_With_Attrs, Vars).
remove_this_constraints_from_variables(Var_With_Attrs, [_Var | Vars]) :-
	remove_this_constraints_from_variables(Var_With_Attrs, Vars).

attributes_difference(Attribute, Var_With_Attrs, Difference) :-
	print_msg(3, 4, '', 'attributes_difference :: Attribute', Attribute),
	print_msg(3, 4, '', 'attributes_difference :: Var_With_Attrs', Var_With_Attrs),
	attribute_contents(Attribute, Target, Disequalities),
	Var_With_Attrs = Disequalities,
	attribute_contents(Difference, Target, []).

attribute_diseq_to_executable_diseq(Constraint_Diseq, Executable_Diseq) :-
	attribute_disequality_contents(Constraint_Diseq, Diseq_1, Diseq_2, EQ_Vars, UQ_Vars),
	functor_local(Executable_Diseq, 'diseq_geuqv', 5, [Diseq_1 |[ Diseq_2 |[ [] |[ EQ_Vars |[ UQ_Vars ]]]]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     PREDICADO   DISTINTO                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Predicado que implementa mediante variables con atributo
% la desigualdad entre terminos y como expresarlo a
% traves de disyuncion de conjunciones de desigualdades
% entre terminos que debe satisfacer cada variable.
% Esta implementacion sirve para variables con dominios
% de valores finitos.

% Incluye una desigualdad en las formulas de las 
% variables implicadas

disequality(T1,T2, UQV_In) :- 
	varsbag_clean_up(UQV_In, UQV), % Only variables, please.	
	varsbag((T1, T2), UQV, [], EQV), % Only variables, please.
	diseq_geuqv(T1,T2, EQV, UQV).

diseq_geuqv(T1,T2, EQV, UQV) :- 
	print_msg(3, 4, 'nl', '', ''),
	print_msg(3, 4, '', 'diseq_geuqv [tmp] :: ((T1, =/=, T2), ---, (GV, EQV, UQV))', ((T1, '=/=', T2), '---', (EQV, UQV))),
	attribute_disequality_contents(Disequality, T1, T2, EQV, UQV),
        test_and_update_vars_attributes([Disequality]),
	print_msg(3, 4, '', 'diseq_geuqv [out] :: ((T1, =/=, T2))', ((T1, '=/=', T2))),
	print_msg(3, 4, 'nl', '', '').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

equality(T1,T2, UQV_In) :- 
	varsbag_clean_up(UQV_In, UQV), % Only variables, please.	
	varsbag((T1, T2), UQV, [], EQV), % Only variables, please.
	eq_geuqv(T1,T2, EQV, UQV).

eq_geuqv(T1, T2, EQV, UQV) :- 
	print_msg(3, 4, 'nl', '', ''),
	print_msg(3, 4, '', 'eq_geuqv [tmp] :: (T1, =, T2), ---, (EQV, UQV)', ((T1, '=', T2), '---', (EQV, UQV))),
	cneg_diseq_eq(T1, T2, UQV),
	print_msg(3, 4, '', 'eq_geuqv [out] :: ((T1, =, T2))', ((T1, '=', T2))),
	print_msg(3, 4, 'nl', '', '').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


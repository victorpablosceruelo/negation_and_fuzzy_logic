:- module(cneg_diseq, 
	[
	    disequality/3, equality/3, 
	    cneg_diseq/6, cneg_eq/6,
	    portray_attributes_in_term/1
	], 
	[assertions]).

:- use_module(cneg_aux,_).

%:- use_package(debug).
%:- use_package(trace).
%:- use_package(nodebug).

% For Ciao Prolog:
:- multifile 
        verify_attribute/2,
        combine_attributes/2,
	portray_attribute/2,
	portray/1.

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
	debug_msg_aux(1, '', '% cneg :: '),
	debug_msg_aux(1, 'remove_attribute_local :: Var :: ', Var),
	detach_attribute(Var),
	debug_msg_aux(1, '  -->> Var :: ', Var),
	debug_msg_nl(1).
% XSB:	del_attr(Var, dist).

get_attribute_local(Var, Attribute) :-
	get_attribute(Var, Attribute).
% XSB:	get_attr(Var, dist, Attribute),
%	debug_msg(0, 'get_attribute_local :: (Var, Attribute)', (Var, Attribute)).

put_attribute_local(Var, Attribute) :-
	debug_msg_aux(1, '', '% cneg :: '),
	debug_msg_aux(1, 'put_attribute_local :: Attribute :: ', Attribute),
	debug_msg_aux(1, '  Var :: ', Var), 
%	get_attribute_if_any(Var), !,
	attach_attribute(Var, Attribute),
	debug_msg_aux(1, '  -->> Var :: ', Var),
	debug_msg_nl(1).
%	put_attr(Var, dist, Attribute).

%get_attribute_if_any(Var) :-
%	debug_msg(0, 'Testing if var has any attribute. Var: '),
%	debug_msg(0, Var),
%	get_attribute_local(Var, _Attribute), !.
%get_attribute_if_any(Var) :-
%	debug_msg(0, 'Testing if var has any attribute. Var: '),
%	debug_msg(0, Var),
%	debug_msg(0, ' has NO attribute').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Attributes contents are encapsulated via the following structure.

%:- dynamic var_attribute/2.
attribute_contents(var_attribute(Target, Disequalities, UnivVars), Target, Disequalities, UnivVars).
disequality_contents(disequality(Diseq_1, Diseq_2), Diseq_1, Diseq_2).
equality_contents(equality(T1, T2), T1, T2).

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


portray_attribute(Attr, Var) :-
	debug_msg(2, 'portray_attribute :: (Attr, Var)', (Attr, Var)).

portray(Attribute) :-
	attribute_contents(Attribute, _Target, Disequalities, UnivVars), !,
	portray_disequalities(Disequalities, UnivVars).

portray(Anything) :- 
	debug_msg_aux(2, '', Anything).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

portray_attributes_in_term(T) :-
	cneg_aux:varsbag(T, [], [], Variables),
	debug_msg(0, 'Attributes for the variables in term', T),
	portray_attributes_in_variables(Variables).

portray_attributes_in_variables([]) :- !.
portray_attributes_in_variables([Var|Vars]) :-
	portray_attributes_in_variable(Var),
	portray_attributes_in_variables(Vars).

portray_attributes_in_variable(Var) :-
	get_attribute_local(Var, Attribute),
	debug_msg(2, 'variable', Var), 
	portray(Attribute).
portray_attributes_in_variable(Var) :-
	debug_msg(2, Var, ' has NO attribute').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

portray_disequalities(Disequalities, UnivVars) :-
	portray_disequalities_aux_1(Disequalities), 
	portray_disequalities_aux_3(UnivVars).

portray_disequalities_aux_1([]) :- !.
portray_disequalities_aux_1([Diseq_1]) :- !,
	portray_disequalities_aux_2(Diseq_1).
portray_disequalities_aux_1([Diseq_1|Diseqs]) :- !,
	portray_disequalities_aux_2(Diseq_1), 
	debug_msg_aux(2, ' AND ', ''),
	portray_disequalities_aux_1(Diseqs).

portray_disequalities_aux_2(Diseq) :-
	disequality_contents(Diseq, Diseq_1, Diseq_2),
	debug_msg_aux(2, '[ ', Diseq_1),
	debug_msg_aux(2, ' =/= ', Diseq_2),
	debug_msg_aux(2, '', ' ]').

portray_disequalities_aux_3([]).
portray_disequalities_aux_3(UnivVars) :-
	UnivVars \== [], !,
	debug_msg_aux(2, ', Universally quantified: [', ''), 
	portray_disequalities_aux_4(UnivVars),
	debug_msg_aux(2, '', ' ]').

portray_disequalities_aux_4([]) :- !.
portray_disequalities_aux_4([FreeVar | FreeVars]) :-
	debug_msg_aux(2, ' ', FreeVar),
	portray_disequalities_aux_4(FreeVars).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% CONSTRAINT VERIFICATION %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify_attribute(Attribute, Target):-
	debug_msg(1, 'verify_attribute :: (Attribute, Target)', (Attribute, Target)), 
	attribute_contents(Attribute, NewTarget, Disequalities, UQV_In), 
	terms_are_equal(Target, NewTarget), !,
	diseq_status(Status, UQV_In, _UQV_Out, 'true', 'true'),
	debug_msg(1, 'test_and_update_vars_attributes :: (Status, Disequalities)', (Status, Disequalities)), 
%	test_and_update_vars_attributes(Status_In, Substitutions, New_Disequalities)
	test_and_update_vars_attributes(Status, [], Disequalities).


% Only for Ciao prolog 
verify_attribute(Attribute, NewTarget):-
%	debug_msg(0, 'Only for Ciao Prolog: '),
	debug_msg(1, 'verify_attribute(Attribute, NewTarget)', verify_attribute(Attribute, NewTarget)), 
	attribute_contents(Attribute, OldTarget, Disequalities, UQV_In), !,
	substitution_contents(Subst, OldTarget, NewTarget),
	diseq_status(Status, UQV_In, _UQV_Out, 'true', 'true'),
	debug_msg(1, 'test_and_update_vars_attributes :: (Status, Disequalities)', (Status, Disequalities)), 
	debug_msg(1, 'test_and_update_vars_attributes :: Subst', Subst), 
%	test_and_update_vars_attributes(Status_In, Substitutions, New_Disequalities)
	test_and_update_vars_attributes(Status, [Subst], Disequalities).

substitution_contents(substitute(Var, T), Var, T).

combine_attributes(Attribute_Var_1, Attribute_Var_2) :-
	debug_msg(0, 'combine_attributes :: Attr_Var1 :: (Attr, Target, Diseqs, UV)', Attribute_Var_1),
	debug_msg(0, 'combine_attributes :: Attr_Var2 :: (Attr, Target, Diseqs, UV)', Attribute_Var_2),
	attribute_contents(Attribute_Var_1, OldTarget_Var_1, Disequalities_Var_1, UQV_Var_1), !,
	attribute_contents(Attribute_Var_2, OldTarget_Var_2, Disequalities_Var_2, UQV_Var_2), !,

	cneg_aux:append(Disequalities_Var_1, Disequalities_Var_2, Disequalities),
	cneg_aux:append(UQV_Var_1, UQV_Var_2, UQV_In),
	substitution_contents(Subst, OldTarget_Var_1, OldTarget_Var_2),
	
	diseq_status(Status, UQV_In, _UQV_Out, 'true', 'true'),
	debug_msg(1, 'test_and_update_vars_attributes :: (Status, Disequalities)', (Status, Disequalities)), 
	debug_msg(1, 'test_and_update_vars_attributes :: Subst', Subst), 
	test_and_update_vars_attributes(Status, [Subst], Disequalities).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% An~ade la formula al atributo de las variables implicadas
% Por q tendriamos q tener en cuenta otros atributos?
% Como cada uno tiene su manejador, tratar de mezclar los atributos no aporta nada.

test_and_update_vars_attributes(Status_In, Substitutions, New_Disequalities) :-
	diseq_status(Status_In, UQV_In, UQV_Out, Do_Not_Fail, Result),
	debug_msg(0, 'test_and_update_vars_attributes :: Status_In', Status_In), 
	debug_msg(0, 'test_and_update_vars_attributes :: Substitutions', Substitutions),
	debug_msg(0, 'test_and_update_vars_attributes :: New_Disequalities', New_Disequalities),  

	cneg_aux:varsbag(New_Disequalities, [], UQV_In, Vars_In), !,
	retrieve_affected_disequalities(Vars_In, [], UQV_In, UQV_Tmp, [], Old_Disequalities), !,
	debug_msg(0, 'test_and_update_vars_attributes :: Old_Disequalities', Old_Disequalities),

	perform_substitutions(Substitutions, UQV_Tmp, UQV_Aux_1), !,

	% At first we check that the new disequalities can be added to the old ones.
	diseq_status(Status_Aux_1, UQV_Aux_1, UQV_Aux_2, Do_Not_Fail, Result),
	simplify_disequations(New_Disequalities, Status_Aux_1, [], Simplified_Disequalities_1),

	% At last we check that the old disequalities are still valid.
	diseq_status(Status_Aux_2, UQV_Aux_2, UQV_Out, 'true', 'true'),
	simplify_disequations(Old_Disequalities, Status_Aux_2, [], Simplified_Disequalities_2),

	% Now we aggregate all of them.
	accumulate_disequations(Simplified_Disequalities_1, Simplified_Disequalities_2, Simplified_Disequalities),

	debug_msg(0, 'test_and_update_vars_attributes :: Simplified_Disequalities', Simplified_Disequalities),
	debug_msg(0, 'test_and_update_vars_attributes :: Status_Out', Status_In), 
	restore_attributes(UQV_Out, Simplified_Disequalities).

retrieve_affected_disequalities([], _Vars_Examined, UV_Out, UV_Out, Diseq_Acc_Out, Diseq_Acc_Out) :- !. % Loop over vars list.
retrieve_affected_disequalities([Var|Vars], Vars_Examined, UV_In, UV_Out, Diseq_Acc_In, Diseq_Acc_Out):- 
	var(Var), % It cannot be other things ...
	get_attribute_local(Var, Attribute), !,
	attribute_contents(Attribute, Var, ThisVar_Disequalities, Attribute_UnivVars), 
	remove_attribute_local(Var), 

	% Store info.
	filter_out_nonvars(Attribute_UnivVars, UnivVars),
	varsbag_addition(UnivVars, UV_In, UV_Aux),
	cneg_aux:varsbag((ThisVar_Disequalities, Attribute_UnivVars), [Var|Vars_Examined], Vars, New_Vars), !,
	accumulate_disequations(ThisVar_Disequalities, Diseq_Acc_In, Diseq_Acc_Aux),
        retrieve_affected_disequalities(New_Vars, [Var|Vars_Examined], UV_Aux, UV_Out, Diseq_Acc_Aux, Diseq_Acc_Out).

retrieve_affected_disequalities([Var|Vars_In], Vars_Examined, UV_In, UV_Out, Diseq_Acc_In, Diseq_Acc_Out) :- 
        retrieve_affected_disequalities(Vars_In, [Var|Vars_Examined], UV_In, UV_Out, Diseq_Acc_In, Diseq_Acc_Out).


% Substitutions need a revision in depth.
perform_substitutions([], UV_In, UV_In) :- !.
perform_substitutions([Subst | MoreSubst], UV_In, UV_Out) :-
	debug_msg(1, 'perform_substitutions :: Subst', Subst),
	substitution_contents(Subst, OldTarget, NewTarget),
	(
	    (
		var(OldTarget),
		var(NewTarget), !, % Both are vars.
		perform_substitution_vars(OldTarget, NewTarget, UV_In, UV_Aux)
	    )
	;
	    (
		var(OldTarget), !, % Only OldTarget is var.
		perform_substitution_var_nonvar(OldTarget, NewTarget, UV_In, UV_Aux)
	    )
	;
	    (
		var(NewTarget), !, % Only NewTarget is var.
		perform_substitution_var_nonvar(NewTarget, OldTarget, UV_In, UV_Aux)
	    )
	;
	    (
		perform_substitution_nonvars(OldTarget, NewTarget, UV_In, UV_Aux)
	    )
	),
	perform_substitutions(MoreSubst, UV_Aux, UV_Out).	

perform_substitution_vars(OldTarget, NewTarget, UV_In, UV_Out) :-
	var(OldTarget),
	var(NewTarget), % Be sure both are vars.
	(
	    (
		cneg_aux:memberchk(OldTarget, UV_In),
		cneg_aux:memberchk(NewTarget, UV_In), % Both are universally quantified.
		diseq_eq(UV_In, UV_Out), ! % Keep universal quantification.
	    )
	;
	    (
		varsbag_remove_var(OldTarget, UV_In, UV_Aux),
		varsbag_remove_var(NewTarget, UV_Aux, UV_Out)
	    )
	),
	diseq_eq(OldTarget, NewTarget), !.

perform_substitution_var_nonvar(OldTarget, NewTarget, UV_In, UV_Out) :-
	var(OldTarget), % Be sure OldTarget is a var.
	varsbag_remove_var(OldTarget, UV_In, UV_Out),
	diseq_eq(OldTarget, NewTarget), !.

perform_substitution_nonvars(OldTarget, NewTarget, UV_In, UV_Out) :-
	functor_local(OldTarget, Name, Arity, Args_1),
	functor_local(NewTarget, Name, Arity, Args_2), !,
	substitutions_cartesian_product(Args_1, Args_2, Subst_List),
	perform_substitutions(Subst_List, UV_In, UV_Out).

% Esto NO es producto cartesiano.
substitutions_cartesian_product([], [], []) :- !.
substitutions_cartesian_product([T1], [T2], [Diseq]) :- !,
	 substitution_contents(Diseq, T1, T2).
substitutions_cartesian_product([T1 | Args_1], [T2 | Args_2], [Diseq | Args]) :- !,
	substitution_contents(Diseq, T1, T2),
	substitutions_cartesian_product(Args_1, Args_2, Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

restore_attributes(UQV, Diseqs) :- 
	varsbag(Diseqs, [], [], Affected_Vars),
	debug_msg(0, 'restore_attributes_vars(Affected_Vars, UQV, Diseqs)', (Affected_Vars, UQV, Diseqs)),
	restore_attributes_vars(Affected_Vars, UQV, Diseqs).

restore_attributes_vars([], _UQV_In, _Diseqs) :- !.
restore_attributes_vars([Var | Affected_Vars], UQV_In, Diseqs) :-
	affected_diseqs(Var, Diseqs, Affected_Diseqs),
	restore_attributes_var(Var, UQV_In, Affected_Diseqs),
	restore_attributes_vars(Affected_Vars, UQV_In, Diseqs).

affected_diseqs(_Var, [], []) :- !.
affected_diseqs(Var, [Diseq | Diseqs], [Diseq | Affected_Diseqs]) :-
	cneg_aux:varsbag(Diseq, [], [], Diseq_Vars),
	cneg_aux:memberchk(Var, Diseq_Vars), !,
	affected_diseqs(Var, Diseqs, Affected_Diseqs).
affected_diseqs(Var, [_Diseq | Diseqs], Affected_Diseqs) :-
	affected_diseqs(Var, Diseqs, Affected_Diseqs).

restore_attributes_var(Var, _UQV_In, _Affected_Diseqs) :-
	var(Var),
	get_attribute_local(Var, Attribute), !,
	debug_msg_nl(0),
	debug_msg(0, 'ERROR: var has an attribute. Attribute: ', Attribute),
	debug_msg_nl(0),
	fail.

restore_attributes_var(Var, UQV_In, Affected_Diseqs) :-
	var(Var),
	cneg_aux:varsbag((Var, Affected_Diseqs), [], [], Affected_Vars),
	varsbag_intersection(Affected_Vars, UQV_In, UQV_Affected),
	!,
	(
	    (
		UQV_Affected == [],
		Affected_Diseqs == [],
		! % We do not want empty attributes.
	    )
	;
	    (
		attribute_contents(Attribute, Var, Affected_Diseqs, UQV_Affected),
		put_attribute_local(Var, Attribute)
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accumulate_disequations(Diseqs_In, Diseqs_Acc, Diseqs_Out) :-
	debug_msg(0, 'accumulate_disequations :: Diseqs_In', Diseqs_In),
	debug_msg(0, 'accumulate_disequations :: Diseqs_Acc', Diseqs_Acc),
	accumulate_disequations_aux(Diseqs_In, Diseqs_Acc, Diseqs_Out),
	debug_msg(0, 'accumulate_disequations :: Diseqs_Out', Diseqs_Out).

accumulate_disequations_aux([], Diseq_Acc_Out, Diseq_Acc_Out) :- !.
accumulate_disequations_aux([Diseq | Diseq_List], Diseq_Acc_In, Diseq_Acc_Out) :-
	cneg_aux:memberchk(Diseq, Diseq_Acc_In), !, % It is there.
	accumulate_disequations_aux(Diseq_List, Diseq_Acc_In, Diseq_Acc_Out).
accumulate_disequations_aux([Diseq | Diseq_List], Diseq_Acc_In, Diseq_Acc_Out) :-
	disequality_contents(Diseq, T1, T2),
	disequality_contents(Diseq_Aux, T2, T1), % Order inversion.
	cneg_aux:memberchk(Diseq_Aux, Diseq_Acc_In), !, % It is there.
	accumulate_disequations_aux(Diseq_List, Diseq_Acc_In, Diseq_Acc_Out).
accumulate_disequations_aux([Diseq | Diseq_List], Diseq_Acc_In, Diseq_Acc_Out) :-
	accumulate_disequations_aux(Diseq_List, [Diseq | Diseq_Acc_In], Diseq_Acc_Out).

% Note that each disequality analized gets a clean status on its Result variable.
% This is because all of them need to be satisfied, we should not override the status of
% a previous disequality with the status of the current one.
simplify_disequations([], Status_In, Diseq_Acc_In, Diseq_Acc_In) :- !,
	diseq_status(Status_In, UQV_In, UQV_In, _Do_Not_Fail, 'true').

simplify_disequations([Diseq|Diseq_List], Status_In, Diseq_Acc_In, Diseq_Acc_Out) :- !,
	diseq_status(Status_In, UQV_In, UQV_Out, Do_Not_Fail, Result_In),
	diseq_status(Status_Aux, UQV_In, UQV_Aux, Do_Not_Fail, Result_Aux),
	simplify_disequation([Diseq], Status_Aux, Simplified_Diseq),
	and_between_statuses(Result_Aux, Result_Out, Result_In),
	accumulate_disequations(Simplified_Diseq, Diseq_Acc_In, Diseq_Acc_Aux),
	diseq_status(Status_Out, UQV_Aux, UQV_Out, Do_Not_Fail, Result_Out),
	simplify_disequations(Diseq_List, Status_Out, Diseq_Acc_Aux, Diseq_Acc_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% just for debug.
simplify_disequation(Diseqs, Status, Answer) :-
	debug_msg_nl(0),
	debug_msg(1, 'simplify_disequation :: (Diseqs, Status)', (Diseqs, Status)),
	simplify_disequation_aux(Diseqs, Status, Answer),
	debug_msg(1, 'simplify_disequation :: (Status, Answer)', (Status, Answer)),
	debug_msg_nl(0).

% For the case we do not have a disequality to simplify.
% The answer is obviously empty, but we might fail because of Do_Not_Fail = fail.
simplify_disequation_aux([], Status_In, []) :- 
	!,
	debug_msg(1, 'simplify_disequation_aux :: Diseqs = []', ''),
	diseq_status(Status_In, UQV_In, UQV_Out, Do_Not_Fail, Result),
	(   (   Do_Not_Fail == 'true', debug_msg(1, 'Not allowed to return fail.', ''), !, fail )
	;   (	Do_Not_Fail == 'fail', debug_msg(1, 'Return value is fail.', ''),
		UQV_Out = UQV_In, 
		Result = 'fail' % We have failed.
	    )
	).

simplify_disequation_aux([Diseq | More_Diseqs], Status_In, Answer) :- % Same var.
	disequality_contents(Diseq, T1, T2),
        var(T1),
        var(T2), % Both are variables.
        T1==T2, !, % Both are the same variable.
	debug_msg(1, 'simplify_disequation_aux :: SAME VAR, T1 == T2', Diseq),

	diseq_status(Status_In, UQV_In, UQV_Out, Do_Not_Fail, Result),
	(   (   Do_Not_Fail == 'true', debug_msg(1, 'Not allowed to return fail.', ''), !, fail )
	;   (	Do_Not_Fail == 'fail', debug_msg(1, 'No return value yet.', ''),
		!, 
		diseq_status(Status_Out, UQV_In, UQV_Out, Do_Not_Fail, Result),
		simplify_disequation_aux(More_Diseqs, Status_Out, Answer)
	    )
	).

simplify_disequation_aux([Diseq | More_Diseqs], Status_In, Answer) :- % Different vars.
	disequality_contents(Diseq, T1, T2),
        var(T1),
        var(T2), % Both are variables, but not the same one.
	diseq_status(Status_In, UQV_In, _UQV_Out, Do_Not_Fail, _Result),
	cneg_aux:memberchk(T1, UQV_In), % Both are UQ vars.
	cneg_aux:memberchk(T2, UQV_In), !,
	debug_msg(1, 'simplify_disequation_aux :: UNIFY UQV(T1) and UQV(T2)', Diseq),

	(   (   Do_Not_Fail == 'true', debug_msg(1, 'Not allowed to return fail.', ''), !, fail )
	;   (	Do_Not_Fail == 'fail', debug_msg(1, 'No return value yet.', ''),
		!,
		diseq_eq(T1, T2), % They can not be disunified, and they are still UQ vars.
		simplify_disequation_aux(More_Diseqs, Status_In, Answer)
	    )
	).

simplify_disequation_aux([Diseq | More_Diseqs], Status_In, Answer) :- % Different vars.
	disequality_contents(Diseq, T1, T2),
        var(T1),
        var(T2), !, % Both are variables.
	debug_msg(1, 'simplify_disequation_aux :: var(T1) and var(T2)', Diseq),
	diseq_status(Status_In, UQV_In, UQV_Out, Do_Not_Fail, Result),
	(
	    (   % T1 is a UQ var, T2 is not a UQ var.
		cneg_aux:memberchk(T1, UQV_In), !,
		debug_msg(1, 'simplify_disequation_aux :: UQV(T1) and var(T2)', Diseq),
		simplify_disequation_aux_uqvar_t1_var_t2([Diseq | More_Diseqs], Status_In, Answer)
	    )
	;
	    (   % T2 is a UQ var, T1 is not a UQ var.
		cneg_aux:memberchk(T2, UQV_In), !,
		debug_msg(1, 'simplify_disequation_aux :: UQV(T2) and var(T1)', Diseq),
		disequality_contents(Diseq_Aux, T2, T1),
		simplify_disequation_aux_uqvar_t1_var_t2([Diseq_Aux | More_Diseqs], Status_In, Answer)
	    )
	;
	    (   % T1 and T2 are NOT UQ vars. 2 solutions. First: T1 =/= T2.
		debug_msg(1, 'simplify_disequation_aux :: var(T1) =/= var(T2)', Diseq),
		diseq_eq(UQV_Out, UQV_In), % Keep UQV info.
		diseq_eq('true', Result), % The solution is completely valid.
		diseq_eq(Answer, [Diseq])
	    )
	;
	    (   % T1 and T2 can not be disunified. We assign Cont = fail. 
		debug_msg(1, 'simplify_disequation_aux :: UNIFY var(T1) and var(T2)', Diseq),
		(   (   Do_Not_Fail == 'true', debug_msg(1, 'Not allowed to return fail.', ''), !, fail )
		;   (	Do_Not_Fail == 'fail', debug_msg(1, 'No return value yet.', ''),
			!,
			diseq_eq(T1, T2), % Since they can not be disunified, unify them.
			simplify_disequation_aux(More_Diseqs, Status_In, Answer)
		    )
		)
	    )	
	).

simplify_disequation_aux([Diseq | More_Diseqs], Status_In, Answer) :- % var and nonvar.
	disequality_contents(Diseq, T1, T2),
	(
	    (   % T1 is a VAR. T2 is not a var.
		var(T1), !,
		debug_msg(1, 'simplify_disequation_aux :: var(T1) and nonvar(T2) ', Diseq),
		simplify_disequation_aux_var_nonvar([Diseq | More_Diseqs], Status_In, Answer)
	    )
	;
	    (   % T2 is a VAR. T1 is not a var.
		var(T2), !,
		debug_msg(1, 'simplify_disequation_aux :: var(T2) and nonvar(T1) ', Diseq),
		disequality_contents(Diseq_Aux, T2, T1),
		simplify_disequation_aux_var_nonvar([Diseq_Aux | More_Diseqs], Status_In, Answer)
	    )
	).

simplify_disequation_aux([Diseq | More_Diseqs], Status_In, Answer):-  % Functors that unify.
	disequality_contents(Diseq, T1, T2),
 	functor_local(T1, Name_1, Arity_1, Args_1),
	functor_local(T2, Name_2, Arity_2, Args_2), 
	Name_1 == Name_2, Arity_1 == Arity_2, !,
	debug_msg(1, 'simplify_disequation_aux :: functor(T1) == functor(T2)', Diseq),

	disequalities_cartesian_product(Args_1, Args_2, Diseq_List),
	cneg_aux:append(Diseq_List, More_Diseqs, New_More_Diseqs),
	simplify_disequation_aux(New_More_Diseqs, Status_In, Answer).

simplify_disequation_aux([Diseq | _More_Diseqs], Status_In, Answer):-  % Functors that do not unify.
	disequality_contents(Diseq, T1, T2),
	functor_local(T1, Name1, Arity1, _Args1),
	functor_local(T2, Name2, Arity2, _Args2),
	(
	    (Name1 \== Name2) 
	; 
	    (Arity1 \== Arity2)
	), !,
	debug_msg(1, 'simplify_disequation_aux :: functor(T1) =/= functor(T2)', Diseq),
	diseq_status(Status_In, UQV_In, UQV_In, _Do_Not_Fail, 'true'), % Result is completely valid.
	diseq_eq(Answer, []). % Answer is True.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Esto NO es producto cartesiano.
disequalities_cartesian_product([], [], []) :- !.
disequalities_cartesian_product([T1], [T2], [Diseq]) :- !,
	 disequality_contents(Diseq, T1, T2).
disequalities_cartesian_product([T1 | Args_1], [T2 | Args_2], [Diseq | Args]) :- !,
	disequality_contents(Diseq, T1, T2),
	disequalities_cartesian_product(Args_1, Args_2, Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simplify_disequation_aux_uqvar_t1_var_t2([Diseq | More_Diseqs], Status_In, Answer) :-
	disequality_contents(Diseq, T1, T2),
        var(T1),
        var(T2), 
	diseq_status(Status_In, UQV_In, UQV_Out, Do_Not_Fail, Result),
	cneg_aux:memberchk(T1, UQV_In), !, % T1 is a uq var, T2 is not a uqvar.
	debug_msg(1, 'simplify_disequation_aux :: UQV(T1) and var(T2) ', Diseq),

	% T1 is not different from T2. Just return fail to disunify, unify and continue.
	(   (   Do_Not_Fail == 'true', debug_msg(1, 'Not allowed to return fail.', ''), !, fail )
	;   (	Do_Not_Fail == 'fail', debug_msg(1, 'No return value yet.', ''),
		!,
		varsbag_remove_var(T1, UQV_In, UQV_Aux),
		cneg_unify(T1, T2),
		diseq_status(Status_Out, UQV_Aux, UQV_Out, Do_Not_Fail, Result),
		simplify_disequation_aux(More_Diseqs, Status_Out, Answer)
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simplify_disequation_aux_var_nonvar([Diseq | More_Diseqs], Status_In, Answer):- 
	disequality_contents(Diseq, T1, T2),
        var(T1),
        functor_local(T2, Name, Arity, _Args_T2), 
	diseq_status(Status_In, UQV_In, UQV_Out, Do_Not_Fail, Result),	
	(
	    (   % A variable is always different from a functor making use of it.
		cneg_aux:varsbag(T2, [], [], Vars_T2),
		cneg_aux:memberchk(T1, Vars_T2), !, % e.g. X =/= s(s(X)).
		debug_msg(1, 'simplify_disequation_aux :: var(T1) and functor(T2) and T1 in vars(T2)', Diseq),
		cneg_unify(UQV_In, UQV_Out),
		cneg_unify('true', Result), % Result is completely valid.
		diseq_eq(Answer, []) % Answer is True.
	    )
	;
	    (   % T1 is a UQ var. Impossible to disunify.
		cneg_aux:memberchk(T1, UQV_In), !,
		debug_msg(1, 'simplify_disequation_aux :: UQV(T1) and functor(T2)', Diseq),
		(   (   Do_Not_Fail == 'true', debug_msg(1, 'Not allowed to return fail.', ''), !, fail )
		;   (	Do_Not_Fail == 'fail', debug_msg(1, 'No return value yet.', ''),
			!,
			varsbag_remove_var(T1, UQV_In, UQV_Aux),
			cneg_unify(T1, T2),
			diseq_status(Status_Out, UQV_Aux, UQV_Out, Do_Not_Fail, Result),
			simplify_disequation_aux(More_Diseqs, Status_Out, Answer)
		    )
		)
	    )
	;
	    (   % The variable must not be the functor (use attributed variables).
		debug_msg(1, 'simplify_disequation_aux :: var(T1) =/= functor(T2)', Diseq),
		functor_local(New_T2, Name, Arity, New_Args_T2), 
		cneg_aux:varsbag(New_Args_T2, [], UQV_In, UQV_Out),
		cneg_unify(Result, 'true'), % Correct result if attr. var. satisfied.
		disequality_contents(New_Diseq, T1, New_T2),
		diseq_eq(Answer, [New_Diseq]) % Answer is Diseq.
	    )
	;
	    (   % Keep the functor but diseq between the arguments.
		% We need to say that we have failed because if we are playing with attributed
		% variables we have no way to get more information on the disequality.
		debug_msg(1, 'simplify_disequation_aux :: UNIFY var(T1) and functor(T2)', Diseq),
		
		(   (   Do_Not_Fail == 'true', debug_msg(1, 'Not allowed to return fail.', ''), !, fail )
		;   (	Do_Not_Fail == 'fail', debug_msg(1, 'No return value yet.', ''),
			!,
			functor_local(T1, Name, Arity, _Args_T1), % T1 = functor 
			simplify_disequation_aux([Diseq | More_Diseqs], Status_In, Answer)
		    )
		)
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

diseq_status([UQV_In |[UQV_Out |[Do_Not_Fail |[Result]]]], UQV_In, UQV_Out, Do_Not_Fail, Result).

and_between_statuses('true', 'true', 'true').
and_between_statuses('fail', 'true', 'fail').
and_between_statuses('true', 'fail', 'fail').
and_between_statuses('fail', 'fail', 'fail').

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

disequality(T1,T2, UQV_In):- 
	cneg_diseq(T1, T2, UQV_In, _UQV_Out, 'true', 'true').

cneg_diseq(T1,T2, UQV_In, UQV_Out, Do_Not_Fail, Result) :- 
	diseq_status(Status, UQV_In, UQV_Out, Do_Not_Fail, Result),
	debug_msg(1, 'cneg_diseq :: ((T1, =/=, T2), Status) [in]', ((T1, '=/=', T2), Status)), 

	disequality_contents(Disequality, T1, T2),
        test_and_update_vars_attributes(Status, [], [Disequality]),

	debug_msg(1, 'cneg_diseq :: ((T1, =/=, T2), Status) [out]', ((T1, '=/=', T2), Status)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

equality(T1, T2, UQV_In) :-
	cneg_eq(T1, T2, UQV_In, _UQV_Out, 'true', 'true').

cneg_eq(T1, T2, UQV_In, UQV_Out, Do_Not_Fail, Result) :- 
	debug_msg(1, 'cneg_eq :: (T1, =, T2)', (T1, '=', T2)),
	debug_msg(1, 'cneg_eq :: (UQV_In, Do_Not_Fail)', (UQV_In, Do_Not_Fail)),
	cneg_aux:varsbag((T1, T2), [], [], Vars_Equality),
	varsbag_intersection(Vars_Equality, UQV_In, Intersection),
	!,
	(
	    ( 
		Intersection == [], !, diseq_eq(T1, T2),
		Result = 'true',
		UQV_Out = UQV_In
	    )
	;
	    (
		Intersection \== [], !, 
		(   (   Do_Not_Fail == 'true', debug_msg(1, 'Not allowed to return fail.', ''), !, fail )
		;   (	Do_Not_Fail == 'fail', debug_msg(1, 'Return value is fail.', ''),
			Result = 'fail',
			% cneg_diseq(T1,T2, UQV_In, UQV_Out, Do_Not_Fail, Result)
			cneg_diseq(T1, T2, UQV_In, UQV_Out, 'true', 'true')
		    )
		)
	    )
	),
	debug_msg(1, 'cneg_eq :: (UQV_Out, Result)', (UQV_Out, Result)).
%	debug_msg_nl(1).

cneg_unify(T, T).

% diseq_eq(X,Y) unify X and Y
diseq_eq(X, X).
% eq(X,Y):-
 %       X=Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

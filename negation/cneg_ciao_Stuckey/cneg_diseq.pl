:- module(cneg_diseq, 
	[
	    diseq_uqv/3, eq_uqv/3, 
	    diseq_eqv/3, eq_eqv/3, 
	    cneg_diseq_uqv/5, cneg_eq_uqv/5,
	    cneg_diseq_eqv/5, cneg_eq_eqv/5,
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
attribute_contents(var_attribute(Target, Disequalities, UQV), Target, Disequalities, UQV).
disequality_contents(disequality(Diseq_1, Diseq_2, GoalVars), Diseq_1, Diseq_2, GoalVars).
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
	attribute_contents(Attribute, _Target, Disequalities, UQV), !,
	portray_disequalities(Disequalities,UQV).

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

portray_disequalities(Disequalities, UQV) :-
	portray_disequalities_aux_1(Disequalities),
	portray_disequalities_aux_3(UQV).

portray_disequalities_aux_1([]) :- !.
portray_disequalities_aux_1([Diseq_1]) :- !,
	portray_disequalities_aux_2(Diseq_1).
portray_disequalities_aux_1([Diseq_1|Diseqs]) :- !,
	portray_disequalities_aux_2(Diseq_1), 
	debug_msg_aux(2, ' AND ', ''),
	portray_disequalities_aux_1(Diseqs).

portray_disequalities_aux_2(Diseq) :-
	disequality_contents(Diseq, Diseq_1, Diseq_2, _GoalVars),
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

verify_attribute(Attribute, Target) :-
	debug_msg(1, 'verify_attribute :: (Attribute, Target)', (Attribute, Target)), 
	verify_attribute_aux(Attribute, Target).

verify_attribute_aux(Attribute, NewTarget) :-
	attribute_contents(Attribute, OldTarget, Disequalities, _UQV), 
	terms_are_equal(NewTarget, OldTarget), !, % If not, a substitution is needed.
	remove_attribute_local(OldTarget), 
	test_and_update_vars_attributes(Disequalities, 'fail', 'true').

% Only for Ciao prolog 
verify_attribute_aux(Attribute, NewTarget) :-
	attribute_contents(Attribute, OldTarget, Disequalities, _UQV), 
	NewTarget \== OldTarget, % A substitution is needed.
	remove_attribute_local(OldTarget), 
	get_goalvars_from_disequalities(Disequalities, [], GoalVars),
	perform_substitutions([(OldTarget, NewTarget)], GoalVars),
	test_and_update_vars_attributes(Disequalities, 'fail', 'true').

combine_attributes(Attribute_Var_1, Attribute_Var_2) :-
	debug_msg(0, 'combine_attributes :: Attr_Var1 :: (Attr, Target, Diseqs, UQV)', Attribute_Var_1),
	debug_msg(0, 'combine_attributes :: Attr_Var2 :: (Attr, Target, Diseqs, UQV)', Attribute_Var_2),
	attribute_contents(Attribute_Var_1, OldTarget_Var_1, Disequalities_Var_1, _UQV_Var_1), !,
	attribute_contents(Attribute_Var_2, OldTarget_Var_2, Disequalities_Var_2, _UQV_Var_2), !,
	remove_attribute_local(OldTarget_Var_1), 
	remove_attribute_local(OldTarget_Var_2), 

	cneg_aux:append(Disequalities_Var_1, Disequalities_Var_2, Disequalities),
	get_goalvars_from_disequalities(Disequalities, [], GoalVars),
	perform_substitutions([(OldTarget_Var_1, OldTarget_Var_2)], GoalVars),
	
	debug_msg(1, 'test_and_update_vars_attributes :: (Disequalities)', (Disequalities)), 
	test_and_update_vars_attributes(Disequalities, 'fail', 'true').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

perform_substitutions([], _GoalVars) :- !.
perform_substitutions([(OldTarget, NewTarget) | MoreSubst], GoalVars_In) :-
	varsbag(GoalVars_In, [], [], GoalVars_Aux), !,
	perform_substitution(OldTarget, NewTarget, GoalVars_Aux),
	perform_substitutions(MoreSubst, GoalVars_Aux).	

perform_substitution(OldTarget, NewTarget, GoalVars) :-
	debug_msg(1, 'perform_substitutions :: (OldTarget, NewTarget, GoalVars)', (OldTarget, NewTarget, GoalVars)),
	(
	    (
		perform_substitution_vars(OldTarget, NewTarget, GoalVars_Aux), ! % Both are vars.
	    )
	;
	    (
		perform_substitution_var_nonvar(OldTarget, NewTarget, GoalVars_Aux), ! % Only OldTarget is var.
	    )
	;
	    (
		perform_substitution_var_nonvar(NewTarget, OldTarget, GoalVars_Aux), ! % Only NewTarget is var.
	    )
	;
	    perform_substitution_nonvars(OldTarget, NewTarget, GoalVars_Aux) % None is a var.
	).

perform_substitution_vars(OldTarget, NewTarget, GoalVars) :-
	var(OldTarget),
	var(NewTarget), % Be sure both are vars.
	cneg_aux:memberchk(OldTarget, GoalVars),
	cneg_aux:memberchk(NewTarget, GoalVars), % None is universally quantified.
	diseq_eq(OldTarget, NewTarget).

perform_substitution_var_nonvar(OldTarget, NewTarget, GoalVars) :-
	var(OldTarget), % Be sure OldTarget is a var.
	cneg_aux:memberchk(OldTarget, GoalVars), % Var is not universally quantified.
	diseq_eq(OldTarget, NewTarget).

perform_substitution_nonvars(OldTarget, NewTarget, GoalVars) :-
	functor_local(OldTarget, Name, Arity, Args_1),
	functor_local(NewTarget, Name, Arity, Args_2), !,
	substitutions_cartesian_product(Args_1, Args_2, Subst_List),
	perform_substitutions(Subst_List, GoalVars).

% Esto NO es producto cartesiano.
substitutions_cartesian_product([], [], []) :- !.
substitutions_cartesian_product([T1], [T2], [(T1, T2)]) :- !.
substitutions_cartesian_product([T1 | Args_1], [T2 | Args_2], [(T1, T2) | Args]) :- !,
	substitutions_cartesian_product(Args_1, Args_2, Args).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_goalvars_from_disequalities([], GoalVars_In, GoalVars_In) :- !.
get_goalvars_from_disequalities([(_Diseq, GoalVars_Diseq) | Disequalities], GoalVars_In, GoalVars_Out) :-
	varsbag(GoalVars_Diseq, [], GoalVars_In, GoalVars_Aux),
	get_goalvars_from_disequalities(Disequalities, GoalVars_Aux, GoalVars_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% An~ade la formula al atributo de las variables implicadas
% Por q tendriamos q tener en cuenta otros atributos?
% Como cada uno tiene su manejador, tratar de mezclar los atributos no aporta nada.

test_and_update_vars_attributes(New_Disequalities, Can_Fail, Result) :-
	debug_msg(0, 'test_and_update_vars_attributes :: New_Disequalities', New_Disequalities),  

	cneg_aux:varsbag(New_Disequalities, [], [], Diseq_Vars), !,
	retrieve_affected_disequalities(Diseq_Vars, [], [], Old_Disequalities), !,
	debug_msg(0, 'test_and_update_vars_attributes :: Old_Disequalities', Old_Disequalities),

	% Get which variables are GoalVars so we distinguish them from UQV.
	get_goalvars_from_disequalities(New_Disequalities, [], All_GoalVars_Tmp),
	get_goalvars_from_disequalities(Old_Disequalities, All_GoalVars_Tmp, All_GoalVars),

	% At first we check that the new disequalities can be added to the old ones.
	simplify_disequations(New_Disequalities, [], Simplified_Disequalities_1, All_GoalVars, Can_Fail, Result),
	% At last we check that the old disequalities are still valid.
	simplify_disequations(Old_Disequalities, [], Simplified_Disequalities_2, All_GoalVars, 'fail', 'true'),

	% Now we aggregate all of them.
	accumulate_disequations(Simplified_Disequalities_1, Simplified_Disequalities_2, Simplified_Disequalities),

	debug_msg(0, 'test_and_update_vars_attributes :: Simplified_Disequalities', Simplified_Disequalities),
	restore_attributes(Simplified_Disequalities).

retrieve_affected_disequalities([], _Visited_Vars, Diseq_Acc_Out, Diseq_Acc_Out) :- !. % Loop over vars list.
retrieve_affected_disequalities([Var|Vars], Visited_Vars, Diseq_Acc_In, Diseq_Acc_Out):- 
	var(Var), % It cannot be other things ...
	get_attribute_local(Var, Attribute), !,
	attribute_contents(Attribute, Var, Disequalities, _UQV), 
	remove_attribute_local(Var), 

	cneg_aux:varsbag(Disequalities, [Var|Visited_Vars], Vars, New_Vars),
	accumulate_disequations(Disequalities, Diseq_Acc_In, Diseq_Acc_Aux),
        retrieve_affected_disequalities(New_Vars, [Var|Visited_Vars], Diseq_Acc_Aux, Diseq_Acc_Out).

retrieve_affected_disequalities([Var|Vars_In], Visited_Vars, Diseq_Acc_In, Diseq_Acc_Out) :- 
        retrieve_affected_disequalities(Vars_In, [Var|Visited_Vars], Diseq_Acc_In, Diseq_Acc_Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

restore_attributes(Diseqs) :- 
	clean_up_goalvars_in_disequalities(Diseqs, Cleaned_Diseqs),
	varsbag(Cleaned_Diseqs, [], [], Affected_Vars),
	debug_msg(1, 'restore_attributes_vars(Affected_Vars, Diseqs)', (Affected_Vars, Cleaned_Diseqs)),
	restore_attributes_vars(Affected_Vars, Cleaned_Diseqs).

restore_attributes_vars([], _Diseqs) :- !.
restore_attributes_vars([Var | Affected_Vars], Diseqs) :-
	affected_diseqs(Var, Diseqs, Affected_Diseqs),
	restore_attributes_var(Var, Affected_Diseqs),
	restore_attributes_vars(Affected_Vars, Diseqs).

affected_diseqs(_Var, [], []) :- !.
affected_diseqs(Var, [Diseq | Diseqs], [Diseq | Affected_Diseqs]) :-
	cneg_aux:varsbag(Diseq, [], [], Diseq_Vars),
	cneg_aux:memberchk(Var, Diseq_Vars), !,
	affected_diseqs(Var, Diseqs, Affected_Diseqs).
affected_diseqs(Var, [_Diseq | Diseqs], Affected_Diseqs) :-
	affected_diseqs(Var, Diseqs, Affected_Diseqs).

restore_attributes_var(Var, _Diseqs) :-
	var(Var),
	get_attribute_local(Var, Attribute), !,
	debug_msg_nl(0),
	debug_msg(0, 'ERROR: var has an attribute. Attribute: ', Attribute),
	debug_msg_nl(0),
	fail.

restore_attributes_var(Var, Diseqs) :-
	var(Var),
	Diseqs == [],
	!, % We do not want empty attributes.
	fail.

restore_attributes_var(Var, Diseqs) :-
	Diseqs \== [],
	var(Var),

	get_goalvars_from_disequalities(Diseqs, [], GoalVars),
	cneg_aux:varsbag(Diseqs, GoalVars, [], UQV_Diseqs),
	attribute_contents(Attribute, Var, Diseqs, UQV_Diseqs),
	put_attribute_local(Var, Attribute).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clean_up_goalvars_in_disequalities([], []) :- !.
clean_up_goalvars_in_disequalities([Diseq_In | Diseqs], [Diseq_Out | Cleaned_Diseqs]) :- 
	clean_up_goalvars_in_disequality(Diseq_In, Diseq_Out),
	clean_up_goalvars_in_disequalities(Diseqs, Cleaned_Diseqs).

clean_up_goalvars_in_disequality(Diseq_In, Diseq_Out) :-
	disequality_contents(Diseq_In, T1, T2, GoalVars_In),
	varsbag((T1, T2), GoalVars_In, [], UQV),
	varsbag((T1, T2), UQV, [], GoalVars_Out),
	disequality_contents(Diseq_Out, T1, T2, GoalVars_Out).

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
	disequality_contents(Diseq, T1, T2, GoalVars),
	disequality_contents(Diseq_Aux, T2, T1, GoalVars), % Order inversion.
	cneg_aux:memberchk(Diseq_Aux, Diseq_Acc_In), !, % It is there.
	accumulate_disequations_aux(Diseq_List, Diseq_Acc_In, Diseq_Acc_Out).
accumulate_disequations_aux([Diseq | Diseq_List], Diseq_Acc_In, Diseq_Acc_Out) :-
	accumulate_disequations_aux(Diseq_List, [Diseq | Diseq_Acc_In], Diseq_Acc_Out).

% Note that each disequality analized gets a clean status on its Result variable.
% This is because all of them need to be satisfied, we should not override the status of
% a previous disequality with the status of the current one.
simplify_disequations([], Diseq_Acc_In, Diseq_Acc_In, _GoalVars, _Can_Fail, 'true') :- !.

simplify_disequations([Diseq|Diseq_List], Diseq_Acc_In, Diseq_Acc_Out, GoalVars, Can_Fail, Result_In) :- !,
	simplify_disequation([Diseq], Simplified_Diseq, GoalVars, Can_Fail, Result_Aux),
	and_between_statuses(Result_Aux, Result_Out, Result_In),
	accumulate_disequations(Simplified_Diseq, Diseq_Acc_In, Diseq_Acc_Aux),
	simplify_disequations(Diseq_List, Diseq_Acc_Aux, Diseq_Acc_Out, GoalVars, Can_Fail, Result_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% just for debug.
simplify_disequation(Diseqs, Answer, GoalVars, Can_Fail, Result) :-
	debug_msg_nl(0),
	debug_msg(1, 'simplify_disequation :: (Diseqs, Can_Fail)', (Diseqs, Can_Fail)),
	simplify_disequation_aux(Diseqs, Answer, GoalVars, Can_Fail, Result),
	debug_msg(1, 'simplify_disequation :: (Result, Answer)', (Result, Answer)),
	debug_msg_nl(0).

% For the case we do not have a disequality to simplify.
% The answer is obviously empty, but we might fail because of Can_Fail = fail.
simplify_disequation_aux([], [], _GoalVars, Can_Fail, Result) :- 
	!,
	debug_msg(1, 'simplify_disequation_aux :: Diseqs = []', ''),
	check_if_allowed_to_fail(Can_Fail),
	Result = 'fail'. % We have failed.

simplify_disequation_aux([Diseq | More_Diseqs], Answer, GoalVars, Can_Fail, Result) :- % Same var.
	disequality_contents(Diseq, T1, T2, _Diseq_GoalVars),
        var(T1),
        var(T2), % Both are variables.
        T1==T2, !, % Both are the same variable.
	debug_msg(1, 'simplify_disequation_aux :: SAME VAR, T1 == T2', Diseq),
	check_if_allowed_to_fail(Can_Fail),
	simplify_disequation_aux(More_Diseqs, Answer, GoalVars, Can_Fail, Result).

simplify_disequation_aux([Diseq | More_Diseqs], Answer, GoalVars, Can_Fail, Result) :- % Different vars.
	disequality_contents(Diseq, T1, T2, _Diseq_GoalVars),
        var(T1),
        var(T2), !, % Both are variables, but not the same one.
	T1 \== T2, % Not the same variable.
	varsbag((T1, T2), GoalVars, [], UQV), % Compute UQ vars.
	debug_msg(1, 'simplify_disequation_aux :: var(T1) and var(T2)', Diseq),
	(
	    (   % Both are UQ vars.
		cneg_aux:memberchk(T1, UQV), 
		cneg_aux:memberchk(T2, UQV), 
		debug_msg(1, 'simplify_disequation_aux :: UNIFY UQV(T1) and UQV(T2)', Diseq),
		check_if_allowed_to_fail(Can_Fail),
		diseq_eq(T1, T2), % They can not be disunified, and they are still UQ vars.
		simplify_disequation_aux(More_Diseqs, Answer, GoalVars, Can_Fail, Result)
	    )
	;
	    (   % T1 is a UQ var, T2 is not a UQ var.
		cneg_aux:memberchk(T1, UQV), 
		cneg_aux:memberchk(T2, GoalVars), 
		debug_msg(1, 'simplify_disequation_aux :: UQV(T1) and var(T2)', Diseq),
		simplify_disequation_aux_uqvar_t1_var_t2([Diseq | More_Diseqs], Answer, GoalVars, Can_Fail, Result)
	    )
	;
	    (   % T2 is a UQ var, T1 is not a UQ var.
		cneg_aux:memberchk(T1, GoalVars),
		cneg_aux:memberchk(T2, UQV), 
		debug_msg(1, 'simplify_disequation_aux :: UQV(T2) and var(T1)', Diseq),
		disequality_contents(Diseq_Aux, T2, T1, GoalVars),
		simplify_disequation_aux_uqvar_t1_var_t2([Diseq_Aux | More_Diseqs], Answer, GoalVars, Can_Fail, Result)
	    )
	;
	    (   % T1 and T2 are NOT UQ vars. 2 solutions. 
		cneg_aux:memberchk(T1, GoalVars),
		cneg_aux:memberchk(T2, GoalVars),
		( 
		    (   % First solution: T1 =/= T2.
			debug_msg(1, 'simplify_disequation_aux :: var(T1) =/= var(T2)', Diseq),
			diseq_eq('true', Result), % The solution is completely valid.
			diseq_eq(Answer, [Diseq])
		    )
		;
		    (   % T1 and T2 can not be disunified. We test if we can fail.
			debug_msg(1, 'simplify_disequation_aux :: UNIFY var(T1) and var(T2)', Diseq),
			check_if_allowed_to_fail(Can_Fail),
			diseq_eq(T1, T2), % Since they can not be disunified, unify them.
			simplify_disequation_aux(More_Diseqs, Answer, GoalVars, Can_Fail, Result)
		    )
		)
	    )	
	).

simplify_disequation_aux([Diseq | More_Diseqs], Answer, GoalVars, Can_Fail, Result) :- % var and nonvar.
	disequality_contents(Diseq, T1, T2, _Diseq_GoalVars),
	(
	    (   % T1 is a VAR. T2 is not a var.
		var(T1), 
		nonvar(T2),
		debug_msg(1, 'simplify_disequation_aux :: var(T1) and nonvar(T2) ', Diseq),
		simplify_disequation_aux_var_nonvar([Diseq | More_Diseqs], Answer, GoalVars, Can_Fail, Result)
	    )
	;
	    (   % T2 is a VAR. T1 is not a var.
		var(T2), 
		nonvar(T1),
		debug_msg(1, 'simplify_disequation_aux :: var(T2) and nonvar(T1) ', Diseq),
		disequality_contents(Diseq_Aux, T2, T1, GoalVars),
		simplify_disequation_aux_var_nonvar([Diseq_Aux | More_Diseqs], Answer, GoalVars, Can_Fail, Result)
	    )
	).

simplify_disequation_aux([Diseq | More_Diseqs], Answer, GoalVars, Can_Fail, Result):-  % Functors that unify.
	disequality_contents(Diseq, T1, T2, _Diseq_GoalVars),
 	functor_local(T1, Name_1, Arity_1, Args_1),
	functor_local(T2, Name_2, Arity_2, Args_2), 
	Name_1 == Name_2, Arity_1 == Arity_2, !,
	debug_msg(1, 'simplify_disequation_aux :: functor(T1) == functor(T2)', Diseq),

	disequalities_cartesian_product(Args_1, Args_2, GoalVars, Diseq_List),
	cneg_aux:append(Diseq_List, More_Diseqs, New_More_Diseqs),
	simplify_disequation_aux(New_More_Diseqs, Answer, GoalVars, Can_Fail, Result).

simplify_disequation_aux([Diseq | _More_Diseqs], Answer, _GoalVars, _Can_Fail, Result):-  % Functors that do not unify.
	disequality_contents(Diseq, T1, T2, _Diseq_GoalVars),
	functor_local(T1, Name1, Arity1, _Args1),
	functor_local(T2, Name2, Arity2, _Args2),
	(
	    (Name1 \== Name2) 
	; 
	    (Arity1 \== Arity2)
	), !,
	debug_msg(1, 'simplify_disequation_aux :: functor(T1) =/= functor(T2)', Diseq),
	%	check_if_allowed_to_fail(Can_Fail),
	Result = 'true', % Result is completely valid.
	diseq_eq(Answer, []). % Answer is True.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Esto NO es producto cartesiano.
disequalities_cartesian_product([], [], _GoalVars, []) :- !.
disequalities_cartesian_product([T1], [T2], GoalVars, [Diseq]) :- !,
	 disequality_contents(Diseq, T1, T2, GoalVars).
disequalities_cartesian_product([T1 | Args_1], [T2 | Args_2], GoalVars, [Diseq | More_Diseqs]) :- !,
	disequality_contents(Diseq, T1, T2, GoalVars),
	disequalities_cartesian_product(Args_1, Args_2, GoalVars, More_Diseqs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simplify_disequation_aux_uqvar_t1_var_t2([Diseq | More_Diseqs], Answer, GoalVars, Can_Fail, Result) :-
	disequality_contents(Diseq, T1, T2, _Diseq_GoalVars),
        var(T1),
        var(T2), 
	varsbag((T1, T2), GoalVars, [], UQV), % Compute UQ vars.
	cneg_aux:memberchk(T1, UQV), % T1 is a uq var, T2 is not a uqvar.
	cneg_aux:memberchk(T2, GoalVars),
	debug_msg(1, 'simplify_disequation_aux :: UQV(T1) and var(T2) ', Diseq),

	% T1 can not be different from T2. We unify them (failing) and continue.
	check_if_allowed_to_fail(Can_Fail),
	cneg_unify(T1, T2),
	simplify_disequation_aux(More_Diseqs, Answer, GoalVars, Can_Fail, Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simplify_disequation_aux_var_nonvar([Diseq | More_Diseqs], Answer, GoalVars, Can_Fail, Result):- 
	disequality_contents(Diseq, T1, T2, _Diseq_GoalVars),
        var(T1),
	nonvar(T2),
        functor_local(T2, Name, Arity, _Args_T2), 
	varsbag((T1, T2), GoalVars, [], UQV), % Compute UQ vars.
	(
	    (   % A variable is always different from a functor making use of it.
		cneg_aux:varsbag(T2, [], [], Vars_T2),
		cneg_aux:memberchk(T1, Vars_T2), !, % e.g. X =/= s(s(X)).
		debug_msg(1, 'simplify_disequation_aux :: var(T1) and functor(T2) and T1 in vars(T2)', Diseq),
		cneg_unify('true', Result), % Result is completely valid.
		diseq_eq(Answer, []) % Answer is True.
	    )
	;
	    (   % T1 is a UQ var. Impossible to disunify.
		cneg_aux:memberchk(T1, UQV), !,
		debug_msg(1, 'simplify_disequation_aux :: UQV(T1) and functor(T2)', Diseq),
		check_if_allowed_to_fail(Can_Fail),
		cneg_unify(T1, T2),
		simplify_disequation_aux(More_Diseqs, Answer, GoalVars, Can_Fail, Result)
	    )
	;
	    (   % The variable must not be the functor (use attributed variables).
		cneg_aux:memberchk(T1, GoalVars),
		debug_msg(1, 'simplify_disequation_aux :: var(T1) =/= functor(T2)', Diseq),
		(
		    (
			functor_local(New_T2, Name, Arity, _UQ_Vars_New_T2), 
			cneg_unify(Result, 'true'), % Correct result if attr. var. satisfied.
			disequality_contents(New_Diseq, T1, New_T2, GoalVars),
			diseq_eq(Answer, [New_Diseq]) % Answer is Diseq.
		    )
		;
		    (   % Keep the functor but diseq between the arguments.
			% We need to say that we have failed because if we are playing with attributed
			% variables we have no way to get more information on the disequality.
			debug_msg(1, 'simplify_disequation_aux :: UNIFY var(T1) and functor(T2)', Diseq),
			check_if_allowed_to_fail(Can_Fail),
			functor_local(T1, Name, Arity, _Args_T1), % T1 = functor 
			simplify_disequation_aux([Diseq | More_Diseqs], Answer, GoalVars, Can_Fail, Result)
		    )
		)
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_vars_with_attributes([], []) :- !.
remove_vars_with_attributes([Var|List_In], List_Out) :-   % If variables have attributes, remove them from the bag.
	get_attribute_local(Var, _Attribute), !,
	remove_vars_with_attributes(List_In, List_Out).
remove_vars_with_attributes([Var|List_In], [Var|List_Out]) :- % Keep only vars without attributes.
	remove_vars_with_attributes(List_In, List_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

diseq_status([GoalVars |[Can_Fail |[Result]]], GoalVars, Can_Fail, Result).

and_between_statuses('true', 'true', 'true').
and_between_statuses('fail', 'true', 'fail').
and_between_statuses('true', 'fail', 'fail').
and_between_statuses('fail', 'fail', 'fail').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_if_allowed_to_fail(Can_Fail) :-
	(   
	    ( 
		Can_Fail == 'fail', 
		debug_msg(1, 'Not allowed to return fail.', ''), 
		fail 
	    )
	;   
	    (	Can_Fail == 'true', 
		debug_msg(1, 'No return value yet.', '')
	    )
	).

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

diseq_uqv(T1,T2, UQV) :- 
	varsbag((T1, T2), UQV, [], GoalVars), % Determine GoalVars.
	cneg_diseq_eqv(T1, T2, GoalVars, 'true', 'true').

diseq_eqv(T1,T2, GoalVars) :- 
	cneg_diseq_eqv(T1, T2, GoalVars, 'true', 'true').

cneg_diseq_uqv(T1,T2, UQV, Can_Fail, Result) :- 
	varsbag((T1, T2), UQV, [], GoalVars), % Determine GoalVars.
	cneg_diseq_eqv(T1,T2, GoalVars, Can_Fail, Result).

cneg_diseq_eqv(T1,T2, GoalVars_In, Can_Fail, Result) :- 
	varsbag(GoalVars_In, [], [], GoalVars_Aux), % Only variables, please.
	varsbag((T1, T2), GoalVars_Aux, [], UQV), % Universally Quantified Variables.
	varsbag((T1, T2), UQV, [], Affected_GoalVars), % Affected GoalVars Only.

	debug_msg(1, 'cneg_diseq [in] :: ((T1, =/=, T2), (Affected_GoalVars, Can_Fail) [out]', ((T1, '=/=', T2), (Affected_GoalVars, Can_Fail))),
	disequality_contents(Disequality, T1, T2, Affected_GoalVars),
        test_and_update_vars_attributes([Disequality], Can_Fail, Result),

	debug_msg(1, 'cneg_diseq [out] :: ((T1, =/=, T2), Result) [out]', ((T1, '=/=', T2), Result)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eq_uqv(T1, T2, UQV) :-
	varsbag((T1, T2), UQV, [], GoalVars), % Determine GoalVars.
	cneg_eq_eqv(T1, T2, GoalVars, 'true', 'true').

eq_eqv(T1, T2, GoalVars) :-
	cneg_eq_eqv(T1, T2, GoalVars, 'true', 'true').

cneg_eq_uqv(T1, T2, UQV, Can_Fail, Result) :- 
	varsbag((T1, T2), UQV, [], GoalVars), % Determine GoalVars.
	cneg_eq_eqv(T1, T2, GoalVars, Can_Fail, Result).

cneg_eq_eqv(T1, T2, GoalVars_In, Can_Fail, Result) :- 
	debug_msg(1, 'cneg_eq :: (T1, =, T2), ---, (GoalVars_In, Can_Fail)', ((T1, '=', T2), '---', (GoalVars_In, Can_Fail))),
	cneg_aux:varsbag(GoalVars_In, [], [], GoalVars_Aux),
	cneg_aux:varsbag((T1, T2), GoalVars_Aux, [], UQV_Equality),
	!,
	(
	    ( 
		UQV_Equality == [], !, 
		diseq_eq(T1, T2),
		Result = 'true'
	    )
	;
	    (
		UQV_Equality \== [], !, 
		check_if_allowed_to_fail(Can_Fail),
		Result = 'fail',
		cneg_aux:varsbag((T1, T2), [], [], Disequality_GoalVars),
		% cneg_diseq(T1,T2, GoalVars, Can_Fail, Result)
		cneg_diseq_eqv(T1, T2, Disequality_GoalVars, 'true', 'true')
	    )
	).

cneg_unify(T, T).

% diseq_eq(X,Y) unify X and Y
diseq_eq(X, X).
% eq(X,Y):-
 %       X=Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(cneg_diseq, [cneg_diseq/3,
			portray_attributes_in_term/1, 
			put_universal_quantification/1,
			remove_universal_quantification/1]).

:- use_module(cneg_aux,_).

% For Ciao Prolog:
:- multifile 
        verify_attribute/2,
        combine_attributes/2,
	portray_attribute/2,
	portray/1.

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


% Local predicates used to easy migration between 
% prologs. 
remove_attribute_local(Var) :- 
%	debug('remove_attribute_local :: Var', Var),
	detach_attribute(Var).
% XSB:	del_attr(Var, dist).
get_attribute_local(Var, Attribute) :-
	get_attribute(Var, Attribute).
% XSB:	get_attr(Var, dist, Attribute),
%	debug('get_attribute_local :: (Var, Attribute)', (Var, Attribute)).
put_attribute_local(Var, Attribute) :-
%	debug('put_attribute_local :: (Var, Attribute)', (Var, Attribute)),
%	get_attribute_if_any(Var), !,
	attach_attribute(Var, Attribute).
%	put_attr(Var, dist, Attribute).

%get_attribute_if_any(Var) :-
%	debug('Testing if var has any attribute. Var: '),
%	debug(Var),
%	get_attribute_local(Var, _Attribute), !.
%get_attribute_if_any(Var) :-
%	debug('Testing if var has any attribute. Var: '),
%	debug(Var),
%	debug(' has NO attribute').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

put_universal_quantification(_X).
remove_universal_quantification(_X).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Attributes contents are encapsulated via the following structure.

%:- dynamic var_attribute/2.
attribute_contents(var_attribute(Target, Is_UnivVar, Disequalities), Target, Is_UnivVar, Disequalities).
disequality_contents(disequality(Diseq_1, Diseq_2, FreeVars), Diseq_1, Diseq_2, FreeVars).
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
	msg('portray_attribute :: (Attr, Var)', (Attr, Var)).

portray(Attribute) :-
	attribute_contents(Attribute, _Target, _Is_UnivVar, Disequalities), !,
	portray_disequalities(Disequalities).

portray(Anything) :- 
	msg_aux('', Anything).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

portray_attributes_in_term(T) :-
	varsbag_local(T, [], [], Variables),
	debug('Attributes for the variables in term', T),
	portray_attributes_in_variables(Variables).

portray_attributes_in_variables([]) :- !.
portray_attributes_in_variables([Var|Vars]) :-
	portray_attributes_in_variable(Var),
	portray_attributes_in_variables(Vars).

portray_attributes_in_variable(Var) :-
	get_attribute_local(Var, Attribute),
	msg('variable', Var), 
	portray(Attribute).
portray_attributes_in_variable(Var) :-
	msg(Var, ' has NO attribute').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

portray_disequalities(Disequalities) :-
	portray_disequalities_aux_1(Disequalities).

portray_disequalities_aux_1([]) :- !.
portray_disequalities_aux_1([Diseq_1]) :- !,
	portray_disequalities_aux_2(Diseq_1).
portray_disequalities_aux_1([Diseq_1|Diseqs]) :- !,
	portray_disequalities_aux_2(Diseq_1), 
	msg_aux(' AND ', ''),
	portray_disequalities_aux_1(Diseqs).

portray_disequalities_aux_2(Diseq) :-
	disequality_contents(Diseq, Diseq_1, Diseq_2, FreeVars),
	FreeVars == [], !,
	msg_aux('[ ', Diseq_1),
	msg_aux(' =/= ', Diseq_2),
	msg_aux('', ' ]').

portray_disequalities_aux_2(Diseq) :-
	disequality_contents(Diseq, Diseq_1, Diseq_2, FreeVars),
	FreeVars \== [], !,
	msg_aux('[ ', Diseq_1),
	msg_aux(' =/= ', Diseq_2),
	msg_aux(', Universally quantified:', ''), 
	portray_disequalities_aux_2(FreeVars),
	msg_aux('', ' ]').

portray_disequalities_aux_2([]) :- !.
portray_disequalities_aux_2([FreeVar | FreeVars]) :-
	msg_aux(' ', FreeVar),
	portray_disequalities_aux_2(FreeVars).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% CONSTRAINT VERIFICATION %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify_attribute(Attribute, Target):-
%	debug(verify_attribute(Attribute, Target)), 
	attribute_contents(Attribute, NewTarget, _Is_UnivVar, Disequalities), 
	terms_are_equal(Target, NewTarget), !,
	update_var_attributes(Disequalities, []).

% Only for Ciao prolog 
verify_attribute(Attribute, NewTarget):-
%	debug('Only for Ciao Prolog: '),
%	debug(verify_attribute(Attribute, NewTarget)), 
	attribute_contents(Attribute, OldTarget, Is_UnivVar, Disequalities), !,
	(
	    (
		Is_UnivVar == true, % An universally quantified variable
		fail                          % is not unifiable.
	    )
	;
	    (
		Is_UnivVar == false, 
		substitution_contents(Subst, OldTarget, NewTarget),
		update_var_attributes(Disequalities, [Subst])
	    )
	).

substitution_contents(substitute(Var, T), Var, T).

combine_attributes(Attribute_Var_1, Attribute_Var_2) :-
	debug('combine_attributes(Attr_Var1, Attr_Var2)', (Attribute_Var_1, Attribute_Var_2)),
	attribute_contents(Attribute_Var_1, OldTarget_Var_1, Is_UnivVar_Var_1, Disequalities_Var_1), !,
	attribute_contents(Attribute_Var_2, OldTarget_Var_2, Is_UnivVar_Var_2, Disequalities_Var_2), !,
	(
	    (
		(
		    Is_UnivVar_Var_1 == true ;  % Universally quantified variables can not be unified.
		    Is_UnivVar_Var_2 == true
		),
		fail
	    )
	;
	    (
		Is_UnivVar_Var_1 == false, 
		Is_UnivVar_Var_2 == false, 
		cneg_aux:append(Disequalities_Var_1, Disequalities_Var_2, Disequalities),
		substitution_contents(Subst, OldTarget_Var_1, OldTarget_Var_2),
		update_var_attributes(Disequalities, [Subst])
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% An~ade la formula al atributo de las variables implicadas
% Por q tendriamos q tener en cuenta otros atributos?
% Como cada uno tiene su manejador, tratar de mezclar los atributos no aporta nada.

update_var_attributes(New_Disequalities, Substitutions):-
	debug('update_var_attributes(New_Disequalities, Substitutions)', (New_Disequalities, Substitutions)), 
	varsbag_local(New_Disequalities, [], [], Vars), !,
	accumulator(Affected_Diseq_Acc, New_Disequalities, Disequalities_Tmp),
	retrieve_affected_disequalities(Vars, [], Affected_Diseq_Acc), !,
%	debug(retrieve_affected_disequalities(Vars, [], New_Disequalities, Disequalities_Tmp)),
	perform_substitutions(Substitutions, Disequalities_Tmp, Disequalities), !,
%	debug(perform_substitutions(Substitutions, Disequalities_Tmp, Disequalities)),

	accumulator(Diseq_Acc, [], Simplified_Disequalities),
	simplify_disequations(Disequalities, Diseq_Acc),
	debug('update_var_attributes(Affected_Diseq, Simpl_Diseq)', (Disequalities, Simplified_Disequalities)),
	restore_disequalities(Simplified_Disequalities).

retrieve_affected_disequalities([], _Vars_Examined, Diseq_Acc) :- !, % Loop over vars list.
	return_accumulator(Diseq_Acc).	
retrieve_affected_disequalities([Var|Vars], Vars_Examined, Diseq_Acc):- 
	var(Var), % It cannot be other things ...
	get_attribute_local(Var, Attribute), !,
	attribute_contents(Attribute, Var, _Is_UnivVar, ThisVar_Disequalities), 
	debug('', 'Problem here'),
	remove_attribute_local(Var), 
	varsbag_local(ThisVar_Disequalities, [Var|Vars_Examined], Vars, New_Vars), !,
	generate_intermediate_accumulator(Diseq_Acc, Tmp_Diseq_Acc, New_Diseq_Acc),
	accumulate_disequations(ThisVar_Disequalities, Tmp_Diseq_Acc),
        retrieve_affected_disequalities(New_Vars, [Var|Vars_Examined], New_Diseq_Acc).
retrieve_affected_disequalities([Var|Vars_In], Vars_Examined, Diseq_Acc):- 
        retrieve_affected_disequalities(Vars_In, [Var|Vars_Examined], Diseq_Acc).

perform_substitutions([], Disequalities_Out, Disequalities_Out) :- !.
perform_substitutions([Subst | MoreSubst], Disequalities_In, Disequalities_Out) :-
	substitution_contents(Subst, OldTarget, NewTarget),
	diseq_eq(OldTarget, NewTarget), !,
	perform_substitutions(MoreSubst, Disequalities_In, Disequalities_Out).
%	replace_in_term_var_by_value(Disequalities_In, OldTarget, NewTarget, Disequalities_Tmp), !,
%	perform_substitutions(MoreSubst, Disequalities_Tmp, Disequalities_Out).

% diseq_eq(X,Y) unify X and Y
diseq_eq(X, X).
% eq(X,Y):-
 %       X=Y.


% Need old vars to play with them too.
restore_disequalities([]) :- !.
restore_disequalities([Diseq | Diseq_List]) :-
	disequality_contents(Diseq, T_1, T_2, FreeVars),
	varsbag_local((T_1, T_2), FreeVars, [], NonFreeVars), !,
	varsbag_local((T_1, T_2), NonFreeVars, [], AffectedFreeVars),
	disequality_contents(Simplified_Diseq, T_1, T_2, AffectedFreeVars),
	!,
	restore_disequality(NonFreeVars, Simplified_Diseq),
	restore_disequalities(Diseq_List).

restore_disequality([], _Diseq) :- !.
restore_disequality([Var|Vars], Diseq) :-
	restore_disequality_var(Var, Diseq),
	restore_disequality(Vars, Diseq).

restore_disequality_var(Var, Diseq) :-
	var(Var),
	get_attribute_local(Var, Old_Attribute), !,
	remove_attribute_local(Var),
	attribute_contents(Old_Attribute, Var, _Is_UnivVar, Old_Diseq),
	attribute_contents(New_Attribute, Var, _Is_UnivVar, [Diseq|Old_Diseq]),
	put_attribute_local(Var, New_Attribute).

restore_disequality_var(Var, Diseq) :-
	var(Var),
	attribute_contents(New_Attribute, Var, _Is_UnivVar, [Diseq]),
	put_attribute_local(Var, New_Attribute).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accumulator(accumulator_structure(Acc_In, Acc_Out), Acc_In, Acc_Out).
return_accumulator(Acc) :-
	accumulator(Acc, Acc_Out, Acc_Out).
add_to_accumulator(Element, Acc, New_Acc) :-
	accumulator(Acc, Acc_In, Acc_Out),
	accumulator(New_Acc, [Element | Acc_In], Acc_Out).
generate_intermediate_accumulator(Acc, Tmp_Acc, New_Acc) :-
	accumulator(Acc, Acc_In, Acc_Out),
	accumulator(Tmp_Acc, Acc_In, Acc_Aux),
	accumulator(New_Acc, Acc_Aux, Acc_Out).

accumulate_disequations([], Diseq_Acc_In, Diseq_Acc_In) :- !.
accumulate_disequations([Diseq | Diseq_List], Diseq_Acc_In) :-
	accumulator(Diseq_Acc, Diseq_Acc_In, _Diseq_Acc_Out),
	memberchk_local(Diseq, Diseq_Acc_In), !, % It is there.
	accumulate_disequations(Diseq_List, Diseq_Acc).
accumulate_disequations([Diseq | Diseq_List], Diseq_Acc) :-
	disequality_contents(Diseq, T1, T2, FreeVars),
	disequality_contents(Diseq_Aux, T2, T1, FreeVars), % Order inversion.
	accumulator(Diseq_Acc, Diseq_Acc_In, _Diseq_Acc_Out),
	memberchk_local(Diseq_Aux, Diseq_Acc_In), !, % It is there.
	accumulate_disequations(Diseq_List, Diseq_Acc).
accumulate_disequations([Diseq | Diseq_List], Diseq_Acc) :-
	add_to_accumulator(Diseq, Diseq_Acc, New_Diseq_Acc),
	accumulate_disequations(Diseq_List, New_Diseq_Acc).

simplify_disequations([], Diseq_Acc) :- !,
	return_accumulator(Diseq_Acc).
simplify_disequations([Diseq|Diseq_List], Diseq_Acc) :- !,
	disequality_contents(Diseq, T1, T2, FreeVars),
	varsbag_local((T1, T2), FreeVars, [], No_FreeVars),
	simplify_1_diseq(Diseq, [], No_FreeVars, Simplified_Diseq),
	generate_intermediate_accumulator(Diseq_Acc, Tmp_Diseq_Acc, New_Diseq_Acc),
	accumulate_disequations(Simplified_Diseq, Tmp_Diseq_Acc),
	simplify_disequations(Diseq_List, New_Diseq_Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simplify_1_diseq(Diseq, More_Diseq, No_FreeVars, _Answer) :-
%	debug('', ''),
	debug('(Diseq, More_Diseq, No_FreeVars)', (Diseq, More_Diseq, No_FreeVars)), 
	fail.
		
simplify_1_diseq(fail, [], _No_FreeVars, _Answer) :- !, fail.
simplify_1_diseq(fail, [First_Diseq | More_Diseq], No_FreeVars, Answer) :- !,
	simplify_1_diseq(First_Diseq, More_Diseq, No_FreeVars, Answer).

simplify_1_diseq(Diseq, More_Diseq, No_FreeVars, Answer) :- % Same var.
	disequality_contents(Diseq, T1, T2, _FreeVars),
        var(T1),      
        var(T2), % Both are variables.
        T1==T2, !,
	simplify_1_diseq(fail, More_Diseq, No_FreeVars, Answer).

simplify_1_diseq(Diseq, More_Diseq, No_FreeVars, Answer) :- % Different vars.
	disequality_contents(Diseq, T1, T2, FreeVars),
        var(T1),
        var(T2), % Both are variables.
	varsbag_local(FreeVars, No_FreeVars, [], Real_FreeVars),
	memberchk_local(T1, Real_FreeVars), % Both are free vars.
	memberchk_local(T2, Real_FreeVars), !,
	simplify_1_diseq(fail, More_Diseq, No_FreeVars, Answer).

simplify_1_diseq(Diseq, More_Diseq, No_FreeVars, Answer) :- % Different vars.
	disequality_contents(Diseq, T1, T2, FreeVars),
        var(T1),
        var(T2), !, % Both are variables.
	varsbag_local(FreeVars, No_FreeVars, [], Real_FreeVars),
	(
	    (   % T1 is a free var, T2 is not a free var.
		memberchk_local(T1, Real_FreeVars), !,
		simplify_1_diseq_freevar_t1_var_t2(Diseq, More_Diseq, No_FreeVars, Answer)
	    )
	;
	    (   % T2 is a free var, T1 is not a free var.
		memberchk_local(T2, Real_FreeVars), !,
		disequality_contents(Diseq_Aux, T2, T1, Real_FreeVars),
		simplify_1_diseq_freevar_t1_var_t2(Diseq_Aux, More_Diseq, No_FreeVars, Answer)
	    )
	;
	    (   % T1 and T2 are NOT free vars. 2 solutions. First: T1 =/= T2.
		diseq_eq(Answer, [Diseq])
	    )
	;
	    (   % Answer is T1 = T2 but needs more information.
		diseq_eq(T1, T2), % Answer is T1 = T2 and more
		simplify_1_diseq(fail, More_Diseq, No_FreeVars, Answer)
	    )	
	).

simplify_1_diseq(Diseq, More_Diseq, No_FreeVars, Answer) :- % var and nonvar.
	disequality_contents(Diseq, T1, T2, FreeVars),
	(
	    (
		var(T1), !,
		simplify_1_diseq_var_nonvar(Diseq, More_Diseq, No_FreeVars, Answer)
	    )
	;
	    (
		var(T2), !,
		disequality_contents(Diseq_Aux, T2, T1, FreeVars),
		simplify_1_diseq_var_nonvar(Diseq_Aux, More_Diseq, No_FreeVars, Answer)
	    )
	).

simplify_1_diseq(Diseq, More_Diseq, No_FreeVars, Answer):-  % Functors that unify.
	disequality_contents(Diseq, T1, T2, FreeVars),
 	functor_local(T1, Name, Arity, Args_1),
	functor_local(T2, Name, Arity, Args_2), !,
	varsbag_local(FreeVars, No_FreeVars, [], Real_FreeVars), % Optimization.
	cartesian_product_between_arguments(Args_1, Args_2, Real_FreeVars, Diseq_List),
	cneg_aux:append(Diseq_List, More_Diseq, New_More_Diseq),
	simplify_1_diseq(fail, New_More_Diseq, No_FreeVars, Answer).

simplify_1_diseq(Diseq, _More_Diseq, _No_FreeVars, Answer):-  % Functors that do not unify.
	disequality_contents(Diseq, T1, T2, _FreeVars),
	functor_local(T1, Name1, Arity1, _Args1),
	functor_local(T2, Name2, Arity2, _Args2),
	(
	    (Name1 \== Name2) 
	; 
	    (Arity1 \== Arity2)
	), !,
	diseq_eq(Answer, []). % Answer is True.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simplify_1_diseq_freevar_t1_var_t2(Diseq, More_Diseq, No_FreeVars, Answer) :-
	disequality_contents(Diseq, T1, T2, FreeVars),
        var(T1),
        var(T2), 
	varsbag_local(FreeVars, No_FreeVars, [], Real_FreeVars),
	memberchk_local(T1, Real_FreeVars), !, % T1 is a free var, T2 is not a freevar.
	(
	    (   % T1 is going to be processed hereafter (appears in More_Diseq).
		varsbag_local(More_Diseq, No_FreeVars, [], More_Diseq_Vars),
		memberchk_local(T1, More_Diseq_Vars), !,
		diseq_eq(T1, T2), % Answer is T1 = T2 and more
		simplify_1_diseq(fail, More_Diseq, [T1 | No_FreeVars], Answer)
	    )
	;
	    (   % T1 appears only once, so it is not different from T2. Just fail.
		simplify_1_diseq(fail, More_Diseq, No_FreeVars, Answer)
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simplify_1_diseq_var_nonvar(Diseq, More_Diseq, No_FreeVars, Answer):- 
	disequality_contents(Diseq, T1, T2, FreeVars),
        var(T1),
        functor_local(T2, Name, Arity, _Args_T2), 
	varsbag_local(FreeVars, No_FreeVars, [], Real_FreeVars),
	(
	    (
		(
		    varsbag_local(T2, [], [], Vars_T2),
		    memberchk_local(T1, Vars_T2), !, % e.g. X =/= s(s(X)).
		    diseq_eq(Answer, []) % Answer is True.
		)
	    ;
		(   % T1 is a free var.
		    memberchk_local(T1, Real_FreeVars), !,
		    simplify_1_diseq_freevar_t1_functor_t2(Diseq, More_Diseq, No_FreeVars, Answer)
		)
	    ;
		(   % Not possible to solve it yet. 2 solutions.
		    diseq_eq(Answer, [Diseq]) % Answer is Diseq.
		)
	    ;
		(   % Keep the functor but diseq between the arguments.
		    functor_local(T1, Name, Arity, Args_T1), % Answer is T1 = functor and more.
		    cneg_aux:append(Args_T1, No_FreeVars, New_No_FreeVars), % Optimization
		    simplify_1_diseq(Diseq, More_Diseq, New_No_FreeVars, Answer)
		)
	    )
	).

simplify_1_diseq_freevar_t1_functor_t2(Diseq, More_Diseq, No_FreeVars, Answer) :-
	disequality_contents(Diseq, T1, T2, FreeVars),
        var(T1),
	varsbag_local(FreeVars, No_FreeVars, [], Real_FreeVars),
	memberchk_local(T1, Real_FreeVars), 
        functor_local(T2, _Name, _Arity, _Args_T2), !,
	(
	    (   % T1 is going to be processed (appears in More_Diseq).
		varsbag_local(More_Diseq, No_FreeVars, [], More_Diseq_FreeVars),
		memberchk_local(T1, More_Diseq_FreeVars), !,
		diseq_eq(T1, T2), % Answer is T1 = T2 and more
		simplify_1_diseq(fail, More_Diseq, [T1 | No_FreeVars], Answer)
	    )
	;
	    (   % T1 appears only once, so it is not different from T2. Just fail.
		simplify_1_diseq(fail, More_Diseq, No_FreeVars, Answer)
	    )
	).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
cartesian_product_between_arguments([], [], _FreeVars, []) :- !.
cartesian_product_between_arguments([T1], [T2], FreeVars, [Diseq]) :- !,
	 disequality_contents(Diseq, T1, T2, FreeVars).
cartesian_product_between_arguments([T1 | Args_1], [T2 | Args_2], FreeVars, [Diseq | Args]) :- !,
	disequality_contents(Diseq, T1, T2, FreeVars),
	cartesian_product_between_arguments(Args_1, Args_2, FreeVars, Args).

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

cneg_diseq(T1,T2, FreeVars):- 
	debug('cneg_diseq(T1,T2)', cneg_diseq(T1,T2)), 
	disequality_contents(Disequality, T1, T2, FreeVars),
        update_var_attributes([Disequality], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


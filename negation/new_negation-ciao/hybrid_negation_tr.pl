%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module for being loaded by intneg to add the complemented
% predicates to the predicates of the module that is being
% compiled. It uses intensional negation to obtain them.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(hybrid_negation_tr,[sentence_transformation/3]).
%:- export(sentence_transformation/3).

% :- import append/3 from basics.
:- use_module(library(lists),[append/3]).

% member/2, ground/1
% :- import unifiable/3 from constraintLib.
:- use_module(hybrid_negation_aux,[term_is_forall/1, term_is_not_forall/1,
	list_length/2, list_to_conj/2, mgu/3, clause_head/4,
	convert_clauses_format/5, revert_clauses_format/2,
	obtain_clause_cj/2, obtain_clause_body/2, 
	obtain_clause_name_and_arity/3, obtain_clause_head/2,
	obtain_clause_nocjs/2, obtain_clause_number/2,
	append_prefix_to_atom/3,
%	obtain_clause_head/2, obtain_clause_arity/2,
	joint_bodies_with_op_and/3, joint_bodies_with_op_or/3,
	debug_cls/2, debug_formatted_cls/2, debug_separation/0,
	head_and_tail/3,
	memberchk/2,
	find_equal_head_name_cls/4]).
%:- import term_is_forall/1, term_is_not_forall/1,
%	list_length/2, list_to_conj/2, mgu/3, clause_head/4,
%	convert_clauses_format/3, revert_clauses_format/2,
%	obtain_clause_cj/2, obtain_clause_body/2, 
%%	obtain_clause_head/2, obtain_clause_arity/2,
%	joint_bodies_with_op_and/3, joint_bodies_with_op_or/3,
%	debug_formatted_cls/2, debug_separation/0,
%	head_and_tail/3,
%%	memberchk/2,
%	find_equal_head_name_cls/4 from intneg_aux.

% :- import debug_intneg/2, debug_intneg_clauses/2 from intneg_io.
% write_sentence/3, write_sentences_list/3, 

% :- module(intneg_tr,_,[]).
% :- module(intneg_tr,[sentence_transformation/3],[]).

:- use_module(library(dynamic)).

% :- use_module(library(data_facts),[retract_fact/1]).
% :- use_module(library(lists),[append/3]).
% :- use_module(library(aggregates),[findall/4]).
% :- use_module(library(metaterms),[ask/2]). %Para Susana
% :- use_module(library(terms_check),[ask/2]).  %Para versiones posteriores
% :- use_module(library(idlists),[memberchk/2]).
%:- use_module(library(term_basic),[functor/3]).
%:- use_module(library(terms), [copy_args/3]).
%:- use_module(library(engine(term_basic))).

% :- use_module(library(write),[write/1,write/2]). % For debugging

% dynamic predicates to store clauses that are going to be
% expanded. It is used to expand them in a continous way
% :- data pre_intneg/1.
:- dynamic pre_intneg/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% deep_forall(D) returns the level of deep that we are going to
% consider for expanding the coverings of the forall
% deep_forall(7).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Falta añadir:
%intneg(G,fail):- ground(G), call(G), !.
%intneg(G,true):- ground(G), \+ call(G), !.


% intneg_impl(L) returns L that is the list of additional 
% clauses that complete the implementation of intneg/2
%intneg_impl([
%	(intneg((G1;G2)):- intneg(G1),intneg(G2)),
%	 (intneg((G1,G2)):- intneg(G1)),
%	  (intneg((G1,G2)):- intneg(G2)),
%	   (intneg(intneg(G)):- call(G)),
%	    end_of_file]).

% sentence_transformation(Sentence,SentList,Module) sustitutes Sentence in
% the program Module that is being compilated by the list of
% sentences SentList. The result clauses are continous
sentence_transformation(end_of_file, Final_Cls, _):- !,
	findall(CL,(retract(pre_intneg(CL))),Cls), !, 
	debug_cls('sentence_transformation: Cls: ', Cls),
	convert_clauses_format(Cls, F_Cls, Invalid_Clauses, [], LoP), % LoP = List of Predicates
	debug_formatted_cls('sentence_transformation: F_Cls: ', F_Cls),
	!, % Backtracking not allowed.
	compilative_negation(F_Cls, LoP, F_Negated_Cls),
	!, % Backtracking not allowed.
	debug_formatted_cls('sentence_transformation: F_Negated_Cls: ', F_Negated_Cls),
	revert_clauses_format(F_Negated_Cls, Negated_Cls),
	!, % Backtracking not allowed.
	
% Add auxiliar clauses.
	auxiliary_implementation(Auxiliary_Cls),
%
	append(Auxiliary_Cls, Invalid_Clauses, Final_Cls_1),
	append(Final_Cls_1, Cls, Final_Cls_2),
	append(Final_Cls_2, Negated_Cls, Final_Cls_3),
	append(Final_Cls_3, [end_of_file], Final_Cls),
%
	!, % Backtracking not allowed.
	debug_formatted_cls('sentence_transformation Final_Cls: ', Final_Cls), nl.

sentence_transformation(0, [], _) :- !. % Starting clause.
sentence_transformation(Fact_or_Cl, [], _):-  % As facts as clauses
	assertz(pre_intneg(Fact_or_Cl)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Includes in NoCj of each clause the Mgu's Cj.
apply_impositions_from_mgu(Mgu_Cj, Cl, NonOv_Cl) :-
	apply_impositions_from_mgu_aux(Mgu_Cj, Cl, NonOv_Cl), !.
apply_impositions_from_mgu(Mgu_Cj, Cl, []) :-
	debug_separation,
	nl, write('% DBG % ERROR % apply_impositions_from_mgu fails. '), 
	debug_formatted_cls('Mgu_Cj: ', Mgu_Cj),
	debug_formatted_cls('Cl: ', Cl).

apply_impositions_from_mgu_aux(Mgu_Cj, Cl, []) :-
	obtain_clause_cj(Cl, Cl_Cj),
	mgu_imposes_nothing(Mgu_Cj, Cl_Cj), !.

apply_impositions_from_mgu_aux(Mgu_Cj, Cl, [NonOv_Cl]) :-
	copy_term(Mgu_Cj, Mgu_Cj_Copy),
	include_in_NoCj(Mgu_Cj_Copy, Cl, NonOv_Cl).


% mgu_imposes_something(Mgu_Cj, Cl_Cj) :-
mgu_imposes_nothing(Mgu_Cj, Cl_Cj) :-
	write('% DBG % mgu_imposes_nothing: (Mgu_Cj, Cl_Cj): '), 
	write((Mgu_Cj, Cl_Cj)), 
	mgu_imposes_nothing_list(Mgu_Cj, Cl_Cj, [], _VE),
	write(' ---> YES '), nl,
	!.
mgu_imposes_nothing(_Mgu_Cj, _Cl_Cj) :-
	write(' ---> NO '), nl,
	!, fail. % End

mgu_imposes_nothing_list([], [], VE, VE) :- !.
mgu_imposes_nothing_list([T1_1 | T1_Others], [T2_1 | T2_Others], VE_In, VE_Out) :-
	mgu_imposes_nothing_1_term(T1_1, T2_1, VE_In, VE_Aux), !,
	mgu_imposes_nothing_list(T1_Others, T2_Others, VE_Aux, VE_Out).

mgu_imposes_nothing_1_term(T1, T2, VE_In, VE_Out) :- 
	var(T1), var(T2), !,
	mgu_imposes_nothing_vars(T1, T2, VE_In, VE_Out).

mgu_imposes_nothing_1_term(T1, _T2, VE_In, VE_In) :- 
	var(T1), !.
mgu_imposes_nothing_1_term(_T1, T2, VE_In, VE_In) :- 
	var(T2), !, fail.

mgu_imposes_nothing_1_term(T1, T2, VE_In, VE_Out) :-
	clause_head(T1, Name, Arity, Args_1),
	clause_head(T2, Name, Arity, Args_2), !,
	mgu_imposes_nothing_list(Args_1, Args_2, VE_In, VE_Out).

mgu_imposes_nothing_vars(V1, V2, VE_In, VE_In) :-
	var(V1), var(V2),
	memberchk_slash_1(V1/V2, VE_In), !.
mgu_imposes_nothing_vars(V1, V2, VE_In, VE_In) :-
	var(V1), var(V2),
	memberchk_slash_2(V1/_V3, VE_In), !, fail.
% mgu_imposes_nothing_vars(V1, V2, VE_In, VE_In) :-
%	var(V1), var(V2),
%	memberchk_slash_3(_V3/V2, VE_In), !, fail.
mgu_imposes_nothing_vars(V1, V2, VE_In, [V1/V2|VE_In]) :-
	var(V1), var(V2), !.

memberchk_slash_1(T1/T2, [ T3/ T4|_Others]) :- T1 == T3, T2 == T4, !.
memberchk_slash_1(T1/T2, [_T3/_T4| Others]) :- memberchk_slash_1(T1/T2, Others).
memberchk_slash_2(T1/T2, [ T3/ T2|_Others]) :- T1 == T3, !.
memberchk_slash_2(T1/T2, [_T3/_T4| Others]) :- memberchk_slash_2(T1/T2, Others).
% memberchk_slash_3(T1/T2, [ T1/ T4|_Others]) :- T2 == T4, !.
% memberchk_slash_3(T1/T2, [_T3/_T4| Others]) :- memberchk_slash_2(T1/T2, Others).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

include_in_NoCj(NewNoCj,
		cl(Name, Arity, Cjs, NoCjs, Body, N),
		cl(Name, Arity, Cjs, [NewNoCj|NoCjs], Body, N)).

%unificate_cls_with_mgu(_Mgu, []) :- !.
%unificate_cls_with_mgu(Mgu,  [Cl]) :- !,
%	unificate_cl_with_mgu(Mgu, Cl).
%unificate_cls_with_mgu(Mgu,  [Cl|Cls]) :- !,
%	unificate_cl_with_mgu(Mgu, Cl),
%	unificate_cls_with_mgu(Mgu, Cls).

unificate_cl_with_mgu(cl(Name, Arity, Cj1, _NoCjs1, _Body1, _N1),
		      cl(Name, Arity, Cj2, _NoCjs2, _Body2, _N2)) :-
	unificate_cjs(Cj1, Cj2).
%	write(unificate_cjs(Cjs1, Cjs2)),
%	write(' --> OK'), nl.

unificate_cjs([],[]) :- !.
unificate_cjs([Cj1], [Cj2]) :- !,
	Cj1=Cj2.
unificate_cjs([Cj1|Cjs1],[Cj2|Cjs2]):-
	Cj1=Cj2,
        unificate_cjs(Cjs1,Cjs2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compilative_negation(Clauses, LoP, Negated_Clauses) :-
	debug_separation,
	write('% DBG % ------------ COMPILATIVE NEGATION ------------'), nl,
	debug_separation, nl, 
	compilative_negation_aux(Clauses, LoP, [], Negated_Clauses), !,
	debug_formatted_cls('compilative_negation: Clauses: ', Clauses), nl,
	debug_formatted_cls('compilative_negation: Negated_Clauses: ', Negated_Clauses), nl.
compilative_negation(Clauses, LoP, []) :-
	nl, nl, 
	write('% DBG % ERROR % Compilative negation fails !!! '), nl,
	debug_formatted_cls('compilative_negation: Clauses: ', Clauses), nl,
	debug_formatted_cls('compilative_negation: LoP: ', LoP), nl,
	debug_separation, !.

% Separa las clausulas segun el nombre/aridad de los predicados.
compilative_negation_aux(_Clauses_IN, [], Neg_Clauses_IN, Neg_Clauses_IN) :- !.
compilative_negation_aux(Clauses_IN, [Pred/Arity], Neg_Clauses_IN, Neg_Clauses_OUT) :- !,
	compilative_negation_aux_2(Clauses_IN, Pred/Arity, Clauses_Affected, _Clauses_OUT), !, 
	compilative_negation_aux_3(Pred/Arity, Clauses_Affected, Neg_Clauses_IN, Neg_Clauses_OUT), !.
compilative_negation_aux(Clauses_IN, [Pred/Arity|LoP], Neg_Clauses_IN, Neg_Clauses_OUT) :- !,
	compilative_negation_aux_2(Clauses_IN, Pred/Arity, Clauses_Affected, Clauses_OUT), !,
	compilative_negation_aux_3(Pred/Arity, Clauses_Affected, Neg_Clauses_IN, Neg_Clauses_TMP), !,
	compilative_negation_aux(Clauses_OUT, LoP, Neg_Clauses_TMP, Neg_Clauses_OUT), !.


% Separa las Cls por nombre.
compilative_negation_aux_2([], _Pred/_Arity, [], []) :- !.
compilative_negation_aux_2([Cl], Pred/Arity, [Cl], []) :-  
	obtain_clause_name_and_arity(Cl, Pred, Arity), !.
compilative_negation_aux_2([Cl], _Pred/_Arity, [], [Cl]) :- !.
compilative_negation_aux_2([Cl|Cls], Pred/Arity, [Cl|MoreCl], Others) :- 
	obtain_clause_name_and_arity(Cl, Pred, Arity), !, 
	compilative_negation_aux_2(Cls, Pred/Arity, MoreCl, Others).
compilative_negation_aux_2([Cl|Cls], Pred/Arity, MoreCl, [Cl|Others]) :- 
	compilative_negation_aux_2(Cls, Pred/Arity, MoreCl, Others).

% Generate the main negative clause ...
compilative_negation_aux_3(Pred/Arity, Cls, Neg_Clauses_IN, [Neg_Clause|Neg_Clauses_IN]) :-
	append_prefix_to_atom('negated_', Pred, Negated_Pred),
	clause_head(Head, Negated_Pred, Arity, Args),
	compile_negation_clause(Args, Cls, Negated_Body),
	obtain_clause_head(Neg_Clause, Head),
	obtain_clause_cj(Neg_Clause, Args),
	obtain_clause_nocjs(Neg_Clause, []),
	obtain_clause_number(Neg_Clause, -1),
	obtain_clause_body(Neg_Clause, body(Negated_Body)).
% (Head :- Negated_Body)

compile_negation_clause(_Args, [], true) :- !.
compile_negation_clause(Args, [Cl], Negated_Body_Cl) :- !,
	compile_negation_clause_aux(Args, Cl, Negated_Body_Cl).
compile_negation_clause(Args, [Cl|Cls], (Negated_Body_Cl, (Negated_Body_Cls))) :- !,
	compile_negation_clause_aux(Args, Cl, Negated_Body_Cl),
	compile_negation_clause(Args, Cls, Negated_Body_Cls).

compile_negation_clause_aux(Args, Cl, Negated_Body_Cl) :-
	obtain_clause_cj(Cl, Cj),
	compile_negated_cj(Args, Cj, Negated_Body_Cl, Negated_Body_Cont),
	obtain_clause_body(Cl, body(Body)),
	compile_negated_body(Body, Negated_Body_Cont).

compile_negated_cj([], [], Negated_Body_Cont, Negated_Body_Cont) :- !.

compile_negated_cj(Args, Cj, ((negate_eq(Args, NewCj); (keep_eq(Args, Cj), quantify_universally(Vars), Negated_Body_Cont))), Negated_Body_Cont) :-
	copy_term(Cj, NewCj),
	change_free_vars_by_fA(NewCj),
	vars_not_in_head(Negated_Body_Cont, Cj, [], Vars).

compile_negated_body(Body, (Negated_Body1; (Keep_Body1, Negated_Body2))) :- 
	conjunction(Body, Body1, Body2), !,
	compile_keep_body(Body1, Keep_Body1),
	compile_negated_body(Body1, Negated_Body1),
	compile_negated_body(Body2, Negated_Body2).

compile_negated_body(Body, (Negated_Body1, Negated_Body2)) :- 
	disjunction(Body, Body1, Body2), !,
	compile_negated_body(Body1, Negated_Body1),
	compile_negated_body(Body2, Negated_Body2).

compile_negated_body(Body, negate_eq(Left, NewRight)) :- 
	equality(Body, Left, Right), !,
	copy_term(Right, NewRight),
	change_free_vars_by_fA(NewRight).

compile_negated_body(Body, negate_ineq(Left, Right)) :- 
	inequality(Body, Left, Right), !.

compile_negated_body(true, fail) :- !.

compile_negated_body(Body, Negated_Body) :- 
	functor(Body, Name, Arity),
	Body=..[Name|Arguments],
	append_prefix_to_atom('negated_', Name, Negated_Name),
	functor(Negated_Body, Negated_Name, Arity),
	Negated_Body=..[Negated_Name|Arguments].

compile_keep_body(Body, (Keep_Body1, Keep_Body2)) :-
	conjunction(Body, Body1, Body2), !,
	compile_keep_body(Body1, Keep_Body1),
	compile_keep_body(Body2, Keep_Body2).

compile_keep_body(Body, (Keep_Body1; Keep_Body2)) :-
	disjunction(Body, Body1, Body2), !,
	compile_keep_body(Body1, Keep_Body1),
	compile_keep_body(Body2, Keep_Body2).

compile_keep_body(Body, keep_eq(Left, Right)) :-
	equality(Body, Left, Right), !.

compile_keep_body(Body, keep_ineq(Left, Right)) :-
	inequality(Body, Left, Right), !.

compile_keep_body(Body, Body) :- !.

equality(A=B, A, B).
equality(eq(A,B), A, B).
equality(keep_eq(A,B), A, B).

%inequality((A=/=B), A, B).
inequality(dist(A,B), A, B).
inequality(keep_ineq(A,B), A, B).

conjunction(Body, Arg1, Arg2) :-
	functor(Body, ',', 2),
	Body=..[',', Arg1, Arg2].
disjunction(Body, Arg1, Arg2) :-
	functor(Body, ';', 2),
	Body=..[';', Arg1, Arg2].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Returns in Vars the variables in Term not in Args.
vars_not_in_head(Term, Vars_List, Vars_I, Vars_I) :-
	var(Term), 
	memberchk(Term, Vars_List), !.
vars_not_in_head(Term, _Vars_List, Vars_I, [Term|Vars_I]) :-
	var(Term), !.
vars_not_in_head(Term, Vars_List, Vars_I, Vars_O) :-
	functor(Term, Name, _Arity), !,
	Term=..[Name|Args_Functor],
	vars_not_in_head_list(Args_Functor, Vars_List, Vars_I, Vars_O).

vars_not_in_head_list([], _Vars_List, Vars_I, Vars_I) :- !.
vars_not_in_head_list([Arg], Vars_List, Vars_I, Vars_O) :- !,
	vars_not_in_head(Arg, Vars_List, Vars_I, Vars_O).
vars_not_in_head_list([Arg|Args], Vars_List, Vars_I, Vars_O) :- !,
	vars_not_in_head(Arg, Vars_List, Vars_I, Vars_TMP),
	vars_not_in_head_list(Args, Vars_List, Vars_TMP, Vars_O).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Receives a head and returns the variables that appear once in it
% changed by fA(_). Those vars have a universal quantification.
% Repeated vars do not need to be changed by fA(_).

change_free_vars_by_fA(Terms) :-
%	debug_formatted_cls('change_free_vars_by_fA: Terms IN: ', Terms),
	get_variables_from_term(Terms, [], ListVars),
%	debug_formatted_cls('change_free_vars_by_fA: ListVars: ', ListVars),
	get_variables_that_appear_once(ListVars, [], [], ListVars_Once, _ListVars_Repeated),
%	debug_formatted_cls('change_free_vars_by_fA: ListVars_Once: ', ListVars_Once),
	unify_variables_with_fA(ListVars_Once),
%	debug_formatted_cls('change_free_vars_by_fA: Terms OUT: ', Terms),
%	debug_separation,
	!. % Backtracking is not allowed.

change_free_vars_by_fA(Terms) :-
	debug_formatted_cls('change_free_vars_by_fA fails. Terms: ',
			    Terms), !.

% Lo is List of vars that appear once.
% Lr is List of vars that appear more than once.
get_variables_from_term(Term, ListVars, [Term|ListVars]) :-
	var(Term), !.

get_variables_from_term(Term, ListVars_In, ListVars_Out):-
	clause_head(Term, _Name, _Arity, Args),
	term_is_not_forall(Term),
	get_variables_from_term_list(Args, ListVars_In, ListVars_Out).

get_variables_from_term(Term, ListVars_In, ListVars_In):-
	clause_head(Term, _Name, _Arity, _Args),
	term_is_forall(Term).

get_variables_from_term_list([], ListVars, ListVars).
get_variables_from_term_list([X], ListVars_In, ListVars_Out):-
	get_variables_from_term(X, ListVars_In, ListVars_Out).
get_variables_from_term_list([X|Xs], ListVars_In, ListVars_Out_2):-
	get_variables_from_term(X, ListVars_In, ListVars_Out_1),
	get_variables_from_term_list(Xs, ListVars_Out_1, ListVars_Out_2).

% get_variables_that_appear_once(ListVars, Lo_In, Lr_In, Lo_Out, Lr_Out)
get_variables_that_appear_once([], Lo_In, Lr_In, Lo_In, Lr_In) :- !.
get_variables_that_appear_once([Var], Lo_In, Lr_In, Lo_Out, Lr_Out) :- !,
	get_variables_that_appear_once_aux(Var, Lo_In, Lr_In, Lo_Out, Lr_Out).
get_variables_that_appear_once([Var|Others], Lo_In, Lr_In, Lo_Out, Lr_Out) :- !,
	get_variables_that_appear_once_aux(Var, Lo_In, Lr_In, Lo_Out_1, Lr_Out_1),
	get_variables_that_appear_once(Others, Lo_Out_1, Lr_Out_1, Lo_Out, Lr_Out).

get_variables_that_appear_once_aux(Var, Lo_In, Lr_In, Lo_Out, [Var|Lr_In]) :-
	variable_is_in_list(Var, Lo_In), !,
 	remove_variable_from_list(Var, Lo_In, Lo_Out).
get_variables_that_appear_once_aux(Var, Lo_In, Lr_In, Lo_In, Lr_In) :-
	variable_is_in_list(Var, Lr_In), !.
get_variables_that_appear_once_aux(Var, Lo_In, Lr_In, [Var|Lo_In], Lr_In) :-
	(\+(variable_is_in_list(Var, Lo_In))), 
	(\+(variable_is_in_list(Var, Lr_In))), !.

remove_variable_from_list(Var,  [],     []) :-
	write('% DBG % ERROR % remove_variable_from_list: '),
	write(Var), write(' is not in the list of variables.'), nl, !.
remove_variable_from_list(Var1, [Var2],  []) :-
	Var1 == Var2, !.
remove_variable_from_list(Var1, [Var2], [Var2]) :- 
	write('% DBG % ERROR % remove_variable_from_list: '),
	write(Var1), write(' is not in the list of variables.'), nl, !.
remove_variable_from_list(Var1, [Var2|Others], Others) :-
	Var1 == Var2, !.
remove_variable_from_list(Var1, [Var2|Others1], [Var2|Others2]) :- !,
	remove_variable_from_list(Var1, Others1, Others2).

unify_variables_with_fA([]) :- !.
unify_variables_with_fA([Var]) :- !,
	Var = fA(_).
unify_variables_with_fA([Var|Others]) :- !,
	Var = fA(_),
	unify_variables_with_fA(Others).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_of_vars_difference(L1, L2, L3) :-
	list_of_vars_difference_aux_1(L1, L2, [], L3).

list_of_vars_difference_aux_1([],  _L2, L3, L3) :- !.
list_of_vars_difference_aux_1([V],  L2, L3, L4) :- !,
	list_of_vars_difference_aux_2(V, L2, L3, L4).
list_of_vars_difference_aux_1([V|Others], L2, L3, L5) :- !,
	list_of_vars_difference_aux_2(V, L2, L3, L4),
	list_of_vars_difference_aux_1(Others, L2, L4, L5).

list_of_vars_difference_aux_2(V, L2, L3, L3) :-
	variable_is_in_list(V, L2),
	!. % Backtracking forbidden.
list_of_vars_difference_aux_2(V, _L2, L3, [V|L3]) :- !.


get_variables_from_clause(Cl, List1, List2) :-
	var(Cl), !,
	include_variable(Cl, List1, List2).
get_variables_from_clause(Cl, List1, List2) :-
	clause_head(Cl, _Name, _Arity, Args),
	get_variables_from_clauses_list(Args, List1, List2).
get_variables_from_clause((Cl), List1, List2) :- !,
	get_variables_from_clause(Cl, List1, List2).
get_variables_from_clause(Cl, List, List) :-
	write('% DBG % ERROR: get_variables_from_clause: unknown term: '),
	write(Cl), nl.

get_variables_from_clauses_list([], List, List) :- !.
get_variables_from_clauses_list([Cl], List1, List2) :- !,
	get_variables_from_clause(Cl, List1, List2).
get_variables_from_clauses_list([Cl|Cls], List1, List3) :- !,
	get_variables_from_clause(Cl, List1, List2),
	get_variables_from_clauses_list(Cls, List2, List3).

include_variable(Var, List, List) :-
	variable_is_in_list(Var, List), !.
include_variable(Var, List, [Var|List]).

variable_is_in_list(Var1, [Var2]) :- !,
	Var1 == Var2.
variable_is_in_list(Var1, [Var2|_Others]) :-
	Var1 == Var2, !.
variable_is_in_list(Var1, [_Var2|Others]) :- !,
	variable_is_in_list(Var1, Others).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

auxiliary_implementation([
%	( 0 )
%	     ( :- meta_predicate(intneg_forall) ),
%	     ( :- meta_predicate(findall_forall) ),
%	     (   
%		 intneg_forall(ForAll_Vars, Pred, L) :-
%	         term_vars(Pred, Pred_Vars),
%		 remove_vars_in_2nd_list(Pred_Vars, ForAll_Vars, Not_ForAll_Vars),
%		 findall_forall(   (ForAll_Vars, Not_ForAll_Vars, expl(Pred)),
%				   intneg_pred(pos, Pred, L), Combined_In), !,
%		 intneg_forall_aux(ForAll_Vars, Not_ForAll_Vars, Combined_In, _Expl)
%	     ),
%	     (
%	         findall_forall(Result_Format, Predicate, Results) :-
%	         copy_term((Result_Format, Predicate), (Result_Format_Copy, Predicate_Copy)),
%	         new_counter_index(Cte), !, 
%	         call(Predicate_Copy),
%		 write('We are calling: '), write(Predicate_Copy), nl,
%	         adequate_forall_results(Cte, Result_Format_Copy, Results)
%	     )
	    ]).

% copy_term((Vars, Pred), (Vars_Copy, _Pred_Copy))),
% get_returns(CS,Ret)),
% get_returns/2, get_returns_for_call/2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


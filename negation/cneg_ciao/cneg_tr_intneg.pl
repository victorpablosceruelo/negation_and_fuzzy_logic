
:- module(cneg_tr_intneg,[generate_tr_intneg_cls/4, cneg_tr_intneg_negate_literal/4],[assertions]).
:- use_module(cneg_aux, _).
:- use_module(library(terms), _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- comment(title, "Contructive Negation Transformation - intneg").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module does de transformation needed to evaluate 
	the constructive intensional negation of any predicate in the original file.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

name_main_negation_predicate("cneg_rt_intneg").
prefix_main_clauses("cneg_tr_intneg_").
prefix_auxiliary_clauses("cneg_tr_intneg_aux_").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_tr_intneg_cls(List_Of_Preds, List_Of_H_and_B, Cls_In, Cls_Out) :-
	echo_msg(2, '', 'cneg_tr_intneg', 'generate_tr_intneg_cls :: Cls_In', (Cls_In)),
	echo_msg(2, '', 'cneg_tr_intneg', 'generate_tr_intneg_cls :: List_Of_Preds', (List_Of_Preds)),
	echo_msg(2, '', 'cneg_tr_intneg', 'generate_tr_intneg_cls :: List_Of_H_and_B', (List_Of_H_and_B)),

%	generate_tr_intneg_main_clauses(List_Of_Preds, Cls_In, Cls_Aux),
%	generate_tr_intneg_auxiliary_clauses(List_Of_H_and_B, Cls_Aux, Cls_Out).
	Cls_Out = Cls_In.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_tr_intneg_main_clauses([], Cls_In, Cls_In) :- !.
generate_tr_intneg_main_clauses([(Name, Arity, Counter) | List_Of_Preds], Cls_In, Cls_Out) :-
	generate_tr_intneg_main_clause(Name, Arity, Counter, Cl),
	generate_tr_intneg_main_clauses(List_Of_Preds, [Cl | Cls_In], Cls_Out).

generate_tr_intneg_main_clause(Name, Arity, _Counter, Cl) :- 
	cneg_main_and_aux_cl_names(Name, Main_Cl_Name, _Aux_Cl_Name),
	functor_local(Cl, ':-', 2, [Head | [_Body]]), 
	functor_local(Head, Main_Cl_Name, Arity, _Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_main_and_aux_cl_names(Name, Main_Cl_Name, Aux_Cl_Name) :-
	name(Name, Name_String),

	prefix_main_clauses(Prefix_1),
	append(Prefix_1, Name_String, Main_Cl_String),
	name(Main_Cl_Name, Main_Cl_String),

	prefix_auxiliary_clauses(Prefix_2),
	append(Prefix_2, Name_String, Aux_Cl_String),
	name(Aux_Cl_Name, Aux_Cl_String).


generate_tr_intneg_auxiliary_clauses([], Cls_In, Cls_In) :- !.
generate_tr_intneg_auxiliary_clauses([H_and_B | List_Of_H_and_B], Cls_In, Cls_Out) :-
	generate_tr_intneg_auxiliary_clause(H_and_B, Cl),
	generate_tr_intneg_auxiliary_clauses(List_Of_H_and_B, [Cl | Cls_In], Cls_Out).

generate_tr_intneg_auxiliary_clause(_H_and_B, _Cl) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_tr_intneg_negate_literal(Body, GoalVars, Result, Neg_Body) :-
	echo_msg(2, '', 'cneg_tr_intneg', 'cneg_tr_intneg_negate_literal :: (Body, GoalVars)', (Body, GoalVars)),
	Result = true, 
	Neg_Body = Body.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

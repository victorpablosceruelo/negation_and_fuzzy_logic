
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

name_main_negation_predicate("cneg_hybrid").
prefix_main_clauses("cneg_tr_hybrid_cneg_").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_tr_intneg_cls(List_Of_Preds, List_Of_H_and_B, Cls_In, Cls_Out) :-
	echo_msg(2, '', 'cneg_tr_intneg', 'generate_tr_intneg_cls :: Cls_In', (Cls_In)),
	echo_msg(2, '', 'cneg_tr_intneg', 'generate_tr_intneg_cls :: List_Of_Preds', (List_Of_Preds)),
	echo_msg(2, '', 'cneg_tr_intneg', 'generate_tr_intneg_cls :: List_Of_H_and_B', (List_Of_H_and_B)),
	Cls_Out = Cls_In.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_tr_intneg_negate_literal(Body, GoalVars, Result, Neg_Body) :-
	echo_msg(2, '', 'cneg_tr_intneg', 'cneg_tr_intneg_negate_literal :: (Body, GoalVars)', (Body, GoalVars)),
	Result = true, 
	Neg_Body = Body.

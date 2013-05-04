:- module(pe1_post_processing,[post_process_pe1/0,post_process_pe1/1],[regtypes,andprolog]).

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(lists), [append/3, length/2]).
:- use_module(library(write), [write/1]).

:- use_module(library(vndict), [create_pretty_dict/2]).
:- use_module(program(p_unit), [program/2]).
:- use_module(program(clause_db), [source_clause/3]).
:- use_module(program(clidlist), 
 	[inverse_rewrite_source_program/2, 
 	 rewrite_source_clause/4,clause_key/2]).
%:- use_module(program(itf_base_db),[defines/3,exports/2]).
:- use_module(program(itf_db), [curr_file/2]).
%:- use_module(ciaopp(api(api_order)),[pr_order_clean/0,pr_order_get/1,pr_order_set/1]).
:- use_module(spec(unfold_operations), [body2list/2, list2body/2]).


:- data option/1.

post_process_pe1 :- post_process_pe1([res_calls]).
post_process_pe1(Opts) :-
	cleanup,
	process_options(Opts),
	retractall_fact(source_clause(_,directive(_),_)),
	program(P,_),
	retractall_fact(source_clause(_,_,_)),
	inverse_rewrite_source_program(P,P_p),
	member(clause(H,B),P_p),
	body2list(B,BList),
	process_head(H,H_p),
	process_body(BList,BList_p),
	list2body(BList_p,B_p),
	clause_key(H_p,NewClId), 
	rewrite_source_clause(H_p,B_p,NewClId,NewCl),
	create_pretty_dict(NewCl,NewDict),
	assertz_fact(source_clause(NewClId,NewCl,NewDict)),
	fail.
post_process_pe1(_) :- 
%	update_module_header,
%	add_directives,
	cleanup,
	nl, write('PE1 Post-processing... Done'), nl.

process_options(Opts) :-
	member(Opt,Opts),
	assertz_fact(option(Opt)),
	fail.
process_options(_).

cleanup :- 
	retractall_fact(option(_)).

process_head(H,H).

process_body([Atom|As],[QCallMain|As_p]) :-
	module_functor_args(Atom,_,res_call,[CallMain]),!,
	split_atom(CallMain,_,F,Args),
	curr_file(_,CurrM),
	build_atom(QCallMain,CurrM,F,Args),
	process_body(As,As_p).
process_body([Atom|As],[Atom_p|As_p]) :-
	option(clpfd),
	split_atom(Atom,M,_F,_Args),
	curr_file(_,CurrM),
	M \== CurrM,!,
	arith_to_clp(Atom,Atom_p),
	process_body(As,As_p).
process_body([Atom|As],[Atom|As_p]) :-
	process_body(As,As_p).
process_body([],[]).

arith_to_clp(A,A_p) :-
	option(clpfd),
	split_atom(A,arithmetic,F,Args),!,
	clp_conversion(arithmetic,F,Args,A_p).
arith_to_clp(A,A_p) :-
	option(clpfd),
	split_atom(A,term_basic,F,Args),!,
	clp_conversion(term_basic,F,Args,A_p).
arith_to_clp(A,A).

%% CLP conversion is not being used. It is done later on during program loading.
%% clp_conversion(_,is,[A,B],'#='(A,B)) :- !.
%% clp_conversion(_,\=,[A,B],A '#\=' B) :- !.
%% clp_conversion(_,=\=,[A,B],A #\= B) :- !.
%% clp_conversion(_,\==,[A,B],A #\= B) :- !.
%% clp_conversion(_,<,[A,B],A #< B) :- !.
%% clp_conversion(_,>,[A,B],A #> B) :- !.
%% clp_conversion(_,=<,[A,B],A #=< B) :- !.
%% clp_conversion(_,>=,[A,B],A #>= B) :- !.
clp_conversion(M,F,Args,A_p) :-
	build_atom(A_p,M,F,Args).

split_atom(Atom,M,F,Args) :-
	Atom =..[Atom_f|Args],
	atom_concat([M,:,F],Atom_f),!.
split_atom(Atom,'',Atom_f,Args) :-
	Atom =..[Atom_f|Args].

build_atom(Atom,M,F,Args) :-
	atom_concat([M,:,F],Atom_f),
	Atom =..[Atom_f|Args].

module_functor_args(Atom,M,F,Args) :-
	Atom =..[Atom_f|Args],
	atom_concat([M,:,F],Atom_f).

generate_qualified(ModName,PredName,Arity,QPred,QGoal) :-
	atom_concat([ModName,':',PredName],QPred),
	length(Args,Arity),
	QGoal =..[QPred|Args].

add_directives :-
	assertz_fact(source_clause(1/0/1,directive(include(library(jvm_in_ciao(interpreter(exec_header))))),dic([],[]))).


%% update_module_header :-
%% 	curr_file(_,ModName),
%% 	generate_qualified(ModName,main,4,MainQPred,MainQGoal),
%% 	retractall_fact(itf_base_db:defines(MainQPred,4,ModName)),
%% 	retractall_fact(itf_base_db:exports(MainQGoal,ModName)),
%% 	add_new_pred(init,0),
%% 	new_pred(NewPred),
%% 	functor(NewPred,SMN,Arity),
%% 	generate_qualified(ModName,SMN,Arity,QPred,QGoal),
%% 	assertz_fact(itf_base_db:defines(QPred,Arity,ModName)),
%% 	assertz_fact(itf_base_db:exports(QGoal,ModName)),
%% 	fail.
%% update_module_header.


%% print_directives :-
%% 	current_fact(source_clause(A,directive(B),C)),
%% 	write(source_clause(A,directive(B),C)),nl,
%% 	fail.
%% print_directives.

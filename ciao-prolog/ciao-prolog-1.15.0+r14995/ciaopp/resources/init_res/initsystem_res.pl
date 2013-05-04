:- module(initsystem_res,
	    [
		call_graph/5,
		insert_symbol_table/4
	    ], [assertions]).

%
%  initsystem.pl		Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for initializing the system.
%

:- use_module(program(clidtypes)).
:- use_module(resources(init_res(callgraph_res)),
	    [
		insert_call_entry/3,
		insert_call_field/4
	    ]).
:- use_module(resources(init_res(symtable_res)),
	    [
		insert_symbol_field/4,
		find_symbol_entry/3
	    ]).
:- use_module(resources(init_res(builtin_res)),
	    [
		second_order_predicate/1,
		second_order_predicate_pred_arg/2
	    ]).
:- use_module(resources(init_res(dec_res)),    [insert_symbol_dec/3]).
:- use_module(resources(top_res(utility_res)), [nonsequence/1]).
:- use_module(resources(init_res(initsystem_basic_res)),
	    [clause_type/2]).
:- use_module(resources(resources_basic)).

% 
%  The following procedures insert the clauses into the symbol table.
% 

:- pred insert_symbol_table/4 :: clause_t * list(symbol_entry) * clause_key_t
	* nnegint # "Insert a clause into the symbol table.".
insert_symbol_table(decl, ST, Clause, Error) :-
	insert_symbol_dec(ST, Clause, Error).
insert_symbol_table(rule, ST, Clause, 1) :-
	insert_symbol_clause(ST, Clause).
insert_symbol_table(fact, ST, Clause, 1) :-
	insert_symbol_clause(ST, Clause).

:- pred insert_symbol_clause/2 :: list(symbol_entry) * clause_key_t #
	"Insert a rule or a fact into the symbol table.".
insert_symbol_clause(ST, Clause) :-
	clause_key(Clause, Rule, _Key),
	clause_head(Rule, Head),
	functor(Head, F, N),
	insert_symbol_field(ST, F/N, clause, Clause).

%  
%  The following procedures insert the clauses into the call graph.
% 

:- pred call_graph/5 :: list(clause_key_t) * list(bottom_entry)
	* list(symbol_entry) * term * nnegint #
	"Build the call graph of the program.".
call_graph([],             _,  _,  _,  1).
call_graph([ClauseKey|PT], BT, ST, CG, Error) :-
	clause_key(ClauseKey, ClausePPKey, _Key),
	clause_type(ClausePPKey, Type),
	insert_call_graph(Type, BT, ST, CG, ClausePPKey, Error1),
	call_graph(PT, BT, ST, CG, Error2),
	Error is Error1*Error2.

:- pred insert_call_graph/6 :: clause_t * list(bottom_entry)
	* list(symbol_entry) * term * clause_ppkey_t * nnegint #
	"Insert a clause into the call graph.".
insert_call_graph(decl, _,  _,  _,  _,      1).
insert_call_graph(rule, BT, ST, CG, Clause, Error) :-
	clause_head_body(Clause, Head, Body),
	functor(Head, F, N),
	insert_call_entry(CG, F/N, _),
	insert_call_body(Body, BT, ST, CG, F/N, Clause, Error).
insert_call_graph(fact, _, _, CG, Clause, 1) :-
	!,
	clause_head(Clause, Head),
	functor(Head, F, N),
	insert_call_entry(CG, F/N, _).

:- pred insert_call_body/7 :: clausebody * list(bottom_entry)
	* list(symbol_entry) * term * predname * clause_ppkey_t * nnegint #
	"Insert the literals in the body of a rule into the call graph.".
insert_call_body((LitPPKey, Body), BT, ST, CG, Pred, Clause, Error) :-
	!,
	lit_ppkey(LitPPKey, Lit, _PPKey),
	insert_call_lit(BT, ST, CG, Pred, Lit, Clause, Error1),
	insert_call_body(Body, BT, ST, CG, Pred, Clause, Error2),
	Error is Error1*Error2.
insert_call_body(LitPPKey, BT, ST, CG, Pred, Clause, Error) :-
	lit_ppkey(LitPPKey, Lit, _Key),
	nonsequence(Lit),
	insert_call_lit(BT, ST, CG, Pred, Lit, Clause, Error).

%
%  Insert a non-builtin non-recursive literal into the call graph.
%
insert_call_lit(_, _, _, Pred, Lit, _, 1) :-
	functor(Lit, F, A), F/A == Pred, !.
insert_call_lit(BT, ST, CG, Pred, Lit, Clause, Error) :-
	functor(Lit, F, A), F/A \== Pred,
	( second_order_predicate(F/A) ->
	    insert_call_lit_1(BT, ST, CG, Pred, Lit, Clause, Error) ;
	    insert_call_lit_2(ST, CG, Pred, Lit, Error) ).

insert_call_lit_1(BT, ST, CG, Pred, Lit, Clause, Error) :-
	second_order_predicate_pred_arg(Lit, L),
	insert_call_body(L, BT, ST, CG, Pred, Clause, Error).

% We assume that non defined predicates have the cost defined with
% trust assertions

insert_call_lit_2(ST, CG, Pred, Lit, Error) :-
	functor(Lit, F, A),
	find_symbol_entry(ST, F/A, STEntry),
	(
	    nonvar(STEntry) ->
	    insert_call_field(CG, Pred, edge, F/A)
	;
	    true
	),
	Error = 1.

/*
insert_call_lit_2(BT, ST, CG, Pred, Lit, Clause, Error) :-
	functor(Lit, F, A),
	find_symbol_entry(BT, F/A, BTEntry),
	(
	    var(BTEntry) ->
	    find_symbol_entry(ST, F/A, STEntry),
	    (
		var(STEntry) ->
		error_message(lit1, F/A, Clause),
		Error = 0
	    ;
		insert_call_field(CG, Pred, edge, F/A),
		Error = 1
	    )
	;
	    Error = 1
	).
*/

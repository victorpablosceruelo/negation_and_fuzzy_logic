:- module(initsystem,
	[
	    call_graph/5,
	    insert_symbol_table/4
	], [assertions]).

%
%  initsystem.pl		Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for initializing the system.
%

:- use_module(infercost(init(callgraph)), 
	[
	    insert_call_entry/3,
	    insert_call_field/4
	]).
:- use_module(infercost(init(symtable)), 
	[
	    insert_symbol_field/4,
	    find_symbol_entry/3
	]).
:- use_module(infercost(init(builtin)), 
	[
	    second_order_predicate/1,
	    second_order_predicate_pred_arg/2
	]).
:- use_module(infercost(init(dec)), [insert_symbol_dec/3]).
:- use_module(infercost(top(utility)), [nonsequence/1]).
:- use_module(infercost(top(error)), [error_message/3]).
:- use_module(infercost(init(initsystem_basic)), [clause_type/2]).

% 
%  The following procedures insert the clauses into the symbol table.
% 

%
%  Insert a clause into the symbol table.
%
insert_symbol_table(1,ST,Clause,Error) :-
	insert_symbol_dec(ST,Clause,Error).
insert_symbol_table(2,ST,Clause,1) :-
	insert_symbol_rule(ST,Clause).
insert_symbol_table(3,ST,Clause,1) :-
	insert_symbol_fact(ST,Clause).

%
%  Insert a rule into the symbol table.
%
insert_symbol_rule(ST,Rule:Key) :-
	arg(1,Rule,Head),
	functor(Head,F,N),
	insert_symbol_field(ST,F/N,clause,Rule:Key).

%
%  Insert a fact into the symbol table.
%
insert_symbol_fact(ST,Fact:Key) :-
	functor(Fact,F,N),
	insert_symbol_field(ST,F/N,clause,Fact:Key).

%  
%  The following procedures insert the clauses into the call graph.
% 

%
%  Build the call graph of the program.
%
call_graph([],_,_,_,1).
call_graph([Clause:_Key|PT],BT,ST,CG,Error) :-
	clause_type(Clause,Type),
	insert_call_graph(Type,BT,ST,CG,Clause,Error1),
	call_graph(PT,BT,ST,CG,Error2),
	Error is Error1*Error2.

%
%
%  Insert a clause into the call graph.
%
insert_call_graph(1,_,_,_,_,1).
insert_call_graph(2,BT,ST,CG,Clause,Error) :-
	arg(1,Clause,Head),
	arg(2,Clause,Body),
	functor(Head,F,N),
	insert_call_entry(CG,F/N,_),
	insert_call_body(BT,ST,CG,F/N,Body,Clause,Error).
insert_call_graph(3,_,_,CG,Clause,1) :-
	functor(Clause,F,N),
	insert_call_entry(CG,F/N,_).

%
%  Insert the literals in the body of a rule into the call graph.
%
insert_call_body(BT,ST,CG,Pred,(Lit,Body),Clause,Error) :-
	insert_call_lit(BT,ST,CG,Pred,Lit,Clause,Error1),
	insert_call_body(BT,ST,CG,Pred,Body,Clause,Error2),
	Error is Error1*Error2.
insert_call_body(BT,ST,CG,Pred,Lit,Clause,Error) :-
	nonsequence(Lit),
	insert_call_lit(BT,ST,CG,Pred,Lit,Clause,Error).

%
%  Insert a non-builtin non-recursive literal into the call graph.
%
insert_call_lit(_,_,_,Pred,Lit,_,1) :-
	functor(Lit,F,A), F/A == Pred, !.
insert_call_lit(BT,ST,CG,Pred,Lit,Clause,Error) :-
	functor(Lit,F,A), F/A \== Pred,
	(second_order_predicate(F/A) ->
		insert_call_lit_1(BT,ST,CG,Pred,Lit,Clause,Error);
		insert_call_lit_2(BT,ST,CG,Pred,Lit,Clause,Error)).

insert_call_lit_1(BT,ST,CG,Pred,Lit,Clause,Error) :-
	second_order_predicate_pred_arg(Lit,L),
	insert_call_body(BT,ST,CG,Pred,L,Clause,Error).

insert_call_lit_2(BT,ST,CG,Pred,Lit,Clause,Error) :-
	functor(Lit,F,A),
	find_symbol_entry(BT,F/A,BTEntry),
	(var(BTEntry) ->
		(find_symbol_entry(ST,F/A,STEntry),
		 (var(STEntry) ->
			(error_message(lit1,F/A,Clause),
			 Error = 0);
			(insert_call_field(CG,Pred,edge,F/A),
			 Error = 1)));
		Error = 1).

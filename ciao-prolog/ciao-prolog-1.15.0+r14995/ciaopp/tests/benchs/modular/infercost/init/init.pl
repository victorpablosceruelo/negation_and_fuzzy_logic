
:- module(init,
	[ clause_type/2,
	  find_symbol_field/4,
	  find_symbol_entry/3,
	  insert_symbol_field/4,
	  legal_pred_arg/1,
	  literal_property/5,
	  second_order_predicate/1,
	  second_order_predicate_pred_arg/2,
	  second_order_predicate_pred_num/3
	],
	[]).

:- use_module('..'(algebraic)).
:- use_module('..'(size)).
:- use_module('..'(top)).

:- use_module('..'(database), [approximation/1]).
:- use_module('..'(undefined)).

:- push_prolog_flag(multi_arity_warnings,off).
:- push_prolog_flag(discontiguous_warnings,off).

:- include(builtin).
:- include(callgraph).
:- include(dec).
:- include(initsystem).
:- include(scc).
:- include(symtable).

:- pop_prolog_flag(discontiguous_warnings).
:- pop_prolog_flag(multi_arity_warnings).

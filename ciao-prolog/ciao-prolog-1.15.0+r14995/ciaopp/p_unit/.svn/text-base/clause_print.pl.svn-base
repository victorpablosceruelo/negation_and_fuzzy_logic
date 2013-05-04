:- module(clause_print,
	[ format_clauses/0,
	  format_clauses/1,
	  print_clauses/0,
	  print_clauses/1
	],[ assertions ]).

:- use_module(program(clause_db)).
:- use_module(library(vndict), [rename/2, varnamesl2dict/2]).
:- use_module(library(write)).

print_clauses(M) :-
	io_aux:message('{Printing clauses read '),
	clause_read(M, Head, Body, VarNames, Source, Line0, Line1),
	display(clause_read(Head, Body, VarNames, Source, Line0, Line1)),
	nl,
	fail.
print_clauses(_M) :-
	io_aux:message('}').

print_clauses :-
	io_aux:message('{Printing clauses read '),
	clause_read(M, Head, Body, _VarNames, _Source, _Line0, _Line1),
	\+ number(Head),
	display(clause_read(M, Head, Body)),
	nl,
	fail.
print_clauses :-
	io_aux:message('}').

format_clauses :- format_clauses(_).

format_clauses(M) :-
	clause_read(M, Head, Body, VarNames, _Source, _Line0, _Line1),
	varnamesl2dict(VarNames,Dict),
	rename((Head:-Body),Dict),
	display(M), display(' : '), 
	( number(Head)
	-> writeq((:- Body))
	 ; writeq((Head:-Body))
	),
	nl,
	fail.
format_clauses(_).

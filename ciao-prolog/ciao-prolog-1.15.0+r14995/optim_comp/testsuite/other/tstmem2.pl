% execute p or a(0) or a2(0)

:- module(_, _, []).

:- use_module(library(prolog_sys)).

:- use_module(engine(internals)).

show_stats :-
	put_code(8'33),
	display('[d'),
%	put_code(8'33),
%	display('[2J'),
	nl,
%	'$empty_gcdef_bin',
	statistics.

% Must run in constant size
p :-
	show_stats,
	q,
	'$empty_gcdef_bin', % TODO: hmmm...
	!,
	p.

q :-
	'$define_predicate'('tstmem2:f'/1, 2'01), % TODO: see dynamic.pl
	asserta_fact(f(1)),
	asserta_fact(f(5)),
	assertz_fact(f(1)),
	assertz_fact(f(1)),
	'$abolish'('tstmem2:f'(_)).

% Does not run in constant size!!
% Hast tables are expanded from time to time...
a(I) :-
	show_stats,
	display(b(I)), nl,
	b(I),
	I1 is I + 1,
	a(I1).

:- data f/1.
b(I) :-
	asserta_fact(f(I)),
	retractall_fact(f(_)).

% Shoul run in constant size (no hashtables involved)
a2(I) :-
	show_stats,
	display(b2(I)), nl,
	b2(I),
	I1 is I + 1,
	a2(I1).

:- data f2/2.
b2(I) :-
	asserta_fact(f2(_,I)),
	retractall_fact(f2(_,_)).

:- use_module(engine(rt_exp), ['$define_predicate'/2]).
%:- use_module(engine(internals),
%	['$current_clauses'/2,
%	 '$erase_nb_root'/3]).
:- use_module(engine(internals), ['$abolish'/1]).

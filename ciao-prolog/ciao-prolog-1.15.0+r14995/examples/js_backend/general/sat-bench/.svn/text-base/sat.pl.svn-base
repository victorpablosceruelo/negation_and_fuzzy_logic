:- module(sat, [], [benchmark]).

% Modified by Jose F. Morales as a benchmark, based on Jacob Howe and
% Andy King "Harness for SAT solver".

name("A Pearl on SAT Solving (using freeze)").
repeat_count(10).
%repeat_count(1).
solve(Sols) :-
	harness([chat_80_1,
	         chat_80_2,
		 chat_80_3,
		 chat_80_4,
		 chat_80_5,
		 chat_80_6,
		 'uf50-0429',
		 'uuf50-0168'
		 ], Sols).

:- use_module(library(lists)).

% In order to avoid dependencies with parsers, we store parsed
% problems as precomputed facts.
%%% :- use_module(parser).
:- if(defined(optim_comp)).
:- include(.(static_files)).
:- else.
:- include(static_files).
:- endif.

:- if(defined(optim_comp)).
:- use_module(.(static_var_order)).
:- else.
:- use_module(static_var_order).
:- endif.

%:- use_module(sat_solver_core).
%:- use_module(sat_solver_back).
%:- use_module(sat_solver_instrumented).
%:- use_module(sat_solver_learning).
%:- use_module(sat_solver_restore).
:- if(defined(optim_comp)).
:- use_module(.(sat_solver_freeze)).
:- else.
:- use_module(sat_solver_freeze).
:- endif.
%:- use_module(sat_solver_when).

harness([], []) :- !.
harness([X|Xs], [Y|Ys]) :- !,
	harness(X, Y), harness(Xs, Ys).
harness(File, Sol) :-
	parser(File, Clauses, Vars), !,
	harness_(File, Clauses, Vars, Sol).

harness_(File, Clauses, Vars, Sol) :-
	order_variables(Clauses, Vars, Ordered_Vars),
	initialise([]),
	search(Clauses, Ordered_Vars, Sat, _),
	length(Clauses, NClauses),
	length(Vars, NVars),
	( Sat = true -> % sat
	    Sol = [sat, File, NClauses, NVars, Vars]
	; Sat = false -> % unsat
	    Sol = [unsat, File, NClauses, NVars]
	),
	!.

%trim([], [], _).
%trim([_ | _], [...], 0) :- !.
%trim([true | Vs], [1 | Ts], N) :-
%	N >= 0, !,
%	N1 is N - 1,
%	trim(Vs, Ts, N1).
%trim([false | Vs], [0 | Ts], N) :-
%	N >= 0, 
%	N1 is N - 1,
%	trim(Vs, Ts, N1).



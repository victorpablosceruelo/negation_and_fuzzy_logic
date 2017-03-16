:- module(rul_paths_generator,[generate_rul_paths/2,generate_rul_paths/1,
	trace_rul_clause/2,cleanup_trace_rul_clauses/0],[assertions]).

:- doc(title,"Path generator as regular tree languages with RUL representation").

:- doc(author, "M. Zamalloa").

:- doc(module,"This module provides the operations to automatically generate the RUL's describing
	the infinite set of trace terms (success paths) for a given program. The calculation
	basically consists in performing a regular types analysis (using ciaopp). It assumes
	a program instrumented with trace terms by the trace_instrumenter").

:- use_module(library(compiler), [use_module/1]).
:- use_module(library(filenames), [no_path_file_name/2, basename/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(format), [format/3]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(write), [write/1]).

:- use_module(ciaopp(driver), [module/2, analyze/2, transform/1]).
:- use_module(ciaopp(printer), [output/1]).
:- use_module(typeslib(typeslib), [get_required_types/1, typedef_to_pred/3]).
:- use_module(spec(unfold_operations), [body2list/2, list2body/2]).

:- data trace_rul_clause/2.

cleanup_trace_rul_clauses :-
	retractall_fact(trace_rul_clause(_,_)).

generate_rul_paths(M) :- generate_rul_paths(M,_).
generate_rul_paths(M,_Opts) :-
%	use_module(M),
	cleanup_trace_rul_clauses,
	module(M,_),
	%statistics(runtime,[Time1,_]),
 	analyze(eterms,C_Ana),
	C_Ana = [time(_Time,_)|_],
	%statistics(runtime,[Time2,_]),
	%Time is Time2 - Time1,write(Time),
	%write(C_Ana),
	basename(M,ModuleName),
	atom_concat([ModuleName,'_rul','.pl'],OutputFile),
 	output(OutputFile),
	get_required_types(Rules), % This has to be done after output/1
	process_type_rules(Rules).

process_type_rules(Rules) :-
	member(Rule,Rules),
	Rule = typedef(::=(Pred,Def)),
	functor(Pred,TypeName,_Ari),
	typedef_to_pred(Def,TypeName,Cls),
	member(clause(H,B),Cls),
	body2list(B,BList),
	assertz_fact(trace_rul_clause(H,BList)),
	write(trace_rul_clause(H,BList)),nl,
	fail.
process_type_rules(_).


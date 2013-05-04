:- module(_,[main/0],[assertions]).

:- use_module(ciaopp(ciaopp)).

:- use_module(library(lists), [delete/3]).
:- use_module(library(system_extra), [ls/3]).
:- use_module(library(write)).

:- doc(title,"Partial Evaluator examples").

:- doc(module,"This modules runs Partial Evaluation over a set of
benchmarks residing in the current directory").


main:-  init_benchmarks(Bs),
 	set_flags,
	peval_benchs(Bs).

:- pred peval_benchs(+Benchs) #"Takes a list @var{Benchs} of
benchmarks and runs partial evaluation on each of them".

peval_benchs([]).  
peval_benchs([B|Bs]):- 
	module(B), 
	analyze(pd),
	transform(codegen), 
	output, 
	peval_benchs(Bs).

:- pred init_benchmarks(-Bs) #"It returns the list @var{Bs} of
benchmarks in the current directory. This list is assumed to be any
.pl except this file".

init_benchmarks(Bs) :- 
	ls('.','*.pl',Ls),
	delete(Ls,'run.pl',Bs).

:-pred set_flags #"Sets global flags, valid for all benchmarks".

set_flags :-
	set_prolog_flag(quiet,error),
	set_pp_flag(local_control,df_hom_emb_as).

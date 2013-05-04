:- module(tak_main,
        [
            tak_test/2
        ],
	[]).


:- use_package(andprolog_nd).

:- use_module('./common_bench').
:- use_module('./tak').
:- use_module('./tak_shfr_mel_udg').
:- use_module('./tak_shfr_uoudg').
:- use_module('./tak_shfr_uudg').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions without granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tak_test(Reps, Name/Arity) :-
        functor(Goal, Name, Arity),
        bench_rep(Reps, Goal, Name/Arity).

:- push_prolog_flag(multi_arity_warnings, off).

:- use_module(library(lists)).

tak_seq :- 
	tak:main.

tak_mel_udg :- 
	tak_shfr_mel_udg:main.

tak_uoudg :- 
	tak_shfr_uoudg:main.

tak_uudg :- 
	tak_shfr_uudg:main.



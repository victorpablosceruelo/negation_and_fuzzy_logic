:- module(hamming_main,
        [
            hamming_test/2
        ],
	[]).


:- use_package(andprolog_nd).

:- use_module('./common_bench').
:- use_module('./hamming').
:- use_module('./hamming_mel_udg').
:- use_module('./hamming_uoudg_uudg').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions without granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hamming_test(Reps, Name/Arity) :-
        functor(Goal, Name, Arity),
        bench_rep(Reps, Goal, Name/Arity).

:- push_prolog_flag(multi_arity_warnings, off).

:- use_module(library(lists)).

hamming_seq :- 
	hamming:main.

hamming_mel_udg :- 
	hamming_mel_udg:main, !.

hamming_uoudg_uudg :- 
	hamming_uoudg_uudg:main, !.


:- module(hamming_test, [h_test/3], []).

:- use_module(library(format)).
:- use_module(library(between)).

:- use_module('./common_bench').
:- use_module(hamming_main).

h_test(_,_MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~nExecuting sequential benchmark~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        hamming_test(Reps, hamming_seq/0), 
        format("~n--------------------------------------------~n", []),
        fail.

h_test(1,MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (MEL & UDG)~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                          hamming_test(Reps, hamming_mel_udg/0)),
        fail.

h_test(2,MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UOUDG & UUDG)~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                          hamming_test(Reps, hamming_uoudg_uudg/0)),
        fail.

h_test(_, _, _).


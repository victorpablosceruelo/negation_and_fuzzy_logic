:- module(aiakl_uudg_test, [a_test/4], []).

:- use_module(library(format)).
:- use_module(library(between)).

:- use_module('./common_bench').
:- use_module(aiakl_uudg_main).

a_test(_, Length, _MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~nExecuting sequential benchmark~n", []),
        format("N - ~d, ~d repetitions~n", [Length, Reps]),
        format("--------------------------------------------~n", []),
        aiakl_test(Length, Reps, init_vars_seq/4),
        format("~n--------------------------------------------~n", []),
        fail.

a_test(1, Length, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (MEL)~n", []),
        format("N - ~d, ~d repetitions~n", [Length, Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                     aiakl_test(Length, Reps, init_vars_mel/4)),
        fail.

a_test(2, Length, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UDG)~n", []),
        format("N - ~d, ~d repetitions~n", [Length, Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                     aiakl_test(Length, Reps, init_vars_udg/4)),
        fail.

a_test(3, Length, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UOUDG)~n", []),
        format("N - ~d, ~d repetitions~n", [Length, Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                     aiakl_test(Length, Reps, init_vars_uoudg/4)),
        fail.

a_test(4, Length, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UUDG)~n", []),
        format("N - ~d, ~d repetitions~n", [Length, Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                     aiakl_test(Length, Reps, init_vars_uudg/4)),
        fail.

a_test(_, _, _, _).


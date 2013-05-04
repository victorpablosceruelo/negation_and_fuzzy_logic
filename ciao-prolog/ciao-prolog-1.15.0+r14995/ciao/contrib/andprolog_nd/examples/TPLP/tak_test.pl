:- module(tak_test, [t_test/3], []).

:- use_module(library(format)).
:- use_module(library(between)).

:- use_module('./common_bench').
:- use_module(tak_main).

t_test(_, _MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~nExecuting sequential benchmark~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        tak_test(Reps, tak_seq/0), 
        format("~n--------------------------------------------~n", []),
        fail.

t_test(1, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (MEL & UDG)~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                          tak_test(Reps, tak_mel_udg/0)),
        fail.

t_test(2, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UOUDG)~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                          tak_test(Reps, tak_uoudg/0)),
        fail.

t_test(3, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UUDG)~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                          tak_test(Reps, tak_uudg/0)),
        fail.

t_test(_, _, _).


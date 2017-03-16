:- module(wms2_test, [w_test/3], []).

:- use_module(library(format)).
:- use_module(library(between)).

:- use_module('./common_bench').
:- use_module(wms2_main).

w_test(_, _MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~nExecuting sequential benchmark~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        wms2_test(Reps, wms2_seq/0), 
        format("~n--------------------------------------------~n", []),
        fail.

w_test(1, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (MEL)~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                          wms2_test(Reps, wms2_mel/0)),
        fail.

w_test(2, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UDG)~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                          wms2_test(Reps, wms2_udg/0)),
        fail.

w_test(3, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UOUDG)~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                          wms2_test(Reps, wms2_uoudg/0)),
        fail.

w_test(4, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UUDG)~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                          wms2_test(Reps, wms2_uudg/0)),
        fail.

w_test(_, _, _).


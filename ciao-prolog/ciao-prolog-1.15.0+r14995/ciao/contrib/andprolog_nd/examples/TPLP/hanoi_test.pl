:- module(hanoi_test, [h_test/4, h_test_gc/5], []).

:- use_module(library(format)).
:- use_module(library(between)).

:- use_module('./common_bench').
:- use_module(hanoi_main).

h_test(_,N, _MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~nExecuting sequential benchmark~n", []),
        format("N = ~d, ~d repetitions~n", [N, Reps]),
        format("--------------------------------------------~n", []),
        hanoi_test(N, _A, _B, _C, Reps, hanoi_seq/5), 
        format("~n--------------------------------------------~n", []),
        fail.

h_test(1,N, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (MEL)~n", []),
        format("N - ~d, ~d repetitions~n", [N, Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                        hanoi_test(N, _A, _B, _C, Reps, hanoi_par_mel/5)).

h_test(2,N, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UDG)~n", []),
        format("N - ~d, ~d repetitions~n", [N, Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                        hanoi_test(N, _A, _B, _C, Reps, hanoi_par_udg/5)).

h_test(3,N, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UOUDG)~n", []),
        format("N - ~d, ~d repetitions~n", [N, Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                        hanoi_test(N, _A, _B, _C, Reps, hanoi_par_uoudg/5)).

h_test(4,N, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UUDG)~n", []),
        format("N - ~d, ~d repetitions~n", [N, Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                        hanoi_test(N, _A, _B, _C, Reps, hanoi_par_uudg/5)).

h_test(_, _, _, _).

h_test_gc(_, N, _Gran, _MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~nExecuting sequential benchmark~n", []),
        format("N = ~d, ~d repetitions~n", [N, Reps]),
        format("--------------------------------------------~n", []),
        hanoi_test(N, _A, _B, _C, Reps, hanoi_seq/5), 
        format("~n--------------------------------------------~n", []),
        fail.

h_test_gc(1,N, Gran, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (MEL)~n", []),
        format("N - ~d, ~d repetitions~n", [N, Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                 hanoi_test_gc(N, _A, _B, _C, Gran, Reps, hanoi_mel_gc/6)).

h_test_gc(2, N, Gran, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UDG)~n", []),
        format("N - ~d, ~d repetitions~n", [N, Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                 hanoi_test_gc(N, _A, _B, _C, Gran, Reps, hanoi_udg_gc/6)).

h_test_gc(3, N, Gran, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UOUDG)~n", []),
        format("N - ~d, ~d repetitions~n", [N, Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                 hanoi_test_gc(N, _A, _B, _C, Gran, Reps, hanoi_uoudg_gc/6)).

h_test_gc(4, N, Gran, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UUDG)~n", []),
        format("N - ~d, ~d repetitions~n", [N, Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                 hanoi_test_gc(N, _A, _B, _C, Gran, Reps, hanoi_uudg_gc/6)).

h_test_gc(_, _, _, _, _).


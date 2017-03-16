:- module(fib_fun_test, [fib_fun_test/3, fib_fun_test_gc/4], []).

:- use_module(library(format)).
:- use_module(library(between)).

:- use_module('./common_bench').
:- use_module(fib_fun_main).

fib_fun_test(N, _MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~nExecuting sequential benchmark (MEL & UDG & UOUDG)~n", []),
        format("N = ~d, ~d repetitions~n", [N, Reps]),
        format("--------------------------------------------~n", []),
        fibonacci_fun_test(N, Reps, fibonacci_fun_seq_mel_udg_uoudg/2), 
        format("~n--------------------------------------------~n", []),
        fail.

fib_fun_test(N, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UUDG)~n", []),
        format("N - ~d, ~d repetitions~n", [N, Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                        fibonacci_fun_test(N, Reps, fibonacci_fun_uudg/2)).

fib_fun_test(_, _, _).

fib_fun_test_gc(N, _Gran, _MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~nExecuting sequential benchmark (MEL & UDG & UOUDG)~n", []),
        format("N = ~d, ~d repetitions~n", [N, Reps]),
        format("--------------------------------------------~n", []),
        fibonacci_fun_test(N, Reps, fibonacci_fun_seq_mel_udg_uoudg/2), 
        format("~n--------------------------------------------~n", []),
        fail.

fib_fun_test_gc(N, Gran, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UUDG)~n", []),
        format("N - ~d, ~d repetitions~n", [N, Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                         fibonacci_fun_test_gc(N, Gran, Reps, fibonacci_fun_uudg_gc/3)).

fib_fun_test_gc(_, _, _, _).

:- module(fft_test, [f_test/3, f_test_gc/4], []).

:- use_module(library(format)).
:- use_module(library(between)).

:- use_module('./common_bench').
:- use_module(fft_main).

f_test(_, _MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~nExecuting sequential benchmark~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        fft_test(Reps, fft_seq/0), 
        format("~n--------------------------------------------~n", []),
        fail.

f_test(1, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (MEL)~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                          fft_test(Reps, fft_mel/0)),
        fail.

f_test(2, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UDG)~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                          fft_test(Reps, fft_udg/0)),
        fail.

f_test(3, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UOUDG)~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                          fft_test(Reps, fft_uoudg/0)),
        fail.

f_test(4, MaxAgents, Reps):-
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UUDG)~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                          fft_test(Reps, fft_uudg/0)),
        fail.

f_test(_, _, _).

f_test_gc(_, _Gran, _MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~nExecuting sequential benchmark~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        fft_test(Reps, fft_seq/0), 
        format("~n--------------------------------------------~n", []),
        fail.

f_test_gc(1, Gran, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (MEL)~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                         fft_test_gc(Gran, Reps, fft_mel_gc/1)),
        fail.

f_test_gc(2, Gran, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UDG)~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                         fft_test_gc(Gran, Reps, fft_udg_gc/1)),
        fail.

f_test_gc(3, Gran, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UOUDG)~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                         fft_test_gc(Gran, Reps, fft_uoudg_gc/1)),
        fail.

f_test_gc(4, Gran, MaxAgents, Reps):- 
        set_prolog_flag(gc, off),
        format("~n~n~nExecuting parallel benchmark (UUDG)~n", []),
        format("~d repetitions~n", [Reps]),
        format("--------------------------------------------~n", []),
        increasing_agents(1, MaxAgents,
                         fft_test_gc(Gran, Reps, fft_uudg_gc/1)),
        fail.

f_test_gc(_, _, _, _).


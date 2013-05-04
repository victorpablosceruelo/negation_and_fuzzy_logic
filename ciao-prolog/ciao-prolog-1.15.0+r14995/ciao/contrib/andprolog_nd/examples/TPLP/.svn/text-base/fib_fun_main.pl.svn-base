:- module(fib_fun_main,
        [
            fibonacci_fun_test/3,
            fibonacci_fun_test_gc/4
        ],
	[]).


:- use_package(andprolog_nd).
:- use_module('./common_bench').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions without granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fibonacci_fun_test(N, Reps, Name/Arity) :-
        functor(Goal, Name, Arity),
        arg(1, Goal, N),
        bench_rep(Reps, Goal, Name/Arity).

fibonacci_fun_uudg(0, 0).
fibonacci_fun_uudg(1, 1).
fibonacci_fun_uudg(N, F) :-
        N > 1,
        N1 is N - 2,
        fibonacci_fun_uudg(N1, F1) '&!>' H1,
        N2 is N - 1,
        fibonacci_fun_uudg(N2, F2),
	H1 '<&!' ,
        F is F1 + F2.

fibonacci_fun_seq_mel_udg_uoudg(0, 0).
fibonacci_fun_seq_mel_udg_uoudg(1, 1).
fibonacci_fun_seq_mel_udg_uoudg(N, F) :-
        N > 1,
        N1 is N - 1,
        fibonacci_fun_seq_mel_udg_uoudg(N1, F1),
        N2 is N - 2,
        fibonacci_fun_seq_mel_udg_uoudg(N2, F2),
        F is F1 + F2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


fibonacci_fun_test_gc(N, Gran, Reps, Name/Arity) :-
        functor(Goal, Name, Arity),
        arg(1, Goal, N),
        arg(2, Goal, Gran),
        bench_rep(Reps, Goal, Name/Arity).

fibonacci_fun_uudg_gc(0, _, 0).
fibonacci_fun_uudg_gc(1, _, 1).
fibonacci_fun_uudg_gc(N, Level, F) :-
        N > 1,
        (
            N =< Level ->
            fibonacci_fun_seq_mel_udg_uoudg(N, F)
        ;
	    N1 is N - 2,
	    fibonacci_fun_uudg_gc(N1, Level, F1) '&!>' H1,
	    N2 is N - 1,
	    fibonacci_fun_uudg_gc(N2, Level, F2),
	    H1 '<&!' ,
	    F is F1 + F2
        ).


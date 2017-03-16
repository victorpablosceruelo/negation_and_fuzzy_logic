:- module(hanoi_main,
        [
            hanoi_test/6,
            hanoi_test_gc/7
        ],
	[]).


:- use_package(andprolog_nd).
:- use_module(library(lists), [append/3]).
:- use_module('./common_bench').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions without granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hanoi_test(N, A, B, C, Reps, Name/Arity) :-
        functor(Goal, Name, Arity),
        arg(1, Goal, N),
        arg(2, Goal, A),
        arg(3, Goal, B),
        arg(4, Goal, C),
        bench_rep(Reps, Goal, Name/Arity).

hanoi_seq(1,A,_,C,[mv(A,C)]).
hanoi_seq(N,A,B,C,M) :-
	N > 1,
	N1 is N - 1,
	hanoi_seq(N1,A,C,B,M1),
	hanoi_seq(N1,B,A,C,M2),
	append(M1,[mv(A,C)],T),
	append(T,M2,M).

hanoi_par_mel(1,A,_,C,[mv(A,C)]).
hanoi_par_mel(N,A,B,C,M) :-
	N > 1,
	N1 is N - 1,
	hanoi_par_mel(N1,A,C,B,M1),
	hanoi_par_mel(N1,B,A,C,M2) '&!'
	append(M1,[mv(A,C)],T),
	append(T,M2,M).

hanoi_par_udg(1,A,_,C,[mv(A,C)]).
hanoi_par_udg(N,A,B,C,M) :-
	N > 1,
	N1 is N - 1,
	hanoi_par_udg(N1,B,A,C,M2) '&!'
        (
	    hanoi_par_udg(N1,A,C,B,M1),
	    append(M1,[mv(A,C)],T)
	),
	append(T,M2,M).

hanoi_par_uoudg(1,A,_,C,[mv(A,C)]).
hanoi_par_uoudg(N,A,B,C,M) :-
	N > 1,
	N1 is N - 1,
	hanoi_par_uoudg(N1,A,C,B,M1) '&!'
	hanoi_par_uoudg(N1,B,A,C,M2),
	append(M1,[mv(A,C)],T),
	append(T,M2,M).

hanoi_par_uudg(1,A,_,C,[mv(A,C)]).
hanoi_par_uudg(N,A,B,C,M) :-
	N > 1,
	N1 is N - 1,
	hanoi_par_uudg(N1,B,A,C,M2) '&!>' H4,
	hanoi_par_uudg(N1,A,C,B,M1),
	append(M1,[mv(A,C)],T),
	H4 '<&!',
	append(T,M2,M).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


hanoi_test_gc(N, A, B, C, Gran, Reps, Name/Arity) :-
        functor(Goal, Name, Arity),
        arg(1, Goal, N),
        arg(2, Goal, A),
        arg(3, Goal, B),
        arg(4, Goal, C),
        arg(5, Goal, Gran),
        bench_rep(Reps, Goal, Name/Arity).

hanoi_mel_gc(N,A,B,C,GS,M) :-
	(
	    N < GS -> hanoi_seq(N,A,B,C,M)
        ;
	    N1 is N - 1,
	    hanoi_mel_gc(N1,A,C,B,GS,M1),
	    hanoi_mel_gc(N1,B,A,C,GS,M2) '&!'
	    append(M1,[mv(A,C)],T),
	    append(T,M2,M)
	).

hanoi_udg_gc(N,A,B,C,GS,M) :-
	(
	    N < GS -> hanoi_seq(N,A,B,C,M)
        ;
	    N1 is N - 1,
	    hanoi_udg_gc(N1,B,A,C,GS,M2) '&!'
	    (
		hanoi_udg_gc(N1,A,C,B,GS,M1),
		append(M1,[mv(A,C)],T)
	    ),
	    append(T,M2,M)
	).

hanoi_uoudg_gc(N,A,B,C,GS,M) :-
	(
	    N < GS -> hanoi_seq(N,A,B,C,M)
        ;
	    N1 is N - 1,
	    hanoi_uoudg_gc(N1,A,C,B,GS,M1) '&!' hanoi_uoudg_gc(N1,B,A,C,GS,M2),
	    append(M1,[mv(A,C)],T),
	    append(T,M2,M)
	).

hanoi_uudg_gc(N,A,B,C,GS,M) :-
	(
	    N < GS -> hanoi_seq(N,A,B,C,M)
        ;
	    N1 is N - 1,
	    hanoi_uudg_gc(N1,B,A,C,GS,M2) '&!>' H4,
	    hanoi_uudg_gc(N1,A,C,B,GS,M1),
	    append(M1,[mv(A,C)],T),
	    H4 '<&!',
	    append(T,M2,M)
	).


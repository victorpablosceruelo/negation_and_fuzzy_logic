:- module(hanoi_dl,
	[
	    seq/2,
	    par/2,
	    par_nondet/2,
	    data/1
	],
	[andprolog_nd]).

:- use_module(library(lists), [append/3, length/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

size(14).
gc(7).

data(X) :- size(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq(N,X) :- hanoidl_seq(N,1,2,3,X).
par(N,X) :- gc(GC), hanoidl_par_gc(N,1,2,3,GC,X).
par_nondet(N,X) :- gc(GC), hanoidl_par_nondet_gc(N,1,2,3,GC,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions without granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hanoidl_seq(N, A, B, C, R):-
        hanoidl_seq_(N, A, B, C, R-[]).

hanoidl_seq_(1, A, _, C, [mv(A,C)|X]-X) :- !.
hanoidl_seq_(N,A,B,C,M) :-
        N > 1,
        N1 is N - 1,
        hanoidl_seq_(N1,A,C,B,M1),
        hanoidl_seq_(N1,B,A,C,M2),
        append_dl(M1,[mv(A,C)|X]-X,T),
        append_dl(T,M2,M).

hanoidl_par(N, A, B, C, R):-
        hanoidl_par_(N, A, B, C, R-[]).

hanoidl_par_(1, A, _, C, [mv(A,C)|X]-X) :- !.
hanoidl_par_(N,A,B,C,M) :-
        N > 1,
        N1 is N - 1,
        hanoidl_par_(N1,A,C,B,M1) '&!'
        hanoidl_par_(N1,B,A,C,M2),
        append_dl(M1,[mv(A,C)|X]-X,T),
        append_dl(T,M2,M).

hanoidl_par_nondet(N, A, B, C, R):-
        hanoidl_par_nondet_(N, A, B, C, R-[]).

hanoidl_par_nondet_(1, A, _, C, [mv(A,C)|X]-X).
hanoidl_par_nondet_(N,A,B,C,M) :-
        N > 1,
        N1 is N - 1,
        hanoidl_par_nondet_(N1,A,C,B,M1) &
        hanoidl_par_nondet_(N1,B,A,C,M2),
        append_dl(M1,[mv(A,C)|X]-X,T),
        append_dl(T,M2,M).

append_dl(A-B, B-C, A-C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions with granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hanoidl_par_gc(N, A, B, C, GS, R):-
        hanoidl_par_gc_(N, A, B, C, GS, R-[]).

hanoidl_par_gc_(1, A, _, C, _, [mv(A,C)|X]-X) :- !.
hanoidl_par_gc_(N,A,B,C,GS,M) :-
	N > 1,
	N1 is N - 1,
	(
	    N < GS ->
	    hanoidl_par_gc_(N1,A,C,B,GS,M1),
	    hanoidl_par_gc_(N1,B,A,C,GS,M2)
        ;
	    hanoidl_par_gc_(N1,A,C,B,GS,M1) '&!'
            hanoidl_par_gc_(N1,B,A,C,GS,M2)
	),
	append_dl(M1,[mv(A,C)|X]-X,T),
	append_dl(T,M2,M).

hanoidl_par_nondet_gc(N, A, B, C, GS, R):-
        hanoidl_par_nondet_gc_(N, A, B, C, GS, R-[]).

hanoidl_par_nondet_gc_(1, A, _, C, _, [mv(A,C)|X]-X).
hanoidl_par_nondet_gc_(N,A,B,C,GS,M) :-
	N > 1,
	N1 is N - 1,
	(
	    N < GS ->
	    hanoidl_par_nondet_gc_(N1,A,C,B,GS,M1),
	    hanoidl_par_nondet_gc_(N1,B,A,C,GS,M2)
        ;
	    hanoidl_par_nondet_gc_(N1,A,C,B,GS,M1) &
            hanoidl_par_nondet_gc_(N1,B,A,C,GS,M2)
	),
	append_dl(M1,[mv(A,C)|X]-X,T),
	append_dl(T,M2,M).


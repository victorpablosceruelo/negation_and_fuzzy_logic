:- module(hanoi,
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

seq(N,X) :- hanoi_seq(N,1,2,3,X).
par(N,X) :- gc(GC), hanoi_par_gc(N,1,2,3,GC,X).
par_nondet(N,X) :- gc(GC), hanoi_par_nondet_gc(N,1,2,3,GC,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions without granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hanoi_seq(1,A,_,C,[mv(A,C)]) :- !.
hanoi_seq(N,A,B,C,M) :-
	N > 1,
	N1 is N - 1,
	hanoi_seq(N1,A,C,B,M1),
	hanoi_seq(N1,B,A,C,M2),
	append(M1,[mv(A,C)],T),
	append(T,M2,M).

hanoi_par(1,A,_,C,[mv(A,C)]) :- !.
hanoi_par(N,A,B,C,M) :-
	N > 1,
	N1 is N - 1,
	hanoi_par(N1,B,A,C,M2) '&!>' H4,
	hanoi_par(N1,A,C,B,M1),
	append(M1,[mv(A,C)],T),
	H4 '<&!',
	append(T,M2,M).

hanoi_par_nondet(1,A,_,C,[mv(A,C)]).
hanoi_par_nondet(N,A,B,C,M) :-
	N > 1,
	N1 is N - 1,
	hanoi_par(N1,B,A,C,M2) &> H4,
	hanoi_par(N1,A,C,B,M1),
	append(M1,[mv(A,C)],T),
	H4 <& ,
	append(T,M2,M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions with granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hanoi_par_gc(N,A,B,C,GS,M) :-
	(
	    N < GS -> hanoi_seq(N,A,B,C,M)
        ;
	    N1 is N - 1,
	    hanoi_par_gc(N1,B,A,C,GS,M2) '&!>' H4,
	    hanoi_par_gc(N1,A,C,B,GS,M1),
	    append(M1,[mv(A,C)],T),
	    H4 '<&!',
	    append(T,M2,M)
	).

hanoi_par_nondet_gc(N,A,B,C,GS,M) :-
	(
	    N < GS -> hanoi_seq(N,A,B,C,M)
        ;
	    N1 is N - 1,
	    hanoi_par_nondet_gc(N1,B,A,C,GS,M2) &> H4,
	    hanoi_par_nondet_gc(N1,A,C,B,GS,M1),
	    append(M1,[mv(A,C)],T),
	    H4 <& ,
	    append(T,M2,M)
	).


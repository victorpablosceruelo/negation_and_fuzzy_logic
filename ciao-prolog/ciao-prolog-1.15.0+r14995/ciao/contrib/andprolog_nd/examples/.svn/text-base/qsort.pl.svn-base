:- module(qsort,
	[
	    seq/2,
	    par/2,
	    par_nondet/2,
	    data/1
	],
	[andprolog_nd]).

:- use_module(library(lists),[append/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

size(10000).
gc(300).

data(X) :- size(N), gen_list(N,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq(L,X) :- qsort_seq(L,X).
par(L,X) :- size(N), gc(GC), qsort_par_gc(L,N,GC,X).
par_nondet(L,X) :- size(N), gc(GC), qsort_par_nondet_gc(L,N,GC,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qsort_seq([], []) :- !.
qsort_seq([X|L], R) :-
	partition(L, X, L1, L2),
	qsort_seq(L2, R2), 
        qsort_seq(L1, R1), 
        append(R1, [X|R2], R).

qsort_par_det([], []) :- !.
qsort_par_det([X|L], R) :-
	partition(L, X, L1, L2),
	qsort_par_det(L2, R2) '&!'
        qsort_par_det(L1, R1), 
        append(R1, [X|R2], R).

qsort_par_nondet([], []) :- !.
qsort_par_nondet([X|L], R) :-
	partition(L, X, L1, L2),
	qsort_par_nondet(L2, R2) &
        qsort_par_nondet(L1, R1), 
        append(R1, [X|R2], R).

partition([], _B, [], []) :- !.
partition([E|R], C, [E|Left1], Right) :- 
	E < C, !,
	partition(R, C, Left1, Right).
partition([E|R], C, Left, [E|Right1]) :-
	E >= C,
	partition(R, C, Left, Right1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions with granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qsort_par_gc([], _InputLen, _GranLevel, []) :- !.
qsort_par_gc([X|L], InputLen, GLev, R) :-
        (
            InputLen > GLev ->
            partition_gc(L, X, L1, L2, 0, N1),
            N2 is InputLen - N1,
            qsort_par_gc(L1, N1, GLev, R1) '&!'
	    qsort_par_gc(L2, N2, GLev, R2),
            append(R1, [X|R2], R)
        ;
            qsort_seq([X|L], R)
        ).

qsort_par_nondet_gc([], _InputLen, _GranLevel, []) :- !.
qsort_par_nondet_gc([X|L], InputLen, GLev, R) :-
        (
            InputLen > GLev ->
            partition_gc(L, X, L1, L2, 0, N1),
            N2 is InputLen - N1,
            qsort_par_nondet_gc(L1, N1, GLev, R1) &
	    qsort_par_nondet_gc(L2, N2, GLev, R2),
            append(R1, [X|R2], R)
        ;
            qsort_seq([X|L], R)
        ).

partition_gc([], _Piv, [], [], LenL1, LenL1) :- !.
partition_gc([E|R], C, [E|Left1], Right, L1LenIn, L1LenOut) :-
        E<C, !,
        L1LenMid is L1LenIn + 1,
        partition_gc(R, C, Left1, Right, L1LenMid, L1LenOut).
partition_gc([E|R], C, Left, [E|Right1], L1lenIn, L1LenOut) :-
        E>=C,
        partition_gc(R, C, Left, Right1, L1lenIn, L1LenOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_list(0, []) :- !.
gen_list(M, [V|Ns]):- 
        M > 0,
        M1 is M - 1,
        V is M*M*M mod 7919,
        gen_list(M1, Ns).

:- module(fibo,
	[
	    seq/2,
	    par/2,
	    par_nondet/2,
	    data/1
	],
	[andprolog_nd]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

size(22).
gc(12).

data(X) :- size(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq(N,X) :- fib_seq(N,X).
par(N,X) :- gc(GC), fib_par_gc(N,GC,X).
par_nondet(N,X) :- gc(GC), fib_par_nondet_gc(N,GC,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fib_seq(0, 0) :- !.
fib_seq(1, 1) :- !.
fib_seq(N, F) :-
        N1 is N - 1,
        N2 is N - 2,
        fib_seq(N2, F2),
        fib_seq(N1, F1),
        F is F1 + F2.

fib_par(0, 0) :- !.
fib_par(1, 1) :- !.
fib_par(N, F) :-
        N1 is N - 1,
        N2 is N - 2,
        fib_par(N2, F2) '&!'
        fib_par(N1, F1),
        F is F1 + F2.

fib_par_gc(0, _, 0) :- !.
fib_par_gc(1, _, 1) :- !.
fib_par_gc(N, Level, F) :-
        (
            N =< Level ->
            fib_seq(N, F)
        ;
            N1 is N - 1,
            N2 is N - 2,
            fib_par_gc(N2, Level, F2) '&!' fib_par_gc(N1, Level, F1),
            F is F1 + F2
        ).

fib_par_nondet(0, 0) :- !.
fib_par_nondet(1, 1) :- !.
fib_par_nondet(N, F) :-
        N1 is N - 1,
        N2 is N - 2,
        fib_par_nondet(N2, F2) &
        fib_par_nondet(N1, F1),
        F is F1 + F2.

fib_par_nondet_gc(0, _, 0) :- !.
fib_par_nondet_gc(1, _, 1) :- !.
fib_par_nondet_gc(N, Level, F) :-
        (
            N =< Level ->
            fib_seq(N, F)
        ;
            N1 is N - 1,
            N2 is N - 2,
	    fib_par_nondet_gc(N2, Level, F2) &
            fib_par_nondet_gc(N1, Level, F1),
            F is F1 + F2
        ).
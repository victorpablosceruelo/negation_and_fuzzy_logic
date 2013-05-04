:- module(
	fibo,
	[
	    testN/3,         % nondeterminism
	    testD/3,         % determinism
	    testS/3,         % sequential
	    main/0
	],
	['andprolog/andprolog_assrt']
	 ).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(system)).


fibo_parN(0, 0).
fibo_parN(1, 1).
fibo_parN(N, F) :-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fibo_parN(N1, F1) & fibo_parN(N2, F2),
        F is F1 + F2,
	!.

fibo_parD(0, 0).
fibo_parD(1, 1).
fibo_parD(N, F) :-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fibo_parN(N1, F1) '&!' fibo_parN(N2, F2),
        F is F1 + F2,
	!.

fibo_seq(0, 0).
fibo_seq(1, 1).
fibo_seq(N, F) :-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fibo_seq(N1, F1),
        fibo_seq(N2, F2),
        F is F1 + F2,
        !.

testN(N, F, Time) :-
        statistics(walltime, _),
        fibo_parN(N, F),
        statistics(walltime, [_,Time]).

testD(N, F, Time) :-
        statistics(walltime, _),
        fibo_parD(N, F),
        statistics(walltime, [_,Time]).

testS(N, F, Time) :-
        statistics(walltime, _),
        fibo_seq(N, F),
        statistics(walltime, [_,Time]).

main :-
	testD(25, J, K),
	nl,
	display('FIBO: '), display(J), nl,
	display('TIME: '), display(K), nl.



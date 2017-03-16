:- module(fib, [fib/2, fibfd_new/2, fibfd_ordered_new/2], [clpfd]).

:- use_module(library(clpfd(fd_constraints))).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format)).
:- use_module(library(lists)).

fib(0, 0).
fib(1, 1).
fib(N, F):-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fib(N1, F1),
        fib(N2, F2),
        F is F1 + F2.

fibfd_new(0, 0).
fibfd_new(1, 1).
fibfd_new(N, F):-
        't<b'(1,N),
	'a-t=c'(N, 1, N1),
	'a-t=c'(N, 2, N2),
        fibfd_new(N1, F1),
        fibfd_new(N2, F2),
        'a+b=c'(F1, F2, F).
%        labeling([F]).

fibfd_ordered_new(0, 0).
fibfd_ordered_new(1, 1).
fibfd_ordered_new(N, F):-
        't<b'(1,N),
	'a-t=c'(N, 1, N1),
	'a-t=c'(N, 2, N2),
        'a+b=c'(F1, F2, F),
        fibfd_ordered_new(N1, F1),
        fibfd_ordered_new(N2, F2).

%        labeling([F]).

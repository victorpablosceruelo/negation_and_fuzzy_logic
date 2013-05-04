:- module(metacall_test, [test1/0, test2/0, test3/0]).

:- use_module(library(debugger), [call_in_module/2]).
:- use_module(library(format)).

:- use_package(hiord).

test1 :-
	garbage_collect,
	statistics(runtime,_),
	repeat1(800000),
	statistics(runtime,[_, Time]),
	format("Used ~d milliseconds~n", Time).

test2 :-
	garbage_collect,
	statistics(runtime,_),
	repeat2(800000),
	statistics(runtime,[_, Time]),
	format("Used ~d milliseconds~n", Time).
test3 :-
	garbage_collect,
	statistics(runtime,_),
	repeat3(800000),
	statistics(runtime,[_, Time]),
	format("Used ~d milliseconds~n", Time).

repeat1(0) :- !.
repeat1(N) :-
	A = 1,
	C = 3,
	call_in_module(metacall_test, do(A, B, C)),
	D = B,
	N1 is N - 1,
	repeat1(N1).  

repeat2(0) :- !.
repeat2(N) :-
	A = 1,
	C = 3,
	do(A, B, C),
	D = B,
	N1 is N - 1,
	repeat2(N1).

repeat3(0) :- !.
repeat3(N) :-
	A = 1,
	C = 3,
	wrapper(do, A, B, C),
	D = B,
	N1 is N - 1,
	repeat3(N1).

:- meta_predicate wrapper(pred(3), A, B, C).
wrapper(X, A, B, C) :-
	X(A, B, C).

do(A, B, C) :-
	N is 100+100,
	N1 is N / 10,
	B = N1,
	R is A + C.

% ?- test1, test2, test3.
% Used 360 milliseconds
% Used 112 milliseconds
% Used 480 milliseconds

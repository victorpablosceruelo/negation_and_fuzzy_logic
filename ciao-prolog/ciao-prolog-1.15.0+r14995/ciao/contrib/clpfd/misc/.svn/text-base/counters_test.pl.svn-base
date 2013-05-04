:- module(counters_test, [test/0]).

% A test to compare the performance of clpfd_stats against
% library(counters).
%
% NOTE: Make sure to enable ":- compilation_fact(collect_statistics)."
%       in clpfd_stats!

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format)).

:- use_module(library(counters)).
:- use_module(library(clpfd(clpfd_stats))).

test1(0) :- !.
test1(N) :-
	inc_stat(chain_calls),
	N1 is N - 1,
	test1(N1).
	
test2(0) :- !.
test2(N) :-
	inccounter(test,_),
	N1 is N - 1,
	test2(N1).

test :-
	statistics(runtime, _),
	test1(100000),
	statistics(runtime, [_,Time1]),
	setcounter(test, 0),
	test2(100000),
	statistics(runtime, [_,Time2]),
	format("Test results ~p versus ~p~n", [Time1,Time2]).
	
	

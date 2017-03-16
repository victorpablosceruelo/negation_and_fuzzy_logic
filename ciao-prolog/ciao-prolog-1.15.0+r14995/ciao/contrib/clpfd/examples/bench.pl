:- module(_, _, [clpfd]).

:- use_module(library(format)).
:- use_module(library(prolog_sys)).
:- use_module(library(aggregates)).

:- data counter_fact/1.
counter(I):-
	retract_fact(counter_fact(I)), !,
	J is I + 1,
	assertz_fact(counter_fact(J)).
counter(0) :-
	assertz_fact(counter_fact(1)).

main:-
	findall(Desc-Goal, test(Desc, Goal), L), !,
	member(Desc-Goal, L),
	call((statistics(runtime, _),Goal, !, statistics(runtime, [_, Time]))),
	counter(I),
	format("test ~w: ~3d\t(~s)\n", [I, Time, Desc]),
	fail.

:- discontiguous(test/2).

:- use_module(.(queens), [queens/4]).

test("queens, n=16, lab=step, diff=clpfd", queens:queens(16, _, [], clpfd)).
test("queens, n=16, lab=step, diff=fd", queens:queens(16, _, [], fd)).
test("queens, n=16, lab=step, diff=idx", queens:queens(16, _, [], idx)).
test("queens, n=16, lab=step, diff=kernel", queens:queens(16, _, [], kernel)).

test("queens, n=90, lab=ff,  diff=clpfd", queens:queens(90, _, [ff], clpfd)).
test("queens, n=90, lab=ff,  diff=fd", queens:queens(90, _, [ff], fd)).
test("queens, n=90, lab=ff,  diff=idx", queens:queens(90, _, [ff], idx)).
test("queens, n=90, lab=ff,  diff=kernel", queens(90, _, [ff], kernel)).

:- use_module(.(bridge), [bridge/2]).

test("bridge", bridge(_, _)).

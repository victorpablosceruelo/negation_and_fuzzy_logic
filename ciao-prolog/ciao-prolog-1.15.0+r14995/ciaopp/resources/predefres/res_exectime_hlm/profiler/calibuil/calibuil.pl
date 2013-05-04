:- module(calibuil, [calibrate_builtins/2,
		calibrate_builtins_arit/2,
		calibrate_builtins_fake/1,
		builtins_pred_dump/0, builtins_arit_dump/0], [assertions]).

:- use_module(library(format)).
:- use_module(resources(resources_db), [arith_costs/2]).

:- use_module(profcost(profdb)).
:- reexport(profcost(calibuil(calibuil_bench))).

:- doc(module, "This module calculates the time spent for each
   builtin that have a constant time.").

min_bench_test_pred(N, Operator, TimeOption, Time) :-
	min_bench_test_pred_(N, 0, Operator, TimeOption, 0.Inf, Time).

min_bench_test_pred_(N, N, _,        _,          Time,  Time) :- !.
min_bench_test_pred_(N, I, Operator, TimeOption, Time0, Time) :-
	I1 is I + 1,
	bench_test_pred(Operator, TimeOption, Time1),
	(Time1 < Time0 -> Time2 = Time1 ; Time2 = Time0),
	min_bench_test_pred_(N, I1, Operator, TimeOption, Time2, Time).

sum_bench_test_pred_(N, N, _,        _,          (S,  SS),  (S, SS)) :- !.
sum_bench_test_pred_(N, I, Operator, TimeOption, (S0, SS0), (S, SS)) :-
	I1 is I + 1,
	min_bench_test_pred(4, Operator, TimeOption, Time),
	S2 is S0 + Time,
	SS2 is SS0 + Time*Time,
	sum_bench_test_pred_(N, I1, Operator, TimeOption, (S2, SS2), (S, SS)).

avg_bench_test_pred(N, Operator, TimeOption, [Time, Variance]) :-
	sum_bench_test_pred_(N, 0, Operator, TimeOption, (0, 0), (S, SS)),
	Time is S/N,
	Variance is SS/N - Time*Time.

init_bench_test_pred(N, TimeOption) :-
	avg_bench_test_pred(N, Operator, TimeOption, Time),
	assertz_fact(builtin_time_pred(Operator, Time)),
	fail.
init_bench_test_pred(_N, _TimeOption).

min_bench_test_arit(N, Operator, TimeOption, Time) :-
	min_bench_test_arit_(N, 0, Operator, TimeOption, 0.Inf, Time).

min_bench_test_arit_(N, N, _,        _,          Time,  Time) :- !.
min_bench_test_arit_(N, I, Operator, TimeOption, Time0, Time) :-
	I1 is I + 1,
	bench_test_arit(Operator, TimeOption, Time1),
	(Time1 < Time0 -> Time2 = Time1 ; Time2 = Time0),
	min_bench_test_arit_(N, I1, Operator, TimeOption, Time2, Time).

sum_bench_test_arit_(N, N, _,        _,          (S,  SS),  (S, SS)) :- !.
sum_bench_test_arit_(N, I, Operator, TimeOption, (S0, SS0), (S, SS)) :-
	I1 is I + 1,
	min_bench_test_arit(4, Operator, TimeOption, Time),
	S2 is S0 + Time,
	SS2 is SS0 + Time*Time,
	sum_bench_test_arit_(N, I1, Operator, TimeOption, (S2, SS2), (S, SS)).

avg_bench_test_arit(N, Operator, TimeOption, [Time, Variance]) :-
	sum_bench_test_arit_(N, 0, Operator, TimeOption, (0, 0), (S, SS)),
	Time is S/N,
	Variance is SS/N - Time*Time.

init_bench_test_arit(N, TimeOption) :-
	avg_bench_test_arit(N, Operator, TimeOption, Time),
	assertz_fact(arith_costs(Operator, Time)),
	fail.
init_bench_test_arit(_N, _TimeOption).


calibrate_builtins_pred_fake(W) :-
	retractall_fact(builtin_time_pred(_, _)),
	assertz_fact(builtin_time_pred(_F/_N, W)).

calibrate_builtins_arit_fake(W) :-
	retractall_fact(arith_costs(_, _)),
	assertz_fact(arith_costs(_F/_N, W)).

calibrate_builtins(N, TimeOption) :-
	calibrate_builtins_pred(N, TimeOption),
	calibrate_builtins_arit(N, TimeOption).

calibrate_builtins_fake(W) :-
	calibrate_builtins_pred_fake(W),
	calibrate_builtins_arit_fake(W).

calibrate_builtins_pred(N, TimeOption) :-
	retractall_fact(builtin_time_pred(_, _)),
	init_bench_test_pred(N, TimeOption).

calibrate_builtins_arit(N, TimeOption) :-
% init some bultins internally used by is/2
	retractall_fact(arith_costs(_, _)),
	init_bench_test_arit(N, TimeOption).

builtins_pred_dump :-
	display('\nBuiltin\tTime (ms)\n'),
	display('======= ===================\n'),
	(
	    builtin_time_pred(Operator, [Time, Variance]),
	    Error is sqrt(Variance),
	    RError is Error / Time * 100,
	    format("~w\t~w (~4f %)\n", [Operator, Time, RError]),
	    fail
	;
	    true
	).

builtins_arit_dump :-
	display('\nArithmetic operator\tTime (ms)\n'),
	display('======= ===================\n'),
	(
	    arith_costs(Operator, [Time, Variance]),
	    Error is sqrt(Variance),
	    RError is Error / Time * 100,
	    format("~w\t~w (~4f %)\n", [Operator, Time, RError]),
	    fail
	;
	    true
	).

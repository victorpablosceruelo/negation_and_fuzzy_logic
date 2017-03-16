:- module(_, [builtin_def/3], [assertions]).

repeat(1, B, B).
repeat(N, B, (B, RB)) :-
	N1 is N - 1,
	repeat(N1, B, RB).

builtin_def(( bench_buil(Type, BuiltinName, BenchPred, Repetitions, Start)
		:- Body ), Clauses, _M) :-
	repeat(Repetitions, Body, RBody),
	(
	    Type == pred ->
	    Clauses = [(calibrated_builtin_pred(BuiltinName))|Tail0],
	    Head = bench_test_pred(BuiltinName, TimeOption, AverageTime)
	;
	    Type == arit ->
	    Clauses = [(calibrated_builtin_arit(BuiltinName))|Tail0],
	    Head = bench_test_arit(BuiltinName, TimeOption, AverageTime)
	),
	Tail0 = [
	    (
		Head :-
		measure_0(TimeOption, BenchPred, T),
		AverageTime is T/Repetitions
	    )|Tail],
	(
	    var(Start) ->
	    Tail = [(BenchPred :- RBody)]
	;
	    Tail = [(BenchPred :- Start, RBody)]
	).

:- module(dec_basic_res, [legal_measure_symbol/3], [assertions]).

:- use_module(resources(top_res(error_res)), [error_message/3]).

:- test legal_measure_symbol(A, B, C) : ( A = [void, int, length,
		size, depth([1, 2, 3])], B = 5, C = true )
# "Test of all supported measures.".

legal_measure_symbol([], 0, _) :- !.
legal_measure_symbol([], N, Clause) :-
	N =\= 0,
	!,
	error_message(measure2, _, Clause),
	fail.
legal_measure_symbol([_|_], 0, Clause) :-
	!,
	error_message(measure2, _, Clause),
	fail.
legal_measure_symbol([void|Measure], N, Clause) :-
	N > 0,
	!,
	N1 is N-1,
	legal_measure_symbol(Measure, N1, Clause).
legal_measure_symbol([int|Measure], N, Clause) :-
	N > 0,
	!,
	N1 is N-1,
	legal_measure_symbol(Measure, N1, Clause).
legal_measure_symbol([length|Measure], N, Clause) :-
	N > 0,
	!,
	N1 is N-1,
	legal_measure_symbol(Measure, N1, Clause).
legal_measure_symbol([size|Measure], N, Clause) :-
	N > 0,
	!,
	N1 is N-1,
	legal_measure_symbol(Measure, N1, Clause).
legal_measure_symbol([depth([_|_])|Measure], N, Clause) :-
	N > 0,
	!,
	N1 is N-1,
	legal_measure_symbol(Measure, N1, Clause).
legal_measure_symbol([M|_], _, Clause) :-
	M \== void,
	M \== int,
	M \== length,
	M \== size,
	( M = depth([_|_]) ->
	    fail ;
	    error_message(measure3, M, Clause) ),
	fail.

%   qsort
%
%   David H. D. Warren
%
%   quicksort a list of 50 integers

#include "../mtsys_common.pl"

#if defined(MERCURY)

:- type benchmark_data == list(int).
:- type benchmark_result == list(int).

:- pred dummy_result(list(int)).
:- mode dummy_result(out) is det.
dummy_result([]).

:- pred benchmark_data(string, int, list(int)).
:- mode benchmark_data(out, out, out) is det.
benchmark_data("qsort", 10000, [27,74,17,33,94,18,46,83,65,2,32,53,28,85,99,47,28,82,6,11,55,29,39,81, 90,37,10,0,66,51,7,21,85,27,31,63,75,4,95,99,11,28,61,74,18, 92,40,53,59,8]).

:- pred benchmark(int, list(int), list(int)).
:- mode benchmark(in, in, out) is det.
benchmark(_N, Data, Out) :-
	qsort(Data, Out, []).

:- pred qsort(list(int), list(int), list(int)).
:- mode qsort(in, out, in) is det.
qsort([], R, R).
qsort([X|L], R, R0) :-	
	partition(L, X, L1, L2),
	qsort(L2, R1, R0),
	qsort(L1, R, [X|R1]).

:- pred partition(list(int), int, list(int), list(int)).
:- mode partition(in, in, out, out) is det.
partition([],_,[],[]).
partition([X|L],Y,L10,L20) :-
	( X =< Y ->
	    L10 = [X|L1],
	    L20 = L2
	; L10 = L1,
	  L20 = [X|L2]
	),
	partition(L,Y,L1,L2).

#else

#if defined(CIAO3)
:- include(engine(spec_arithmetic)).
#endif

#if defined(CIAO3)
:- '$props'(benchmark_data/3, [impnat=bytecode]).
#endif
benchmark_data(qsort, 10000, [27,74,17,33,94,18,46,83,65,2,32,53,28,85,99,47,28,82,6,11,55,29,39,81, 90,37,10,0,66,51,7,21,85,27,31,63,75,4,95,99,11,28,61,74,18, 92,40,53,59,8]).

benchmark(Data, Out) :-
	qsort(Data, Out, []).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(qsort/3, sht, [(atomic([]) ; list), var, (atomic([]) ; list)]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
#endif
#if (OPT_MASK & OPT_ARGMEMS)
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(qsort/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(qsort/3, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(qsort/3, [should_trim_frame=no]).
#endif
qsort([], R, R) :- !.
qsort([X|L], R, R0) :-	
	partition(L, X, L1, L2),
	qsort(L2, R1, R0),
	qsort(L1, R, [X|R1]).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(partition/4, sht, [(atomic([]) ; list), int, var, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
#endif
#if (OPT_MASK & OPT_ARGMEMS)
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(partition/4, [argderefs=[true,true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(partition/4, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(partition/4, [should_trim_frame=no]).
#endif
partition([],_,[],[]) :- !.
partition([X|L],Y,[X|L1],L2) :-
#if (OPT_MASK & OPT_TYPES)
	'$trust_type'(X, smallint),
	'$trust_type'(Y, smallint),
#endif
	X =< Y, !,
	partition(L,Y,L1,L2).
partition([X|L],Y,L1,[X|L2]) :-
	partition(L,Y,L1,L2).

#endif

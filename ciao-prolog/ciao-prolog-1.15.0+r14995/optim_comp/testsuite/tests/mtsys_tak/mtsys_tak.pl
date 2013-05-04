#include "../mtsys_common.pl"

#if defined(MERCURY)

:- type benchmark_data ---> triple(int, int, int).
:- type benchmark_result == int.

:- pred dummy_result(benchmark_result).
:- mode dummy_result(out) is det.
dummy_result(0).

:- pred benchmark_data(string, int, benchmark_data).
:- mode benchmark_data(out, out, out) is det.
benchmark_data("tak", 100, triple(18, 12, 6)).

:- pred benchmark(int, benchmark_data, benchmark_result).
:- mode benchmark(in, in, out) is det.
benchmark(_N, triple(X, Y, Z), Out) :-
        tak(X, Y, Z , Out).

:- pred tak(int, int, int, int).
:- mode tak(in, in, in, out) is det.

tak(X, Y, Z, A) :-
	(X =< Y ->
		Z = A
	;
		X1 is X - 1,
		tak(X1, Y, Z, A1),
		Y1 is Y - 1,
		tak(Y1, Z, X, A2),
		Z1 is Z - 1,
		tak(Z1, X, Y, A3),
		tak(A1, A2, A3, A)
	).

#else

#if defined(CIAO3)
:- include(engine(spec_arithmetic)).
#endif

benchmark_data(tak, 100, triple(18, 12, 6)).

benchmark(triple(X, Y, Z), Out) :-
	tak(X, Y, Z, Out).

#if (OPT_MASK & OPT_TYPES)
:- '$trust_entry'(tak/4, sht, [smallint, smallint, smallint, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(tak/4, [argmodes=[in,in,in,out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(tak/4, [argmems=[cvar,cvar,cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
:- '$props'(tak/4, [argderefs=[true,true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(tak/4, [imp=det]).
% :- '$props'(tak/4, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
% :- '$props'(tak/4, [should_trim_frame=no]).
#endif

tak(X,Y,Z,A) :-
	X =< Y, !,
	Z = A.
tak(X,Y,Z,A) :-
	% X > Y,
	X1 is X - 1,
	tak(X1,Y,Z,A1),
	Y1 is Y - 1,
	tak(Y1,Z,X,A2),
	Z1 is Z - 1,
	tak(Z1,X,Y,A3),
	tak(A1,A2,A3,A).
#endif

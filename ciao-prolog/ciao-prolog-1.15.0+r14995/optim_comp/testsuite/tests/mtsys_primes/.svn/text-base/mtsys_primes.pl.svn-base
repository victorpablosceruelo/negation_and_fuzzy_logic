#include "../mtsys_common.pl"

#if defined(MERCURY)

:- type benchmark_data == int.
:- type benchmark_result == list(int).

:- pred dummy_result(benchmark_result).
:- mode dummy_result(out) is det.
dummy_result([]).

:- pred benchmark_data(string, int, benchmark_data).
:- mode benchmark_data(out, out, out) is det.
benchmark_data("primes", 10000, 98).

:- pred benchmark(int, benchmark_data, benchmark_result).
:- mode benchmark(in, in, out) is det.
benchmark(_N, Data, Out) :-
	primes(Data, Out).

:- pred primes(int, list(int)).
:- mode primes(in, out) is det.
primes(Limit, Ps) :-
	integers(2, Limit, Is),
	sift(Is, Ps).

:- pred integers(int, int, list(int)).
:- mode integers(in, in, out) is det.
integers(Low, High, Rest0) :- 
	( Low =< High ->
	    M is Low + 1,
	    Rest0 = [Low | Rest],
	    integers(M, High, Rest)
	; Rest0 = []
	).

:- pred sift(list(int), list(int)).
:- mode sift(in, out) is det.
sift([], []).
sift([I | Is], [I | Ps]) :-
	remove(Is, I, New),
	sift(New, Ps).

:- pred remove(list(int), int, list(int)).
:- mode remove(in, in, out) is det.
remove([], _, []).
remove([I | Is], P, Nis0) :-
	IModP is I mod P,
	( IModP \= 0 ->
	    Nis0 = [I | Nis],
	    remove(Is, P, Nis)
	; Nis0 = Nis,
	  remove(Is, P, Nis)
	).

#else

#if defined(CIAO3)
:- include(engine(spec_arithmetic)).
#endif

benchmark_data(primes, 10000, 98).

benchmark(Data, Out) :-
	primes(Data, Out).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(primes/2, sht, [int, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(primes/2, [argmodes=[in,in]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(primes/2, [argderefs=[true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(primes/2, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
% :- '$props'(primes/2, [should_trim_frame=no]).
#endif
primes(Limit, Ps) :-
	integers(2, Limit, Is),
	sift(Is, Ps).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(integers/3, sht, [int, int, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(integers/3, [argmodes=[in,in,in]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(integers/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(integers/3, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
% :- '$props'(integers/3, [should_trim_frame=no]).
#endif
integers(Low, High, [Low | Rest]) :- 
#if (OPT_MASK & OPT_TYPES)
	'$trust_type'(Low, smallint),
	'$trust_type'(High, smallint),
#endif
	Low =< High, !,
	M is Low + 1,
	integers(M, High, Rest).
integers(_,_,[]).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(sift/2, sht, [(list ; atomic([])), var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(sift/2, [argmodes=[in,in]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(sift/2, [argderefs=[true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(sift/2, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
% :- '$props'(sift/2, [should_trim_frame=no]).
#endif
sift([], []) :- !.
sift([I | Is], [I | Ps]) :-
	remove(Is, I, New),
	sift(New, Ps).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(remove/3, sht, [(list ; atomic([])), int, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(remove/3, [argmodes=[in,in,in]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(remove/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(remove/3, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
% :- '$props'(remove/3, [should_trim_frame=no]).
#endif
remove([], _, []) :- !.
remove([I | Is], P, Nis0) :-
#if (OPT_MASK & OPT_TYPES)
	'$trust_type'(I, smallint),
	'$trust_type'(P, smallint),
#endif
	IModP is I mod P,
#if (OPT_MASK & OPT_TYPES)
	'$trust_type'(IModP, smallint),
#endif
	IModP =\= 0, !,
	Nis0 = [I | Nis],
	remove(Is, P, Nis).
remove([_I | Is], P, Nis) :-
	remove(Is, P, Nis).

#endif

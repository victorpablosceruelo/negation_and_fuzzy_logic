#include "../mtsys_common.pl"

benchmark_data(factorial, 20000, 100).

benchmark(X, Out) :-
	fac(X, Out).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(fac/2, sht, [smallint, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(fac/2, [argmodes=[in,out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(fac/2, [argmems=[cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(fac/2, [argderefs=[true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(fac/2, [imp=det]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(fac/2, [should_trim_frame=no]).
#endif
fac(0, 1) :- !.
fac(X, Y) :-
%	X > 0,
	X0 is X - 1,
	fac(X0, Y0),
	Y is X * Y0.

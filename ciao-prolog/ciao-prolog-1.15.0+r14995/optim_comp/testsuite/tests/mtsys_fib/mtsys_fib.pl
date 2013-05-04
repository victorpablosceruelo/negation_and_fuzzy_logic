#include "../mtsys_common.pl"

benchmark_data(fib, 1000, 1000).

benchmark(Data, Out) :-
	fib(Data, Out).
%	display(Out), nl.

fib(N,F):- 
        fibaux(N,0,1,F).

% #if defined(CIAO3)
% :- include(engine(spec_arithmetic)).
% #endif

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(fibaux/4, sht, [int, (int ; large), (int ; large), var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
% :- '$props'(fibaux/4, [argmodes=[in,in,in,in]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(fibaux/4, [argmems=[x(0),x(1),x(2),cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(fibaux/4, [argderefs=[true,true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(fibaux/4, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(fibaux/4, [should_trim_frame=no]).
#endif
fibaux(0, Fact, _Fpost, F) :- !, F = Fact.
fibaux(N, Fact, Fpost, F) :- 
% #if defined(CIAO3)
% 	'$trust_type'(N, smallint),
% #endif
        N1 is N - 1,
        Nfib is Fact + Fpost,
	fibaux(N1, Fpost, Nfib, F).

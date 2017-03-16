#include "../mtsys_common.pl"

benchmark_data(exp, 10, exp(13, 7111)).

% :- '$props'(benchmark/2, [imp=nondet]).
benchmark(exp(N, Exp), R-Res) :-
%        display('Naively calculating '), display(N),
%        display('^'), display(Exp), display(' = '), flush_output,
        exponential_naive(N, Exp,R),
%        display(R), nl,
%        display('Divide-and-conquer calculating '), display(N),
%        display('^'), display(Exp), display(' = '), flush_output,
        exponential_div(N, Exp, Res),
%        display(Res), nl,
	true.

%% exponential(Base, Exp, Res): Be smart and split Exp in halves

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(exponential_div/3, sht, [(int ; large), int, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(exponential_div/3, [argmodes=[in,in,out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(exponential_div/3, [argmems=[cvar,cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(exponential_div/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% no improvement...
% :- '$props'(exponential_div/3, [imp=nondet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(exponential_div/3, [should_trim_frame=no]).
#endif
exponential_div(_Base, 0, 1) :- !.
exponential_div(Base, Exp, Res):-
        Exp > 0,
        HalfExp is Exp // 2,
        exponential_div(Base, HalfExp, HalfRes),
	exponential_div_2(Base, Exp, HalfRes, Res).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(exponential_div_2/4, sht, [(int ; large), int, (int ; large), var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(exponential_div_2/4, [argmodes=[in,in,in,out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(exponential_div_2/4, [argmems=[cvar,cvar,cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(exponential_div_2/4, [argderefs=[true,true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(exponential_div_2/4, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(exponential_div_2/4, [should_trim_frame=no]).
#endif
exponential_div_2(_, Exp, HalfRes, Res) :-
	ExpMod2 is Exp mod 2,
#if (OPT_MASK & OPT_TYPES)
	'$trust_type'(ExpMod2, smallint),
#endif
	ExpMod2 =:= 0, !,
	Res is HalfRes*HalfRes.
exponential_div_2(Base, _, HalfRes, Res) :-
	Res is HalfRes*HalfRes*Base.

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(exponential_naive/3, sht, [(int ; large), int, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(exponential_naive/3, [argmodes=[in,in,out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(exponential_naive/3, [argmems=[cvar,cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(exponential_naive/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% BUG: breaks...
% :- '$props'(exponential_naive/3, [imp=semidet]).
% :- '$props'(exponential_naive/3, [imp=nondet]).
#endif
#if (OPT_MASK & OPT_TRIM)
% :- '$props'(exponential_naive/3, [should_trim_frame=no]).
#endif
exponential_naive(_Base, 0, 1) :- !.
exponential_naive(Base, Exp, Res):-
        Exp > 0,
        NewExp is Exp - 1,
        exponential_naive(Base, NewExp, PartRes),
        Res is PartRes * Base.


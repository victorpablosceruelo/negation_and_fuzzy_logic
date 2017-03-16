#include "../mtsys_common.pl"

#if defined(MERCURY)

:- type benchmark_data == int.
:- type benchmark_result == quad.

:- pred dummy_result(quad).
:- mode dummy_result(out) is det.
dummy_result(quad(x,x,x,x)).

:- pred benchmark_data(string, int, int).
:- mode benchmark_data(out, out, out) is det.
benchmark_data("deriv", 100000, 1).

:- pred benchmark(int, int, quad).
:- mode benchmark(in, in, out) is det.
benchmark(_N, _P, quad(E1, E2, E3, E4)) :-
        ops8(E1),
        divide10(E2),
        log10(E3),
        times10(E4).

:- import_module require.

:- type quad --->       quad(expr, expr, expr, expr).
:- type expr --->       log(expr)
                ;       expr * expr
                ;       expr / expr
                ;       x
                ;       num(int)
                ;       expr + expr
                ;       expr - expr
                ;       - expr
                ;       ^(expr, int)
                ;       exp(expr)
                .

:- pred times10(expr).
:- mode times10(out) is det.

:- pred log10(expr).
:- mode log10(out) is det.

:- pred ops8(expr).
:- mode ops8(out) is det.

:- pred divide10(expr).
:- mode divide10(out) is det.

:- pred d(expr, expr, expr).
:- mode d(in, in, out) is det.

ops8(E) :-
        d((x + num(1)) * ((^(x, 2) + num(2)) * (^(x, 3) + num(3))), x, E).

divide10(E) :-
        d(((((((((x / x) / x) / x) / x) / x) / x) / x) / x) / x, x, E).

log10(E) :-
        d(log(log(log(log(log(log(log(log(log(log(x)))))))))), x, E).

times10(E) :-
        d(((((((((x * x) * x) * x) * x) * x) * x) * x) * x) * x, x, E).

d(U + V, X, DU + DV) :-
        d(U, X, DU),
        d(V, X, DV).
d(U - V, X, DU - DV) :-
        d(U, X, DU),
        d(V, X, DV).
d(U * V, X, DU * V + U * DV) :-
        d(U, X, DU),
        d(V, X, DV).
d(U / V, X, (DU * V - U * DV) / ^(V, 2)) :-
        d(U, X, DU),
        d(V, X, DV).
d(^(U, N), X, DU * num(N) * ^(U, N1)) :-
        N1 is N - 1,
        d(U, X, DU).
d(-U, X, -DU) :-
        d(U, X, DU).
d(exp(U), X, exp(U) * DU) :-
        d(U, X, DU).
d(log(U), X, DU / U) :-
        d(U, X, DU).
d(x, X, num(1)) :-
        ( X = x ->
                true
        ;
                error("differentiating wrt nonvariable")
        ).
d(num(_), _, num(0)).

#else

benchmark_data(deriv, 100000, _Data).

benchmark(_Data, quad(E1, E2, E3, E4)) :-
	ops8(E1), divide10(E2), log10(E3), times10(E4).

ops8(E) :-
	d((x + 1) * ((^(x, 2) + 2) * (^(x, 3) + 3)), x, E).

divide10(E) :-
	d(((((((((x / x) / x) / x) / x) / x) / x) / x) / x) / x, x, E).

log10(E) :-
	d(log(log(log(log(log(log(log(log(log(log(x)))))))))), x, E).

times10(E) :-
	d(((((((((x * x) * x) * x) * x) * x) * x) * x) * x) * x, x, E).

#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(d/3, [argmodes=[in,in,out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(d/3, [argmems=[cvar,cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(d/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(d/3, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(d/3, [should_trim_frame=no]).
#endif
d(U + V, X, DU + DV) :-
	!,
	d(U, X, DU),
	d(V, X, DV).
d(U - V, X, DU - DV) :-
	!,
	d(U, X, DU),
	d(V, X, DV).
d(U * V, X, DU * V + U * DV) :-
	!,
	d(U, X, DU),
	d(V, X, DV).
d(U / V, X, (DU * V - U * DV) / ^(V, 2)) :-
	!,
	d(U, X, DU),
	d(V, X, DV).
d(^(U, N), X, DU * N * ^(U, N1)) :-
	!,
	N1 is N - 1,
	d(U, X, DU).
d(-U, X, -DU) :-
	!,
	d(U, X, DU).
d(exp(U), X, exp(U) * DU) :-
	!,
	d(U, X, DU).
d(log(U), X, DU / U) :-
	!,
	d(U, X, DU).
d(X, X, 1) :-
	!.
d(_, _, 0).

#endif

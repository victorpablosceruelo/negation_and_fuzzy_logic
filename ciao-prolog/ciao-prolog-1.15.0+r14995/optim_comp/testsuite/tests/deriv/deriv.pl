:- module(_, [main/0], [pure]).

:- use_module(engine(internals)).
:- use_module(engine(arithmetic)).
%:- use_module(engine(atomic_basic)).
%:- use_module(engine(attributes)).
%:- use_module(engine(basic_props)).
:- use_module(engine(basiccontrol)).
%:- use_module(engine(interpreter)).
%:- use_module(engine(data_facts)).
%:- use_module(engine(exceptions)).
%:- use_module(engine(io_aux)).
%:- use_module(engine(io_basic)).
%:- use_module(engine(prolog_flags)).
%:- use_module(engine(streams_basic)).
%:- use_module(engine(system_info)).
:- use_module(engine(term_basic)).
%:- use_module(engine(term_compare)).
%:- use_module(engine(term_typing)).

:- include('../common').
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

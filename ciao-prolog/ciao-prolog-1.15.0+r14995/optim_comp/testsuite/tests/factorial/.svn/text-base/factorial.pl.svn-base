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
benchmark_data(factorial, 20000, 100).
benchmark(X, Out) :-
	fac(X, Out).

fac(0, 1) :- !.
fac(X, Y) :-
% X > 0,
        X0 is X - 1,
        fac(X0, Y0),
        Y is X * Y0.

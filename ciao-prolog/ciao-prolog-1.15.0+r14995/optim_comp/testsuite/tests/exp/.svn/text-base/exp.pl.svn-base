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
%:- use_module(engine(prolog_flags)).
%:- use_module(engine(system_info)).
:- use_module(engine(term_basic)).
%:- use_module(engine(term_compare)).
%:- use_module(engine(term_typing)).
%:- use_module(library(write)).
%:- use_module(engine(streams_basic)).

:- include('../common').
benchmark_data(exp, 10, exp(13, 7111)).
benchmark(exp(N, Exp), R-Res) :-
%        write('Naively calculating '), write(N),
%        write('^'), write(Exp), write(' = '), flush_output,
        exponential_naive(N, Exp,R),
%        write(R), nl,
%        write('Divide-and-conquer calculating '), write(N),
%        write('^'), write(Exp), write(' = '), flush_output,
        exponential_div(N, Exp, Res),
%        write(Res), nl,
	true.

%% exponential(Base, Exp, Res): Be smart and split Exp in halves
exponential_div(_Base, 0, 1) :- !.
exponential_div(Base, Exp, Res):-
        Exp > 0,
        HalfExp is Exp // 2,
        exponential_div(Base, HalfExp, HalfRes),
        exponential_div_2(Base, Exp, HalfRes, Res).

exponential_div_2(_, Exp, HalfRes, Res) :-
        ExpMod2 is Exp mod 2,
        ExpMod2 =:= 0, !,
        Res is HalfRes*HalfRes.
exponential_div_2(Base, _, HalfRes, Res) :-
        Res is HalfRes*HalfRes*Base.

exponential_naive(_Base, 0, 1) :- !.
exponential_naive(Base, Exp, Res):-
        Exp > 0,
        NewExp is Exp - 1,
        exponential_naive(Base, NewExp, PartRes),
        Res is PartRes * Base.

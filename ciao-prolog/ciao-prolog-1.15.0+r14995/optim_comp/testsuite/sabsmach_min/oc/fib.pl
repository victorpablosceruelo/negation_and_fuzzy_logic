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
%:- use_module(engine(term_basic)).
%:- use_module(engine(term_compare)).
%:- use_module(engine(term_typing)).

main :-
        fib(1000, _Out).
% display(Out), nl.

fib(N,F):-
        fibaux(N,0,1,F).
fibaux(0, Fact, _Fpost, Fact) :- !.
fibaux(N, Fact, Fpost, F) :-
        N1 is N - 1,
        Nfib is Fact + Fpost,
        fibaux(N1, Fpost, Nfib, F).

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

main :-
        primes(98, _Out).
primes(Limit, Ps) :-
        integers(2, Limit, Is),
        sift(Is, Ps).
integers(Low, High, [Low | Rest]) :-




        Low =< High, !,
        M is Low + 1,
        integers(M, High, Rest).
integers(_,_,[]).
sift([], []).
sift([I | Is], [I | Ps]) :-
        remove(Is, I, New),
        sift(New, Ps).
remove([], _, []).
remove([I | Is], P, Nis0) :-




        IModP is I mod P,



        IModP =\= 0, !,
        Nis0 = [I | Nis],
        remove(Is, P, Nis).
remove([_I | Is], P, Nis) :-
        remove(Is, P, Nis).

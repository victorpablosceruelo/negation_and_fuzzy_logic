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
benchmark_data(queens11, 1, _).
benchmark(_Data, _Out) :-
	do_queens.

do_queens :-
        queens(11, _Qs),
        fail.
do_queens.

queens(N, Qs):-
        queens_list(N, Ns),
        queens_2(Ns, [], Qs).
queens_2([], Qs, Qs).
queens_2(Unplaced, Placed, Qs):-
        sel(Q, Unplaced, NewUnplaced),
        no_attack(Q, Placed),
        queens_2(NewUnplaced, [Q|Placed], Qs).
no_attack(Q, Safe):- no_attack_2(Safe, Q, 1).
no_attack_2([], _Queen, _Nb).
no_attack_2([Y|Ys], Queen, Nb) :-
        A is Y + Nb,
        Queen =\= A,
        B is Y - Nb,
        Queen =\= B,
        Nb1 is Nb + 1,
        no_attack_2(Ys, Queen, Nb1).
sel(X, [X|Ys], Ys).
sel(X, [Y|Ys], [Y|Zs]):-
        sel(X, Ys, Zs).

queens_list(0, []).
queens_list(N, [N|Ns]) :-
        N > 0,
        N1 is N - 1,
        queens_list(N1, Ns).

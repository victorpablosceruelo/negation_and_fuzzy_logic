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

%:- use_module(engine(hiord_rt), [call/1]).
% crypt
%
% Cryptomultiplication:
% Find the unique answer to:
% OEE
% EE
% ---
% EOEE
% EOE
% ----
% OOEE
%
% where E=even, O=odd.
% This program generalizes easily
% to any such problem.
% Written by Peter Van Roy

main :-
        crypt(_Out).

crypt([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]) :-
        odd_nondet(A), even_nondet(B), even_nondet(C), even_nondet(E),
        mult([C, B, A], E, [I, H, G, F | X]),
        lefteven_semidet(F), odd_semidet(G), even_semidet(H), even_semidet(I), zero(X), lefteven_nondet(D),
        mult([C, B, A], D, [L, K, J | Y]),
        lefteven_semidet(J), odd_semidet(K), even_semidet(L), zero(Y),
        sum2([I, H, G, F], [0, L, K, J], [P, O, N, M | Z]),
        odd_semidet(M), odd_semidet(N), even_semidet(O), even_semidet(P), zero(Z).
        % write(' '), write(A), write(B), write(C), nl,
        % write('  '), write(D), write(E), nl,
        % write(F), write(G), write(H), write(I), nl,
        % write(J), write(K), write(L), nl,
        % write(M), write(N), write(O), write(P), nl.

% In the usual source this predicate is named sum. However, sum is a
% language construct in NU-Prolog, and cannot be defined as a predicate.
% If you try, nc comes up with an obscure error message.

sum2(AL, BL, CL) :-
        sum3(AL, BL, 0, CL).

sum3([A | AL], [B | BL], Carry, [C | CL]) :- !,
        X0 is A + B,
        X is X0 + Carry,
        C is X mod 10,
        NewCarry is X // 10,
        sum3(AL, BL, NewCarry, CL).
sum3([], BL, 0, BL) :- !.
sum3(AL, [], 0, AL) :- !.
sum3([], [B | BL], Carry, [C | CL]) :- !,
        X is B + Carry,
        NewCarry is X // 10,
        C is X mod 10,
        sum3([], BL, NewCarry, CL).
sum3([A | AL], [], Carry, [C | CL]) :- !,
        X is A + Carry,
        NewCarry is X // 10,
        C is X mod 10,
        sum3([], AL, NewCarry, CL).
sum3([], [], Carry, [Carry]).

mult(AL, D, BL) :- mult2(AL, D, 0, BL).

mult2([], _, Carry, [C, Cend]) :-
        C is Carry mod 10,
        Cend is Carry // 10.
mult2([A | AL], D, Carry, [B | BL] ) :-






        X0 is A * D,



        X is X0 + Carry,



        B is X mod 10,
        NewCarry is X // 10,
        mult2(AL, D, NewCarry, BL).
zero([]).
zero([X | L]) :-



        X = 0,



        zero(L).
odd_nondet(1).
odd_nondet(3).
odd_nondet(5).
odd_nondet(7).
odd_nondet(9).
odd_semidet(1).
odd_semidet(3).
odd_semidet(5).
odd_semidet(7).
odd_semidet(9).
even_nondet(0).
even_nondet(2).
even_nondet(4).
even_nondet(6).
even_nondet(8).
even_semidet(0).
even_semidet(2).
even_semidet(4).
even_semidet(6).
even_semidet(8).
lefteven_nondet(2).
lefteven_nondet(4).
lefteven_nondet(6).
lefteven_nondet(8).
lefteven_semidet(2).
lefteven_semidet(4).
lefteven_semidet(6).
lefteven_semidet(8).

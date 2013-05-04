% crypt
%
% Cryptomultiplication:
% Find the unique answer to:
%	OEE
%	 EE
% 	---
%      EOEE
%      EOE
%      ----
%      OOEE
%
% where E=even, O=odd.
% This program generalizes easily
% to any such problem.
% Written by Peter Van Roy

#include "../mtsys_common.pl"

#if defined(MERCURY)

:- type benchmark_data == int.
:- type benchmark_result == list(int).

:- pred dummy_result(list(int)).
:- mode dummy_result(out) is det.
dummy_result([]).

:- pred benchmark_data(string, int, int).
:- mode benchmark_data(out, out, out) is det.
benchmark_data("crypt", 1000, 0).

:- pred benchmark(int, int, list(int)).
:- mode benchmark(in, in, out) is det.
benchmark(_N, _P, Out) :-
        ( Out2 = promise_only_solution(crypt) ->
	    Out = Out2
	; Out = []
	).

:- pred crypt(list(int)).
:- mode crypt(out) is cc_nondet.

:- pred sum2(list(int), list(int), list(int)).
:- mode sum2(in, in, out) is det.

:- pred sum2_(list(int), list(int), int, list(int)).
:- mode sum2_(in, in, in, out) is det.

:- pred mult(list(int), int, list(int)).
:- mode mult(in, in, out) is det.

:- pred mult_(list(int), int, int, list(int)).
:- mode mult_(in, in, in, out) is det.

:- pred zero(list(int)).
:- mode zero(in) is semidet.

:- pred odd(int).
:- mode odd(in) is semidet.
:- mode odd(out) is multidet.

:- pred even(int).
:- mode even(in) is semidet.
:- mode even(out) is multidet.

:- pred lefteven(int).
:- mode lefteven(in) is semidet.
:- mode lefteven(out) is multidet.

crypt([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]) :-
        crypt:odd(A), crypt:even(B), crypt:even(C), crypt:even(E),
        mult([C, B, A], E, [I, H, G, F | X]),
        lefteven(F), crypt:odd(G), crypt:even(H), crypt:even(I), zero(X), lefteven(D),
        mult([C, B, A], D, [L, K, J | Y]),
        lefteven(J), crypt:odd(K), crypt:even(L), zero(Y),
        sum2([I, H, G, F], [0, L, K, J], [P, O, N, M | Z]),
        crypt:odd(M), crypt:odd(N), crypt:even(O), crypt:even(P), zero(Z).
        % write(' '), write(A), write(B), write(C), nl,
        % write('  '), write(D), write(E), nl,
        % write(F), write(G), write(H), write(I), nl,
        % write(J), write(K), write(L), nl,
        % write(M), write(N), write(O), write(P), nl.

% In the usual source this predicate is named sum. However, sum is a
% language construct in NU-Prolog, and cannot be defined as a predicate.
% If you try, nc comes up with an obscure error message.

sum2(AL, BL, CL) :-
        sum2_(AL, BL, 0, CL).

sum2_([], [], Carry, Cs) :-
        ( Carry = 0 ->
                Cs = []
        ;
                Cs = [Carry]
        ).
sum2_([], [B | BL], Carry, Cs) :-
        ( Carry = 0 ->
                Cs = [B | BL]
        ;
                X is B + Carry,
                NewCarry is X // 10,
                C is X mod 10,
                sum2_([], BL, NewCarry, CL),
                Cs = [C | CL]
        ).
sum2_([A | AL], [], Carry, Cs) :-
        ( Carry = 0 ->
                Cs = [A | AL]
        ;
                X is A + Carry,
                NewCarry is X // 10,
                C is X mod 10,
                sum2_([], AL, NewCarry, CL),
                Cs = [C | CL]
        ).
sum2_([A | AL], [B | BL], Carry, Cs) :-
        X is A + B + Carry,
        C is X mod 10,
        NewCarry is X // 10,
        sum2_(AL, BL, NewCarry, CL),
        Cs = [C | CL].

mult(AL, D, BL) :- mult_(AL, D, 0, BL).

mult_([A | AL], D, Carry, [B | BL] ) :-
        X is A * D + Carry,
        B is X mod 10,
        NewCarry is X // 10,
        mult_(AL, D, NewCarry, BL).
mult_([], _, Carry, [C, Cend]) :-
        C is Carry mod 10,
        Cend is Carry // 10.

zero([]).
zero([0 | L]) :- zero(L).

odd(1).
odd(3).
odd(5).
odd(7).
odd(9).

even(0).
even(2).
even(4).
even(6).
even(8).

lefteven(2).
lefteven(4).
lefteven(6).
lefteven(8).

#else

% Multiple predicate low-level specialization is not supported yet, so
% we do it by hand using *_nondet and *_semidet. The predicates 
% odd, even, etc. are so simple that specialized multiple versions 
% are a good idea.

#if defined(CIAO3)
:- include(engine(spec_arithmetic)).
#endif

benchmark_data(crypt, 1000, _).

benchmark(_Data, Out) :-
	crypt(Out).

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
	sum2_(AL, BL, 0, CL).

sum2_([A | AL], [B | BL], Carry, [C | CL]) :- !,
#if OPTIMIZED
	'$trust_type'(A, smallint),
	'$trust_type'(B, smallint),
#endif
	X0 is A + B,
#if OPTIMIZED
	'$trust_type'(X0, smallint),
#endif
	X is X0 + Carry,
#if OPTIMIZED
	'$trust_type'(X, smallint),
#endif
	C is X mod 10,
	NewCarry is X // 10,
	sum2_(AL, BL, NewCarry, CL).
sum2_([], BL, 0, BL) :- !.
sum2_(AL, [], 0, AL) :- !.
sum2_([], [B | BL], Carry, [C | CL]) :- !,
#if OPTIMIZED
	'$trust_type'(B, smallint),
	'$trust_type'(Carry, smallint),
#endif
	X is B + Carry,
#if OPTIMIZED
	'$trust_type'(X, smallint),
#endif
	NewCarry is X // 10,
	C is X mod 10,
	sum2_([], BL, NewCarry, CL).
sum2_([A | AL], [], Carry, [C | CL]) :- !,
#if OPTIMIZED
	'$trust_type'(A, smallint),
#endif
#if OPTIMIZED
	'$trust_type'(Carry, smallint),
#endif
	X is A + Carry,
#if OPTIMIZED
	'$trust_type'(X, smallint),
#endif
	NewCarry is X // 10,
	C is X mod 10,
	sum2_([], AL, NewCarry, CL).
sum2_([], [], Carry, [Carry]).

mult(AL, D, BL) :- mult_(AL, D, 0, BL).

mult_([], _, Carry, [C, Cend]) :-
#if OPTIMIZED
	'$trust_type'(Carry, smallint),
#endif
	C is Carry mod 10,
	Cend is Carry // 10.
mult_([A | AL], D, Carry, [B | BL] ) :-
#if OPTIMIZED
	'$trust_type'(A, smallint),
#endif
#if OPTIMIZED
	'$trust_type'(D, smallint),
#endif
	X0 is A * D,
#if OPTIMIZED
	'$trust_type'(X0, smallint),
#endif
	X is X0 + Carry,
#if OPTIMIZED
	'$trust_type'(X, smallint),
#endif
	B is X mod 10,
	NewCarry is X // 10,
	mult_(AL, D, NewCarry, BL).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(zero/1, sht, [(list ; atomic([]))]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
% :- '$props'(zero/1, [argmodes=[in]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(zero/1, [argmems=[cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(zero/1, [argderefs=[true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(zero/1, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
% :- '$props'(zero/1, [should_trim_frame=no]).
#endif
zero([]) :- !.
zero([X | L]) :-
#if OPTIMIZED
	'$trust_type'(X, smallint),
#endif
	X = 0,
#if OPTIMIZED
%	'$trust_type'(X, list),
#endif
	zero(L).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(odd_nondet/1, sht, [var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(odd_nondet/1, [argmodes=[out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(odd_nondet/1, [argmems=[cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(odd_nondet/1, [argderefs=[true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(odd_nondet/1, [imp=nondet]).
#endif
#if (OPT_MASK & OPT_TRIM)
% :- '$props'(odd_nondet/1, [should_trim_frame=no]).
#endif
odd_nondet(1).
odd_nondet(3).
odd_nondet(5).
odd_nondet(7).
odd_nondet(9).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(odd_semidet/1, sht, [int]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
% :- '$props'(odd_semidet/1, [argmodes=[in]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(odd_semidet/1, [argmems=[cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(odd_semidet/1, [argderefs=[true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(odd_semidet/1, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
% :- '$props'(odd_semidet/1, [should_trim_frame=no]).
#endif
odd_semidet(1) :- !.
odd_semidet(3) :- !.
odd_semidet(5) :- !.
odd_semidet(7) :- !.
odd_semidet(9).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(even_nondet/1, sht, [var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(even_nondet/1, [argmodes=[out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(even_nondet/1, [argmems=[cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(even_nondet/1, [argderefs=[true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(even_nondet/1, [imp=nondet]).
#endif
#if (OPT_MASK & OPT_TRIM)
% :- '$props'(even_nondet/1, [should_trim_frame=no]).
#endif
even_nondet(0).
even_nondet(2).
even_nondet(4).
even_nondet(6).
even_nondet(8).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(even_semidet/1, sht, [int]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
% :- '$props'(even_semidet/1, [argmodes=[in]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(even_semidet/1, [argmems=[cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(even_semidet/1, [argderefs=[true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(even_semidet/1, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
% :- '$props'(even_semidet/1, [should_trim_frame=no]).
#endif
even_semidet(0) :- !.
even_semidet(2) :- !.
even_semidet(4) :- !.
even_semidet(6) :- !.
even_semidet(8).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(lefteven_nondet/1, sht, [var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(lefteven_nondet/1, [argmodes=[out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(lefteven_nondet/1, [argmems=[cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(lefteven_nondet/1, [argderefs=[true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(lefteven_nondet/1, [imp=nondet]).
#endif
#if (OPT_MASK & OPT_TRIM)
% :- '$props'(lefteven_nondet/1, [should_trim_frame=no]).
#endif
lefteven_nondet(2).
lefteven_nondet(4).
lefteven_nondet(6).
lefteven_nondet(8).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(lefteven_semidet/1, sht, [int]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
% :- '$props'(lefteven_semidet/1, [argmodes=[in]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(lefteven_semidet/1, [argmems=[cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(lefteven_semidet/1, [argderefs=[true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(lefteven_semidet/1, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
% :- '$props'(lefteven_semidet/1, [should_trim_frame=no]).
#endif
lefteven_semidet(2) :- !.
lefteven_semidet(4) :- !.
lefteven_semidet(6) :- !.
lefteven_semidet(8).

#endif


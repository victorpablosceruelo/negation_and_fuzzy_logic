#include "../mtsys_common.pl"

#if defined(MERCURY)

:- type benchmark_data == int.
:- type benchmark_result == list(int).

:- pred dummy_result(list(int)).
:- mode dummy_result(out) is det.
dummy_result([]).

:- pred benchmark_data(string, int, int).
:- mode benchmark_data(out, out, out) is det.
benchmark_data("queens11", 1, 11).

:- pred benchmark(int, int, list(int)).
:- mode benchmark(in, in, out) is det.
benchmark(_N, Data, Out) :-
        ( Out2 = promise_only_solution(do_queens(Data)) ->
	    Out = Out2
	; Out = []
	).

:- pred do_queens(int, list(int)).
:- mode do_queens(in, out) is cc_multi.
do_queens(Data, Qs) :-
        queens(Data, Qs),
	Qs = []. % fail
do_queens(_, []).

:- pred queens(int, list(int)).
:- mode queens(in, out) is nondet.
queens(N, Qs):-
        queens_list(N, Ns),
        queens_2(Ns, [], Qs).

:- pred queens_2(list(int), list(int), list(int)).
:- mode queens_2(in, in, out) is nondet.
queens_2([], Qs, Qs).
queens_2(Unplaced, Placed, Qs):-
        sel(Q, Unplaced, NewUnplaced),
        no_attack(Q, Placed),
        queens_2(NewUnplaced, [Q|Placed], Qs).

:- pred no_attack(int, list(int)).
:- mode no_attack(in, in) is semidet.
no_attack(Q, Safe):- no_attack_2(Safe, Q, 1).

:- pred no_attack_2(list(int), int, int).
:- mode no_attack_2(in, in, in) is semidet.
no_attack_2([], _Queen, _Nb).
no_attack_2([Y|Ys], Queen, Nb) :-
        A is Y + Nb,
	Queen \= A,
        B is Y - Nb,
	Queen \= B,
        Nb1 is Nb + 1,
        no_attack_2(Ys, Queen, Nb1).

:- pred sel(int, list(int), list(int)).
:- mode sel(out, in, out) is nondet.
sel(X, [X|Ys], Ys).
sel(X, [Y|Ys], [Y|Zs]):-
        sel(X, Ys, Zs).

:- pred queens_list(int, list(int)).
:- mode queens_list(in, out) is det.
queens_list(N, Ns0) :-
	( N = 0 ->
	    Ns0 = []
	; N1 is N - 1,
	  Ns0 = [N|Ns],
	  queens_list(N1, Ns)
	).

#else

#if defined(CIAO3)
:- include(engine(spec_arithmetic)).
#endif
% 11-queens program (obtain all the solutions)

benchmark_data(queens11, 1, _).

benchmark(_Data, _Out) :-
	do_queens.

do_queens:-
        queens(11, _Qs),
%	display(_Qs), nl,
	fail.
do_queens.

queens(N, Qs):-
        queens_list(N, Ns),
        queens_2(Ns, [], Qs).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(queens_2/3, sht, [intlist, intlist, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(queens_2/3, [argmodes=[in,in,out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
:- '$props'(queens_2/3, []).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(queens_2/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
:- '$props'(queens_2/3, []).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(queens_2/3, [should_trim_frame=no]).
#endif
queens_2([], Qs, Qs).
queens_2(Unplaced, Placed, Qs):-
        sel(Q, Unplaced, NewUnplaced),
        no_attack(Q, Placed),
        queens_2(NewUnplaced, [Q|Placed], Qs).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(no_attack/2, sht, [int, intlist]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(no_attack/2, [argmems=[cvar,x(0)]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(no_attack/2, [argderefs=[true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(no_attack/2, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(no_attack/2, [should_trim_frame=no]).
#endif
no_attack(Q, Safe):- no_attack_2(Safe, Q, 1).


#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(no_attack_2/3, sht, [intlist, int, int]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(no_attack_2/3, [argmems=[x(0),cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(no_attack_2/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(no_attack_2/3, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(no_attack_2/3, [should_trim_frame=no]).
#endif
no_attack_2([], _Queen, _Nb) :- !.
no_attack_2([Y|Ys], Queen, Nb) :-
#if defined(CIAO3)
	'$trust_type'(Y, smallint),
	'$trust_type'(Nb, smallint),
#endif
        A is Y + Nb,
#if defined(CIAO3)
	'$trust_type'(Queen, smallint),
#endif
	Queen =\= A,
        B is Y - Nb,
	Queen =\= B,
        Nb1 is Nb + 1,
        no_attack_2(Ys, Queen, Nb1).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(sel/3, sht, [var, intlist, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(sel/3, [argmodes=[out,in,in]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
:- '$props'(sel/3, []).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
:- '$props'(sel/3, []).
#endif
#if (OPT_MASK & OPT_IMP)
:- '$props'(sel/3, []).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(sel/3, [should_trim_frame=no]).
#endif
sel(X, [X|Ys], Ys).
sel(X, [Y|Ys], [Y|Zs]):-
        sel(X, Ys, Zs).

queens_list(0, []).
queens_list(N, [N|Ns]) :-
        N > 0,
        N1 is N - 1,
        queens_list(N1, Ns).
#endif

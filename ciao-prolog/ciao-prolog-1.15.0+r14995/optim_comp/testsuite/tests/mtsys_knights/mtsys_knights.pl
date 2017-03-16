#include "../mtsys_common.pl"

#if defined(MERCURY)

:- type benchmark_data == int.
:- type benchmark_result == list(pair).

:- pred dummy_result(benchmark_result).
:- mode dummy_result(out) is det.
dummy_result([]).

:- pred benchmark_data(string, int, benchmark_data).
:- mode benchmark_data(out, out, out) is det.
benchmark_data("knights", 1, 5).

:- pred benchmark(int, benchmark_data, benchmark_result).
:- mode benchmark(in, in, out) is det.
benchmark(_N, Data, P) :-
	knights(Data, P).

:- type pair ---> p(int, int).

:- pred knights(int, list(pair)).
:- mode knights(in, out) is det.
knights(N, P) :-
	( P2 = promise_only_solution(path(N, 1, 1)) ->
	    P = P2
	; P = []
	).

:- pred member_(list(pair), pair).
:- mode member_(in, out) is nondet.
member_([X|_], X).
member_([_|Xs], X) :- member_(Xs, X).

:- pred path(int, int, int, list(pair)).
:- mode path(in, in, in, out) is cc_nondet.
path(S, X, Y, Path) :-
	S2 is S * S - 1,
	path_2(S2, [p(X, Y)], S, Path).

:- pred path_2(int, list(pair), int, list(pair)).
:- mode path_2(in, in, in, out) is nondet.
path_2(0, Path, _, Path).
path_2(Left0, Path0, S, Path) :-
	Path0 = [P|_],
	P = p(X, Y),
%	display(P), nl,
	move(X, Y, X1, Y1),
	valid(X1, Y1, S),
	New = p(X1, Y1),
%	display(New), nl,
	\+ member_(Path0, New),
	Left is Left0 - 1,
	path_2(Left, [New|Path0], S, Path).

:- pred valid(int, int, int).
:- mode valid(in, in, in) is semidet.
valid(X, Y, S) :-
	bound(X, S),
	bound(Y, S).

:- pred bound(int, int).
:- mode bound(in, in) is semidet.
bound(X, S) :- X >= 1, X =< S.

:- pred move(int, int, int, int).
:- mode move(in, in, out, out) is multidet.
move(X, Y, X1, Y1) :-
	delta(A, B),
	move_1(X, X1, A),
	move_1(Y, Y1, B).

:- pred delta(int, int).
:- mode delta(out, out) is multidet.
delta(1, 2).
delta(2, 1).

:- pred move_1(int, int, int).
:- mode move_1(in, out, in) is multidet.
move_1(X, X1, A) :- X1 is X + A.
move_1(X, X1, A) :- X1 is X - A.

#else 

#if defined(CIAO3)
:- include(engine(spec_arithmetic)).
#endif

benchmark_data(knights, 1, 5).

benchmark(N, P) :-
	knights(N, P).
%	display(P), nl.

knights(N, P) :-
	path(N, 1, 1, P), !.

member_([X|_], X).
member_([_|Xs], X) :- member_(Xs, X).

#if OPTIMIZED
% :- '$trust_entry'(path/4, sht, [int, int, int, var]).	
#endif
path(S, X, Y, Path) :-
	S2 is S * S - 1,
	path_2(S2, [(X, Y)], S, Path).

#if OPTIMIZED
% :- '$trust_entry'(path2/4, sht, [int, nonvar, int, var]).	
#endif
path_2(0, Path, _, Path).
path_2(Left0, Path0, S, Path) :-
	Path0 = [P|_],
	P = (X, Y),
#if defined(CIAO3)
	'$trust_type'(X, smallint),
	'$trust_type'(Y, smallint),
#endif
%	display(P), nl,
	move(X, Y, X1, Y1),
	valid(X1, Y1, S),
	New = (X1, Y1),
%	display(New), nl,
	\+ member_(Path0, New),
#if defined(CIAO3)
	'$trust_type'(Left0, smallint),
#endif
	Left is Left0 - 1,
	path_2(Left, [New|Path0], S, Path).

#if (OPT_MASK & OPT_TYPES)
:- '$trust_entry'(valid/3, sht, [smallint, smallint, smallint]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(valid/3, [argmodes=[in,in,in]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(valid/3, [argmems=[cvar,cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(valid/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(valid/3, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(valid/3, [should_trim_frame=no]).
#endif
valid(X, Y, S) :-
	bound(X, S),
	bound(Y, S).

#if (OPT_MASK & OPT_TYPES)
:- '$trust_entry'(bound/2, sht, [smallint, smallint]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(bound/2, [argmodes=[in,in]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(bound/2, [argmems=[cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(bound/2, [argderefs=[true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(bound/2, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(bound/2, [should_trim_frame=no]).
#endif
bound(X, S) :- X >= 1, X =< S.

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(move/4, sht, [int, int, var, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(move/4, [argmodes=[in,in,out,out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(move/4, [argmems=[cvar,cvar,cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(move/4, [argderefs=[true,true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(move/4, [imp=nondet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(move/4, [should_trim_frame=no]).
#endif
move(X, Y, X1, Y1) :-
	delta(A, B),
	move_1(X, X1, A),
	move_1(Y, Y1, B).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(delta/2, sht, [var, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(delta/2, [argmodes=[out,out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(delta/2, [argmems=[cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(delta/2, [argderefs=[true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(delta/2, [imp=nondet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(delta/2, [should_trim_frame=no]).
#endif
delta(1, 2).
delta(2, 1).

#if (OPT_MASK & OPT_TYPES)
:- '$trust_entry'(move_1/3, sht, [smallint, var, smallint]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(move_1/3, [argmodes=[in,out,in]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(move_1/3, [argmems=[cvar,cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(move_1/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(move_1/3, [imp=nondet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(move_1/3, [should_trim_frame=no]).
#endif
move_1(X, X1, A) :- X1 is X + A.
move_1(X, X1, A) :- X1 is X - A.

#endif

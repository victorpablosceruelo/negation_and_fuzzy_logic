:- module(_, _, []).

% 11-queens program (obtain all the solutions)

:- use_module(library(prolog_sys)).
benchmark_system(newciao).
'$cputime'(X) :- statistics(runtime, [X|_]).
benchmark_start :-
	'$cputime'(T1),
	benchmark_data(Name, Count, Data),
	benchmark_loop(Count, Data),
	'$cputime'(T2),
	Time is T2-T1,
	benchmark_system(System),
	display(t(System,Name,Count,Time)), display('.'), nl.

benchmark_loop(Count, Data) :-
	repeat(Count),
	benchmark(Data, _Result),
	fail.
benchmark_loop(_, _).

repeat(_N).
repeat(N) :-
	N > 1,
	N1 is N - 1,
	repeat(N1).

benchmark_data(queens11, 10, _).

benchmark(_Data, _Out) :-
	do_queens.

do_queens:-
        queens(11, Qs),
%	display(Qs), nl,
	fail.
do_queens.

:- '$ptoc_prop'('ptoc_q:queens'/2, [register=true]).
queens(N, Qs):-
        queens_list(N, Ns),
        queens_2(Ns, [], Qs).

:- '$ptoc_type'(intlist, (list ; atomic([]))).

:- '$ptoc_prop'('ptoc_q:queens_2'/3, [call_types=[intlist, intlist, var], exit_types=[intlist, intlist, intlist]]).
:- '$ptoc_prop'('ptoc_q:queens_2'/3, [argmodes=[in,in,out]]).
:- '$ptoc_prop'('ptoc_q:queens_2'/3, [argderefs=[true,true,true]]).
%:- '$ptoc_prop'('ptoc_q:queens_2'/3, [should_trim_frame=no]).
queens_2([], Qs, Qs).
queens_2(Unplaced, Placed, Qs):-
        sel(Q, Unplaced, NewUnplaced),
        no_attack(Q, Placed),
        queens_2(NewUnplaced, [Q|Placed], Qs).

:- '$ptoc_prop'('ptoc_q:no_attack'/2, [call_types=[smallint, intlist], exit_types=[smallint, intlist]]).
:- '$ptoc_prop'('ptoc_q:no_attack'/2, [argmodes=[in,in]]).
%:- '$ptoc_prop'('ptoc_q:no_attack'/2, [argmems=[cvar,x(0)]]).
:- '$ptoc_prop'('ptoc_q:no_attack'/2, [argderefs=[true,true]]).
:- '$ptoc_prop'('ptoc_q:no_attack'/2, [imp=semidet]).
%:- '$ptoc_prop'('ptoc_q:no_attack'/2, [should_trim_frame=no]).
no_attack(Q, Safe):- no_attack_2(Safe, Q, 1).

:- '$ptoc_prop'('ptoc_q:no_attack_2'/3, [call_types=[intlist, smallint, smallint], exit_types=[intlist, smallint, smallint]]).
:- '$ptoc_prop'('ptoc_q:no_attack_2'/3, [argmodes=[in,in,in]]).
%:- '$ptoc_prop'('ptoc_q:no_attack_2'/3, [argmems=[x(0),cvar,cvar]]).
:- '$ptoc_prop'('ptoc_q:no_attack_2'/3, [argderefs=[true,true,true]]).
:- '$ptoc_prop'('ptoc_q:no_attack_2'/3, [imp=semidet]).
%:- '$ptoc_prop'('ptoc_q:no_attack_2'/3, [should_trim_frame=no]).
no_attack_2([], _Queen, _Nb).
no_attack_2([Y|Ys], Queen, Nb) :-
        A is Y + Nb,
	Queen =\= A,
        B is Y - Nb,
	Queen =\= B,
        Nb1 is Nb + 1,
        no_attack_2(Ys, Queen, Nb1).

:- '$ptoc_prop'('ptoc_q:sel'/3, [call_types=[var, intlist, var]]).
:- '$ptoc_prop'('ptoc_q:sel'/3, [argmodes=[out,in,in]]).
:- '$ptoc_prop'('ptoc_q:sel'/3, []).
:- '$ptoc_prop'('ptoc_q:sel'/3, []).
:- '$ptoc_prop'('ptoc_q:sel'/3, []).
%:- '$ptoc_prop'('ptoc_q:sel'/3, [should_trim_frame=no]).
sel(X, [X|Ys], Ys).
sel(X, [Y|Ys], [Y|Zs]):-
        sel(X, Ys, Zs).

queens_list(0, []).
queens_list(N, [N|Ns]) :-
        N > 0,
        N1 is N - 1,
        queens_list(N1, Ns).

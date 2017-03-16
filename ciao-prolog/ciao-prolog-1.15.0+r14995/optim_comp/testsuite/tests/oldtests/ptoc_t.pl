:- module(_, _, []).

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

benchmark_data(tak, 1000, triple(18, 12, 6)).

benchmark(triple(X, Y, Z), Out) :-
	tak(X, Y, Z, Out).

/*:- ctocprop(tak/4, [imp=det, argimptypes=[smallint, smallint, smallint, smallint], call_types=[smallint, smallint, smallint, var], exit_types=[smallint, smallint, smallint, smallint], argmodes=[in,in,in,out], argmems=[cvar,cvar,cvar,cvar], argderefs=[true,true,true,true], should_trim_frame=no]). THIS WILL NOT WORK!! CHOICE POINTS CANNOT STORE UNTAGGED VALUES (at this moment) */

:- '$preddef'(tak/4, ptoc).
:- '$ptoc_prop'('ptoc_t:tak'/4, [call_types=[smallint, smallint, smallint, var], exit_types=[smallint, smallint, smallint, smallint]]).
% :- '$ptoc_prop'('ptoc_t:tak'/4, [argmodes=[in,in,in,out]]).
%:- '$ptoc_prop'('ptoc_t:tak'/4, [argmems=[cvar,cvar,cvar,cvar]]).
%:- '$ptoc_prop'('ptoc_t:tak'/4, [argderefs=[true,true,true,true]]).
:- '$ptoc_prop'('ptoc_t:tak'/4, [imp=det]).
:- '$ptoc_prop'('ptoc_t:tak'/4, [should_trim_frame=no]).
:- '$ptoc_prop'('ptoc_t:tak'/4, [register = true]).
:- '$ptoc_prop'('ptoc_t:tak'/4, [indexed = false]).

%tak(X,Y,Z,A) :-
%        display(tak(X,Y,Z,A)), nl, fail.
tak(X,Y,Z,A) :-
	X =< Y, !,
%        display(tak_1(X,Y,Z,A)), nl,
	Z = A.
tak(X,Y,Z,A) :-
%        display(tak(X,Y,Z,A)), nl,
	% X > Y,
	X1 is X - 1,
	tak(X1,Y,Z,A1),
	Y1 is Y - 1,
	tak(Y1,Z,X,A2),
	Z1 is Z - 1,
	tak(Z1,X,Y,A3),
	tak(A1,A2,A3,A).

/*
:- '$preddef'(ap/3, ptoc).
:- '$ptoc_prop'('ptoc_t:ap'/3, [imp=nondet]).
:- '$ptoc_prop'('ptoc_t:ap'/3, [should_trim_frame=no]).
:- '$ptoc_prop'('ptoc_t:ap'/3, [register = true]).

ap([], X, X).
ap([X|Xs], Ys, [X|Zs]) :- ap(Xs, Ys, Zs).
*/

:- module( tak, [tak/4 , main/0], [assertions , nativeprops] ).

:- use_module(library(write)).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(system)).
:- use_module(library(format), [format/2]).
:- use_module(library(between)).
:- use_module(library(dynamic)).
:- use_module(library(aggregates), [findall/3]).
:- use_module('./common_bench').

:- push_prolog_flag(multi_arity_warnings, off).

:- dynamic time_exec/1.


tak(X,Y,Z,A) :-
        X=<Y,
        !,
        Z=A .
tak(X,Y,Z,A) :-
        X1 is X-1,
        Y1 is Y-1,
        Z1 is Z-1,
        tak(Z1,X,Y,A3),
        tak(Y1,Z,X,A2),
        tak(X1,Y,Z,A1),
        tak(A1,A2,A3,A) .

main4(X,Y,Z,_1) :-
        X1 is X-1,
        Y1 is Y-1,
        Z1 is Z-1,
        tak(Z1,X,Y,A3),
        tak(Y1,Z,X,A2),
        tak(X1,Y,Z,A1),
        tak(A1,A2,A3,_A),
        X2 is X-1,
        Y2 is Y-1,
        Z2 is Z-1,
        tak(Z2,X,Y,A6),
        tak(Y2,Z,X,A5),
        tak(X2,Y,Z,A4),
        tak(A4,A5,A6,_B),
        X3 is X-1,
        Y3 is Y-1,
        Z3 is Z-1,
        tak(Z3,X,Y,A9),
        tak(Y3,Z,X,A8),
        tak(X3,Y,Z,A7),
        tak(A7,A8,A9,_C),
        X4 is X-1,
        Y4 is Y-1,
        Z4 is Z-1,
        tak(Z4,X,Y,A12),
        tak(Y4,Z,X,A11),
        tak(X4,Y,Z,A10),
        tak(A10,A11,A12,_D) .

main :-
% 	set_prolog_flag(gc, off),
% 	between(1,8,_),
%         statistics(walltime, [T1,_]),
        main4(20,10,3,_1).
%         statistics(walltime, [T2,_]),
%         Delta is T2 - T1,
% 	format("-- main=~f ms.~n", [Delta]),
% 	asserta_fact(time_exec(Delta)),
% 	fail.
% main :-
% 	findall(Time,(time_exec(Time),retract_fact(time_exec(Time))),L),
% 	delete_min_max(L,L2),
% 	average_list(L2,Average),
% 	format("     Time Average     = ~f ms.~n~n", [Average]).



:- module(wms2_main,
        [
            wms2_test/2
        ],
	[]).


:- use_package(andprolog_nd).

:- use_module('./common_bench').
:- use_module('./wms2').
:- use_module('./wms2_mel').
:- use_module('./wms2_udg').
:- use_module('./wms2_uoudg').
:- use_module('./wms2_uudg').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions without granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wms2_test(Reps, Name/Arity) :-
        functor(Goal, Name, Arity),
        bench_rep(Reps, Goal, Name/Arity).

:- push_prolog_flag(multi_arity_warnings, off).

:- use_module(library(lists)).

wms2_seq :- 
	wms2:main.

wms2_mel :- 
	wms2_mel:main.

wms2_udg :- 
	wms2_udg:main.

wms2_uoudg :- 
	wms2_uoudg:main.

wms2_uudg :- 
	wms2_uudg:main.



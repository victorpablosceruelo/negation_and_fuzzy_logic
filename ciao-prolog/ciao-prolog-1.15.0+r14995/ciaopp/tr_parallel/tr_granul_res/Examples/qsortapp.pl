:- module(qsortapp, [qsort/2], [assertions, predefres(res_all)]).

/* 
   This version is annotated for inference of lower and upper bounds of a
   set of resources: steps, giunif, gounif, nargs, viunif, and vounif. In
   addition, it is also annotated to apply the granularity analysis on any
   subset of these resources. 

   ?- module(qsortapp).
   ?- set_pp_flag(para_grain,gr).  % granularity analysis using 'old' infercost
   ?- analyze(shfr),analyze(eterms),analyze(nfg).
   ?- transform(mel).

   ?- module(qsortapp).
   ?- set_pp_flag(para_grain,gr_res).  % granularity analysis using resource analysis
   ?- analyze(shfr),analyze(eterms),analyze(nfg).
   ?- transform(mel).
   
*/


:- entry qsort(As, Bs)
	: (list(As), ground(As), var(Bs)).

:- granularity_resources([steps, giunif, gounif, nargs, viunif, vounif]).

append([],    X, X).
append([H|X], Y, [H|Z]) :- append(X, Y, Z).

partition([],    _, [],        []).
partition([E|R], C, [E|Left1], Right) :- E < C, !,
	partition(R, C, Left1, Right).
partition([E|R], C, Left, [E|Right1]) :-
	partition(R, C, Left, Right1).

qsort([],    []).
qsort([X|L], R) :-
	partition(L, X, L1, L2),
	qsort(L2, R2),
	qsort(L1, R1),
	append(R1, [X|R2], R).

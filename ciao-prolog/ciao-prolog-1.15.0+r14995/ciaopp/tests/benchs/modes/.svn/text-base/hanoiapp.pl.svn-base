/*-----------------------------------------------------------------------------
Program: Hanoi
Author:  
Date:

Notes:
1. To run:
    ?- shanoi(n,a,b,c,Moves).
2. Solution is reached as in the standard approach by intermediate movements.
   Solutions are given in the form of a list of succesive movements.
-----------------------------------------------------------------------------*/
:- module(hanoiapp,[ shanoi/5 ],[ assertions,nativeprops]).
:- use_module(library(format), 
	[ format/2 ]).
:- use_module(library(prolog_sys), 
	[ statistics/2 ]).

:- entry shanoi(A,B,C,D,E) : ( var(E), ground([A,B,C,D]) ).

shanoi(1,A,_B,C,[mv(A,C)]).
shanoi(N,A,B,C,M) :-
	N > 1,
	N1 is N - 1,
	shanoi(N1,A,C,B,M1),
	shanoi(N1,B,A,C,M2),
	append(M1,[mv(A,C)],T),
	append(T,M2,M).

append([],L,L).
append([H|L],L1,[H|R]) :-
	append(L,L1,R).

%-----------------------------------------------------------------------------

demo:-  Size=15,
	format('Towers of Hanoi for ~w elements',[Size]),
	time(_,_A), nl, nl,
	shanoi(Size,a,b,c,Sol),
	time(T,Sol),
	format('The solution is: ~w',[Sol]),nl,
	format('Solved in ~w msec.',[T]), nl.

time(Time,_):- statistics(runtime,[_,Time]).


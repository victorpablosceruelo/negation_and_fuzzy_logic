:- module(sgm, 
	[
	    times/2,
	    sg/2
	]).

:- use_package(tabling).
 %% :- const_table_module(library(disequality(disequality_tab))).
 %% :- use_package(library(disequality)).
 %% :- const_table_module(library(
 %% 	difference_constraints(difference_constraints_tab))).
 %% :- use_package(library(difference_constraints)).

:- include(times).

:- table sg/2.

sg(X,X) :- X > 50.
sg(X,Y):-
	edge(X,XX),
	sg(XX,YY),
	edge(Y,YY),
	Y > 50.

 %% spend_time(D,NT,T) :-
 %% 	NT2 is NT * 1000,
 %% 	abolish_all_tables,
 %%         statistics(runtime,[_,_]),
 %% 	X =/= D, sg(_,X), 
 %% 	abolish_all_tables,
 %%         statistics(runtime,[_,Tt]),
 %% 	N is (NT2 / Tt) + 1,
 %%         statistics(runtime,[_,_]),
 %%         (
 %%             between(1,N,_),
 %%             sg(_,X), abolish_all_tables,
 %%             fail
 %%         ;
 %%             true
 %%         ),
 %%         statistics(runtime,[_,Tfin]),
 %%         (
 %%             between(1,N,_),
 %%             fail
 %%         ;
 %%             true
 %%         ),
 %%         statistics(runtime,[_,Tfin2]),
 %%         T is (Tfin - Tfin2) / N,
 %% 	display('Execution time '), display(T), nl.

:-include('sg_edge.pl').

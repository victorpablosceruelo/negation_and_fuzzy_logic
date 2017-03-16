:- module(fischer_merge,
	[
 %% 	    f/1,
	    times/2,
	    fischer/2
	],[]).

 %% :- use_module(library(dynamic)).
 %% :- dynamic sol/1.
 %% :- dynamic answ/1.

:- use_package(library(tabling)).
:- const_table_module(library(difference_constraints(difference_constraints_tab))).
:- use_package(library(difference_constraints)).

:- use_module(library(lists), [length/2]).

:- include('../times').
:- include('../initialize').
:- include('../member').

:- table reach/1.

 %% f(N) :- 
 %% 	assert(sol(0)),
 %% 	assert(answ(0)),
 %% 	fischer(N,_S),
 %% 	retract(sol(N1)),
 %% 	N2 is N1 + 1,
 %% 	assert(sol(N2)),	
 %%  	fail.
 %% f(_) :-
 %% 	retract(answ(N1)),
 %% 	nl, display(answers(N1)),nl,
 %% 	retract(sol(N2)),
 %% 	display(soluciones(N2)),nl.
	
fischer(N,state(LY,Y)) :-
	length(Y,N),
	reach(state(LY,Y)).

reach(state(l(0),Y)) :-
      initialize_time_LB(Y).
 %%       retract(answ(N1)),
 %%       N2 is N1 + 1,
 %%       assert(answ(N2)).

reach(state(LY,Y)) :- 
	length(Y,N),
	length(X,N),
	reach(state(LX,X)), 
	each_trans(N,state(LX,X),state(LY,Y)).
 %% 	retract(answ(N1)),
 %% 	N2 is N1 + 1,
 %% 	assert(answ(N2)).


each_trans(N,state(LX,X),state(LY,Y)) :-
	get_ta_member(_,X,LX,Xi,I),
	trans(N,state(LX,Xi),state(LY,Yi)),
	difference_constraints_do_canonical,
	take_out_member(I,X,Xdelay),
	difference_constraints_reset(Xi,Yi,Xdelay),
	difference_constraints_delay(Xdelay),
	put_member(I,LY,Yi,_,Xdelay,_,Y).

trans(I, state(l(N),_), state(l(0),X)) :- 
	N == I, !,
	X #>= 0.

trans(_, state(l(N),X), state(l(M),Y)) :- 
	M is N + 1,
	X #>= 1,
	Y #>= 0.


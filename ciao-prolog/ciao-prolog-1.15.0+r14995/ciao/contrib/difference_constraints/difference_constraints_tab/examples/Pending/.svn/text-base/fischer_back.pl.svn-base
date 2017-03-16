:- module(fischer_back,
	[
 %% 	    f/1,
	    times/2,
  	    fischer/2
	],[]).

:- use_module(library(dynamic)).
:- dynamic sol/1.
:- dynamic answ/1.

:- use_module(library(write),[print/1]).

:- use_package(library(tabling)).
:- const_table_module(library(difference_constraints(difference_constraints_tab))).
:- use_package(library(difference_constraints)).

:- use_module(library(lists), [length/2]).

:- include(times).
:- include(initialize).
:- include(member).

:- table back_reach/1.

 %% f(N) :- 
 %%   	assert(sol(0)),
 %%  %% 	assert(answ(0)),
 %% 	fisher(N,S),
 %% 	(
 %% 	    S = state([l0,l0],_,_) ->
 %% 	    display(bug),nl
 %% 	;
 %% 	    retract(sol(N1)),
 %% 	    N2 is N1 + 1,
 %% 	    assert(sol(N2)),	
 %% 	    %%     print(soluciones(S)), nl,
 %% 	    %%   	gecode_print, nl,
 %% 	    fail
 %% 	).
 %% 
 %% f(_) :-
 %%  %% 	retract(answ(N1)),
 %%  %% 	display(answers(N1)),nl,
 %%  	retract(sol(N2)),
 %%  	display(soluciones(N2)),nl.
	
fischer(N,state(LY,K,Y)) :-
	length(LY,N),
	length(Y,N),
	back_reach(state(LY,K,Y)).

back_reach(state(LY,K,Y)) :-
  	init_var_state_pair(LY,lcs), 
 %% 	LY=[lcs,lcs,lcs],
	initialize_time_LB(Y),
	difference_constraints_var(K),
	apply_invariant(LY,Y).
 %% 	retract(answ(N1)),
 %% 	N2 is N1 + 1,
 %% 	assert(answ(N2)).
 %% 	print(sol_base(state(LY,K,Y))),nl.


back_reach(state(LY,K2,Y)) :- 
	length(LY,N),
	length(LX,N),
	length(X,N),
 	back_reach(state(LX,K1,X)), 
 %% 	print(leo_sol(state(LX,K1,X))),nl,
 %% 	gecode_print,nl,
 	each_back_trans(state(LX,K1,X),state(LY,K2,Y)).
 %% 	retract(answ(N1)),
 %% 	N2 is N1 + 1,
 %% 	assert(answ(N2)).
 %% 	print(sol_recursiva(state(LY,K2,Y))),nl.

each_back_trans(state(LX,KX,X),state(LY,KY,Y)) :-
	get_ta_member(LX,X,LXi,Xi,I),
	trans(state(LYi,I,KY,Yi),state(LXi,I,KX,Xi),Reset),
	(
	    Reset = yes ->
 %% 	    do_canonical,
	    take_out_member(I,X,XReset),
	    inv_reset(Xi,XReset)
	;
	    true
	),
	put_member_no_delay(I,LYi,Yi,LX,X,LY,Y),
	apply_invariant(LY,Y).

trans(state(l0,Id,K,X), state(l1,Id,K,_),yes) :- !,
	K = 0,
	X #>= 0.

trans(state(l1,Id,K,X), state(l2,Id,Id,_),yes) :- !,
	difference_constraints_var(K),
	X #>= 0,
	X #=< 2.

trans(state(l2,Id,K,X), state(l0,Id,K,X),no) :-
	X #>= 4,
	K #<> Id, !.

trans(state(l2,Id,K,X), state(lcs,Id,K,X),no) :- !,
	K #= Id,
	X #>= 4.	

trans(state(lcs,Id,K,X), state(l0,Id,0,X),no) :-
	difference_constraints_var(K).

apply_invariant([],[]).
apply_invariant([L|RL],[X|RX]) :-
	invariant(state(L,_,_,X)),
	apply_invariant(RL,RX).

invariant(state(l0,_,_,_)).
invariant(state(l1,_,_,X)) :- X #=< 2.
invariant(state(l2,_,_,_)).
invariant(state(lcs,_,_,_)).


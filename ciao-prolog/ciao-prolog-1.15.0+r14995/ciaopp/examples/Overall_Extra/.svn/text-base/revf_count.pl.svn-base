:- module(_, _, [assertions,fsyntax,regtypes]).
:- use_module(library(assertions(native_props))).
:- use_module(library(dynamic)).
:- use_module(library(write)).

:- entry nrev/2 : {list, ground} * var.

:- check comp nrev(A,_) + steps(length(A)+1).  

%% :- check success nrev(A,L) => int(A).  

:- dynamic counter/1.

clear :- 
	retract(counter(_)), !, assert(counter(0)).
clear :- 
	assert(counter(0)).
get(X) :- counter(X).
addstep :- write('.'),
	   retract(counter(N)), N1 is N+1, assert(counter(N1)).
compute(X,Y) :- Y is 0.5* (X**2)+ 1.5*X +1.

nrev( [], [] ) :- addstep.
nrev([H|L],_1) :-
        nrev(L,_2),
        conc(_2,[H],_1),
	addstep.

conc([],L,L) :- addstep.

conc([H|L],K,[H|_1]) :-
        conc(L,K,_1), 
	addstep.

%% 2 - 6 
%% 4 - 15
%% 8 - 45
%% 16 - 153
%% 32 - 561
%% 64 - 2045
%% 128 - 8385
%% 256 - 33153
%% 512 - 131841
% -> cuadratic

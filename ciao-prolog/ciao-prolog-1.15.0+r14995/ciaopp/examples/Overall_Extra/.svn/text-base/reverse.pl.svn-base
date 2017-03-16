:- module(reverse, [nrev/2], [assertions]).

:- use_module(library(assertions(native_props))).

:- entry nrev(A,B) : (ground(A), list(A, term), var(B)).

:- check comp nrev(A,B) + steps_ub(length(A)+1). 
:- check comp nrev(A,B) + steps(length(A)+1).    
:- check comp nrev(A,B) + steps_lb(length(A)).  
%% :- check comp nrev(A,B) + steps_lb(0.5*exp(length(A),2)+1.5*length(A)+1).%4
%% :- check comp nrev(A,B) + steps_lb(exp(length(A),2)+2*length(A)+1).      %5
%% :- check comp nrev(A,B) + steps_ub(exp(length(A),2)+2*length(A)+1).      %6
%% :- check comp nrev(A,B) + steps_ub(0.5*exp(length(A),2)+1.5*length(A)+1).%7
%% :- check comp nrev(A,B) + steps(exp(length(A),2)+2*length(A)+1).         %8
%% :- check comp nrev(A,B) + steps(0.5*exp(length(A),2)+1.5*length(A)+1).   %9

nrev([],[]).
nrev([H|L],R) :-
	nrev(L,R1),
	append(R1,[H],R).

append([],L,L).
append([H|L],L1,[H|R]) :-
	append(L,L1,R).

 %% NOTES:

 %% With lower bounds analysis should be:

 %%  1 - false: ub assertion which is <.
 %%  2 - false: exact assertion which is <.  
 %%  3 - checked: lb assertion which is <
 %%  4 - checked: lb assertion which is = 
 %%  5 - check: lb assertion which is >
 %%  6 - check: ub assertion which is >
 %%  7 - check: ub assertion which is =
 %%  8 - check: exact assertion which is >  
 %%  9 - check: exact assertion which is =  

% With upper bounds analysis should be:

 %%  1 - check: ub assertion which is <.
 %%  2 - check: exact assertion which is <.  
 %%  3 - check: lb assertion which is <
 %%  4 - check: lb assertion which is = 
 %%  5 - false: lb assertion which is >
 %%  6 - checked: ub assertion which is >
 %%  7 - checked: ub assertion which is =
 %%  8 - false: exact assertion which is >  
 %%  9 - check: exact assertion which is =  

%File : count_time.pl            

%main call:      time(T).
%                time_all(T).

:- module(_, _,[]).

:- use_module(proof_intneg).

:- use_module(library(prolog_sys)).  

:- meta_predicate get_goal(goal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Proofs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%get_goal(boole(1)).    
%get_goal(boole(X)). 
%get_goal(ancestor(X,susan)). 
%get_goal(ancestor(peter,X)). 
%get_goal((peano(500000,N), less(N,0))). 
%get_goal((peano(500000,N), less(N,X))). 
%get_goal((peano(1000000,N), less(N,X))).  
%get_goal((peano(500000,N), less(0,N))).    
%get_goal((peano(500000,N), less(X,N))). 
%get_goal((peano(1000000,N), less(X,N))).    
%get_goal((peano(500,N), positive(N))). 
%get_goal((peano(700,N), positive(N))).  
%get_goal((peano(1000,N), positive(N))). 
%get_goal((peano(100000,N), add(X,Y,N))). 
%get_goal((peano(100000,N), add(N,Y,Z))). 
%get_goal((peano(100000,N), add(X,N,Z))).

%get_goal(intneg(boole(1))).   
%get_goal(intneg(boole(X))). 
%get_goal(intneg(ancestor(X,susan))). 
%get_goal(intneg(ancestor(peter,X))).
%get_goal((peano(500000,N), intneg(less(N,0)))). 
%get_goal((peano(500000,N), intneg(less(N,X)))).
%get_goal((peano(1000000,N), intneg(less(N,X)))).  
%get_goal((peano(500000,N), intneg(less(0,N)))).    
%get_goal((peano(500000,N), intneg(less(X,N)))). 
%get_goal((peano(1000000,N), intneg(less(X,N)))).    
%get_goal((peano(500,N), intneg(positive(N)))).   
%get_goal((peano(700,N), intneg(positive(N)))).   
%get_goal((peano(1000,N), intneg(positive(N)))).
%get_goal((peano(100000,N), intneg(add(X,Y,N)))). 
%get_goal((peano(100000,N), intneg(add(N,Y,Z)))). 
%get_goal((peano(100000,N), intneg(add(X,N,Z)))). 

%get_goal(cnegf(boole(1))).   %T = 1171575162
%get_goal(cnegf(boole(X))). %T = 1171575221
%get_goal(cnegf(ancestor(X,susan))). %T = 1171575388
%get_goal(cnegf(ancestor(peter,X))).
%get_goal((peano(500000,N), cnegf(less(N,0)))). 
%get_goal((peano(500000,N), cnegf(less(N,X)))).
%get_goal((peano(1000000,N), cnegf(less(N,X)))).  
%get_goal((peano(500000,N), cnegf(less(0,N)))).    
%get_goal((peano(500000,N), cnegf(less(X,N)))). 
%get_goal((peano(1000000,N), cnegf(less(X,N)))).    
%get_goal((peano(500,N), cnegf(positive(N)))).   
%get_goal((peano(700,N), cnegf(positive(N)))).   
%get_goal((peano(10000,N), cnegf(positive(N)))).
%get_goal((peano(1000000,N), cnegf(positive(N)))). 
%get_goal((peano(100000,N), cnegf(add(X,Y,N)))). 
%get_goal((peano(100000,N), cnegf(add(N,Y,Z)))). 
%get_goal((peano(100000,N), cnegf(add(X,N,Z)))).

%boole(0). 
%boole(1).

%get_goal(\+ boole(1)).     
%get_goal(\+ boole(X)).
%get_goal(\+ ancestor(X,susan)).
%get_goal(\+ ancestor(peter,X)).
%get_goal((peano(500000,N), \+ less(N,0))). 
%get_goal((peano(500000,N), \+ less(N,X))).
%get_goal((peano(1000000,N), \+ less(N,X))).  
%get_goal((peano(500000,N), \+ less(0,N))).    
%get_goal((peano(500000,N), \+ less(X,N))). 
%get_goal((peano(1000000,N), \+ less(X,N))).    
%get_goal((peano(500,N), \+ positive(N))).   
%get_goal((peano(700,N), \+ positive(N))). 
%get_goal((peano(1000,N), (\+ positive(N)))). 
%boole(X) cierto siempre que X sea un numero booleano.

%get_goal(cneg(boole(1))).     
%get_goal(cneg(boole(X))).
%get_goal(cneg(ancestor(X,susan))).
%get_goal(cneg(ancestor(peter,X))).
%get_goal((peano(500000,N), cneg(less(N,0)))). 
%get_goal((peano(500000,N), cneg(less(N,X)))).
%get_goal((peano(1000000,N), cneg(less(N,X)))).  
%get_goal((peano(500000,N), cneg(less(0,N)))).    
%get_goal((peano(500000,N), cneg(less(X,N)))). 
%get_goal((peano(1000000,N), cneg(less(X,N)))).    
%get_goal((peano(500,N), cneg(positive(N)))).   
%get_goal((peano(700,N), cneg(positive(N)))).   
%get_goal((peano(1000,N), cneg(positive(N)))).
%get_goal((peano(100000,N), cneg(add(X,Y,N)))). 
%get_goal((peano(100000,N), cneg(add(N,Y,Z)))). 
%get_goal((peano(100000,N), cneg(add(X,N,Z)))). 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%peano(0,0).
%peano(N,s(P1)):-
%	N > 0,
%	N1 is N-1,
%	peano(N1,P1).


peano(N,N2):-
	N > 0,
	N1 is N-1,
	peano_aux(N1,0,N2).

peano_aux(0,Cero,Sol):-Sol=Cero.
peano_aux(N,Cero,_Sol):-
	N > 0,
	N1 is N-1,
	Cero1=s(Cero),
	peano_aux(N1,Cero1,_Sol).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sal(X):-get_goal(X).

%time(T).
%time_all(T).

% time(T) retuns the time spending in evaluate all solutions of a goal
time1(T):-
	statistics(runtime,_),
	get_goal(G),
	%(call(G);true),
	(proof(G,1);true), 
	statistics(runtime,[_,T]).

% time_all(T) retuns the time spending in evaluate all solutions of a goal
time_all(T):-
	statistics(runtime,_),
	get_goal(G),
	%(call(G),fail;true),
        (proof(G,1),fail;true),
	statistics(runtime,T).

% proof(Goal,N) calls the goal Goal N times
proof(_Goal,0). 
proof(Goal,N):-
         N > 0,
         call(Goal),
         N1 is N - 1,
         proof(Goal,N1).

%main: time(T).
%           time_all(T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%% THE END %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% File : count_time.pl            main call:      time(T).
%                                                 time_all(T).

:- module(_, _,[]).

:- use_module(queensPeano).

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
%get_goal((peano(500000,N), positive(N))).   
%get_goal((peano(1000000,N), positive(N))). 
%get_goal((peano(100000,N), add(N,Y,Z))). 
%get_goal((peano(100000,N), add(_X,N,_Y))). 

%get_goal(intneg(boole(1),S)).     
%get_goal(intneg(boole(X),S)).
%get_goal(intneg(ancestor(X,susan),S)).
%get_goal(intneg(ancestor(peter,X),S)).
%get_goal((peano(500000,N), intneg(less(N,0),S))). 
%get_goal((peano(500000,N), intneg(less(N,X),S))).
%get_goal((peano(1000000,N), intneg(less(N,X),S))).  
%get_goal((peano(500000,N), intneg(less(0,N),S))).    
%get_goal((peano(500000,N), intneg(less(X,N),S))). 
%get_goal((peano(1000000,N), intneg(less(X,N),S))).    
%get_goal((peano(500,N), intneg(positive(N),S))).   
%get_goal((peano(1000,N), intneg(positive(N),S))). 

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
%get_goal((peano(500000,N), \+ positive(N))).   
%get_goal((peano(1000000,N), \+ positive(N))). 

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
%get_goal((peano(700,N), cneg(positive(N)))).   
%get_goal((peano(1000,N), cneg(positive(N)))). 
%get_goal((peano(100000,N), cneg(add(X,Y,N)))). 
%get_goal((peano(100000,N), cneg(add(N,X,Y)))). 
get_goal((peano(100000,N), cneg(add(_X,N,_Y)))). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

peano(0,0).
peano(N,s(P1)):-
	N > 0,
	N1 is N-1,
	peano(N1,P1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%time(T).
%time_all(T).

% time(T) retuns the time spending in evaluate all solutions of a goal
time(T):-
	statistics(runtime,_),
	get_goal(G),
	(proof(G,1);true),
	statistics(runtime,T).

% time_all(T) retuns the time spending in evaluate all solutions of a goal
time_all(T):-
	statistics(runtime,_), 
	get_goal(G),
	(proof(G,1),fail;true),
	statistics(runtime,T).

% proof(Goal,N) calls the goal Goal N times
proof(_Goal,0). 
proof(Goal,N):-
         N > 0,
         call(Goal),
         N1 is N - 1,
         proof(Goal,N1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%% THE END %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


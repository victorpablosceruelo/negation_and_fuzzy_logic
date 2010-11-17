% File : count_time.pl            main call:      time(T).
%                                                 time_all(T).

:- module(_, _,[]).

:- use_module(queensPeano).

:- use_module(library(prolog_sys)).  

:- meta_predicate get_goal(goal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Proofs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%get_goal(less(0,s(s(0)))).
%get_goal(\+ less(0,s(s(0)))).
%get_goal(intneg(less(0,s(s(0))))).
%get_goal(cneg(less(0,s(s(0))))).

%get_goal(less(s(s(0)),0)).
%get_goal(\+ less(s(s(0)),0)).
%get_goal(intneg(less(s(s(0)),0))).
%get_goal(cneg(less(s(s(0)),0))).

%get_goal(less(X,s(s(0)))).
%get_goal(\+ less(X,s(s(0)))).
%get_goal(intneg(less(X,s(s(0))))).
%get_goal(cneg(less(X,s(s(0))))).

%get_goal(less(s(s(0)),X)).
%get_goal(\+ less(s(s(0)),X)).
%get_goal(intneg(less(s(s(0)),X))).
%get_goal((peano(500000,N),cneg(less(_X,N)))).
%get_goal((peano(500000,N),cneg(less(N,X)))).

%get_goal(ancestor(X,susan)).
%get_goal(\+ ancestor(X,susan)).
%get_goal(intneg(ancestor(X,susan))).
%get_goal(cneg(ancestor(X,susan))).

%get_goal(ancestor(peter,X)).
%get_goal(\+ ancestor(peter,X)).
%get_goal(intneg(ancestor(peter,X))).
%get_goal(cneg(ancestor(peter,X))).

%get_goal(boole(1)).
%get_goal(\+ boole(1)).
%get_goal(intneg(boole(0))).
%get_goal(cneg(boole(1))).

%get_goal(boole(_X)).
%get_goal(\+ boole(_X)).
%get_goal(intneg(boole(_X))).
%get_goal(cneg(boole(_X))).

%get_goal(binary_list(X)).
%get_goal(\+ binary_list(X)).
%get_goal(intneg(binary_list(X))).
%get_goal(cneg(binary_list(X))).

%get_goal(binary_list([0,1,1,0,1])).
%get_goal(\+ binary_list([0,1,1,0,1])).
%get_goal(intneg(binary_list([0,1,1,0,1]))).
%get_goal(cneg(binary_list([0,1,1,0,1]))).

%get_goal(natural(X)).
%get_goal(\+ natural(X)).
%get_goal(intneg(natural(X))).
%get_goal(cneg(natural(X))).

%get_goal(natural(s(s(s(s(0)))))).
%get_goal(\+ natural(s(s(s(s(0)))))).
%get_goal(intneg(natural(s(s(s(s(0))))))).
%get_goal(cneg(natural(s(s(s(s(0))))))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%get_goal((peano(500000,N), natural(N))).
%get_goal((peano(500000,N), \+ natural(N))).
%get_goal((peano(500000,N), intneg(natural(N)))). 
%get_goal((peano(500000,N), cneg(natural(N)))).

%get_goal((peano(500,N), positive(N))).
%get_goal((peano(500,N), \+ positive(N))).
%get_goal((peano(500,N), intneg(positive(N)))).
get_goal((peano(500,N), cneg(positive(N)))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

peano(0,0).
peano(N,s(P1)):-
	N > 0,
	N1 is N-1,
	peano(N1,P1).

%get_goal((peano(500000,N), less(N,N))).
%get_goal((peano(500000,N), \+ less(N,N))).
%get_goal((peano(500000,N), intneg(less(N,N)))).
%get_goal((peano(500000,N), cneg(less(N,N)))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%time(T).
%time_all(T).


% get_time(T) retuns the time spending in evaluate one solution of a goal
get_time(T):-
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


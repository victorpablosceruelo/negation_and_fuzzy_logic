:- module(ancestor,_,[assertions,.(cnegt)]).

:- use_module(library(prolog_sys)).  

:- meta_predicate get_goal(goal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parent(lucas,maria).
parent(lucas,pedro).
parent(maria,juan).
parent(pedro,susana).

person(lucas).
person(maria).
person(pedro).
person(juan).
person(susana).

ancestor(X,Y):-
	parent(X,Y).
ancestor(X,Y):-
	parent(Z,Y),
	ancestor(X,Z).

%%%%%%%%%%%% Llamada a las pruebas para cneg

cneg_ancestor(X,Y):- cneg(ancestor(X,Y)).


%%%%%%%%%%%% Llamada a las pruebas para cnegt

cnegt_ancestor(X,Y):- cnegt(ancestor(X,Y)).


%%%%%%%%%%%% Tipado de los argumentos para ancestor/2

:- success ancestor(A,N) => (person(A),person(N)).


%%%%%%%%%%% Medicion de tiempos de ejecucion

%%%%%%%%%%% Goals

%get_goal(ancestor(X,maria)).
%get_goal(cneg(ancestor(X,maria))).
get_goal(cnegt(ancestor(X,maria))).

% time_exe(T) retuns the time spending in evaluate all solutions of a goal
time_exe(T):-
	statistics(runtime,_),
	get_goal(G),
	(proof(G,1);true),
	statistics(runtime,T).  %[_|T]).
        %format("Used ~d milliseconds~n", T).

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

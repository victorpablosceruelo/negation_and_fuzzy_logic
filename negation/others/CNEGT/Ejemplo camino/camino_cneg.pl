:- module(camino_cneg,_,[assertions,.(cnegt)]).

:- use_module(library(prolog_sys)).  

:- meta_predicate get_goal(goal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nodo(a).
nodo(b).
nodo(c).
nodo(d).
nodo(e).
nodo(f).

camino([]).
camino([X|C]):-
	nodo(X),
	camino(C).

conecta(a,b).
conecta(a,c).
conecta(b,d).
conecta(c,d).
conecta(d,e).
conecta(d,f).

recorrido(Y,Y,[Y]).
recorrido(X,Y,[X|C]):-
	conecta(X,Z),
	recorrido(Z,Y,C).

pasa_por(X,[X|C]).
pasa_por(X,[Y|C]):-
	pasa_por(X,C).

%%%%%%%%%%%% Llamada a las pruebas para cneg

cneg_pasa_por(X,Y):- cneg(pasa_por(X,Y)).

%%%%%%%%%%%% Llamada a las pruebas para cnegt

cnegt_pasa_por(X,Y):- cnegt(pasa_por(X,Y)).

%%%%%%%%%%%% Tipado de los argumentos para cnegt

:- success pasa_por(NodoInter,Camino) => (nodo(NodoInter)).

%%%%%%%%%%% Medicion de tiempos de ejecucion

%%%%%%%%%%% Goals

%get_goal(pasa_por(X,[a,b,d,e])).
%get_goal(cneg(pasa_por(X,[a,b,d,e]))).
get_goal(cnegt(pasa_por(X,[a,b,d,e]))).


% time_exe(T) retuns the time spending in evaluate all solutions of a goal
time_exe(T):-
	statistics(runtime,_),
	get_goal(G),
%intnegt_alumnos(X),
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

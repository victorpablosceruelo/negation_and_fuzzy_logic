:- module(par,_,[assertions,.(cnegt)]).

:- use_module(library(prolog_sys)).  

:- meta_predicate get_goal(goal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

par(0).   %% cnegf
par(s(s(X))):-
	par(X).
parN(0).   % no cnegf
parN(s(s(X))):-
	parN(X).

tipoNat(0).
tipoNat(s(X)):- tipoNat(X).

tipoDigito(0).
tipoDigito(s(0)).
tipoDigito(s(s(0))).
tipoDigito(s(s(s(0)))).
tipoDigito(s(s(s(s(0))))).
tipoDigito(s(s(s(s(s(0)))))).
tipoDigito(s(s(s(s(s(s(0))))))).
tipoDigito(s(s(s(s(s(s(s(0)))))))).
tipoDigito(s(s(s(s(s(s(s(s(0))))))))).
tipoDigito(s(s(s(s(s(s(s(s(s(0)))))))))).


%%%%%%%%%%%% Llamada a las pruebas para cneg

cneg_par(X):- cneg(par(X)).
cneg_parN(X):- cneg(parN(X)).

%%%%%%%%%%%% Llamada a las pruebas para cnegt

cnegt_par(X):- cnegt(par(X)).
cnegt_parN(X):- cnegt(parN(X)).

%%%%%%%%%%%% Tipado de los argumentos para cnegt

:- success par(X) => (tipoDigito(X)).
:- success parN(X) => (tipoNat(X)).

% %%%%%%%%%%% Medicion de tiempos de ejecucion

% %%%%%%%%%%% Goals

%get_goal(par(X)).
%get_goal(cneg(par(X))).
get_goal(cnegt(par(X))).


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

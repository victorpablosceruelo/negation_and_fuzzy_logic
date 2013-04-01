:- module(_nqueens, _ , [assertions]).

%:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(neg,[neg/1]).
:- use_module(cnegf,[cnegf/1]).




:- trust success neg(X) => true.
:- entry queens(N,X) : (ground(N),list(X,num)).
%:- trust success cnegf(X) => true.


select(X,[X|Xs],Xs).
select(X,[Y|Ys],[Y|Zs]) :- select(X,Ys,Zs).


%genera una lista de enteros en un rango dado M -> N
range(M,N,[M|Ns]) :- M < N, M1 is M+1, range(M1,N,Ns).
range(N,N,[N]).


%permutaciones(Xs,[Z|Zs]) :-  select(Z,Xs,Ys), permutaciones(Ys,Zs).
%permutaciones([],[]).

%determina posiciones seguras
%safe([Q|Qs]) :-  safe(Qs), neg(attack(Q,Qs)).
%safe([]).

%determina si una reina ataca a otra
attack(X,Xs) :- attacka(X,1,Xs).

attacka(X,N,[Y|_Ys]) :- X is Y+N ; X is Y-N.
attacka(X,N,[_Y|Ys]) :- N1 is N+1, attacka(X,N1,Ys).


%resuelve el problema de las N reinas
queens(N,Qs) :- range(1,N,Ns), queensa(Ns,[],Qs).
queensa(UnplacedQs,SafeQs,Qs) :-
	select(Q,UnplacedQs,UnplacedQs1),
	neg(attack(Q,SafeQs)),
	queensa(UnplacedQs1,[Q|SafeQs],Qs).
queensa([],Qs,Qs).

%not_queens(N,Qs) :-  cnegf(queens(N,Qs)).

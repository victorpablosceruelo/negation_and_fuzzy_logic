 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               EXAMPLES  FOR PACKAGE INTNEG                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- module(proof_intneg,_,[.(intneg),.(cneg)]).
% :- module(proof_intneg,_,[.(cneg)]).
% :- use_module(forall,[]). 
:- module(proof_intneg,_,[intneg]).

peano(0,0).
peano(N,s(P1)):-
	N > 0,
	N1 is N-1,
	peano(N1,P1).

%less(X,Y) cierto siempre que X sea menor estricto que Y.
less(0,s(X)). %:- number1(X).
less(s(X),s(Y)):-
	less(X,Y).

%boole(X) cierto siempre que X sea un numero booleano.
boole(0). 
boole(1).

%parent(X,Y) hechos en los que el X es padre de Y.
parent(john, mary).
parent(john, peter).
parent(mary, joe).
parent(peter, susan).

%ancetor(X,Y) cierto siempre que X sea antecesor de Y.
ancestor(X, Y):-
 	parent(X, Y).
ancestor(X, Y):-
	parent(Z, Y),
	ancestor(X, Z).

%positive(X) cierto si X es un numero positivo.
positive(s(0)).
positive(s(X)):-
	positive(X).

%add(X,Y,Z) devuelve en Z la suma entre los numeros X e Y.
add(0,X,X). 
add(s(X),Y,s(Z)):-  
	add(X,Y,Z).

%%%%%%%%%%%%%%%%%%%%%% C A L L S 

%call_g:- boole(9).
%call_naf:- \+ boole(9).
%call_cneg:- cneg(boole(9)).

call_g:- peano(500,N), positive(N).
call_naf:- peano(500,N),\+ positive(N).
% call_cneg:- peano(500,N), cneg(positive(N)).





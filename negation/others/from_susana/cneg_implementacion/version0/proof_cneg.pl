
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               EXAMPLES  FOR PACKAGE CNEG                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(proof_cneg,_,[]).

%:- module(proof_cneg,[odd/1,not_odd_number/1],[cneg]).

:- use_package(.(cneg)). 

%:- module(proof_cneg,_,[cneg]).
:- use_module(dist,[dist/2]).

odd(s(0)).
odd(s(s(X))):-
	odd(X).

not_odd_number(X):-
	cneg(odd(X)).  

parent(d,r).
parent(r,s).
grandparent(X,Y):- 
	parent(X,Z),
	parent(Z,Y).

boole(0).
boole(1).

binary_list([]).
binary_list([Y|L]):-
	boole(Y),
	binary_list(L).

dist_list([]).
dist_list([(X,Y)|L]):-
	dist(X,Y),
	dist_list(L).

p(1,2,X,X):- 
	dist(X,3),
	dist(X,Z),
	q(Z).

q(Z):- dist(Z,0).
q(Z):- r(Z,_W).

r(8,9).

positive(0).
positive(s(X)):-
	positive(X).

natural(X):-
	dist(X,0),
	positive(X).


no_boole(X):- cneg(boole(X)).
no_binary_list(X):-cneg(binary_list(X)).
no_dist_list(X):-cneg(dist_list(X)).
no_p(V1,V2,VX,VY):-cneg((V1,V2,VX,VY)).
no_positive(X):-cneg(positive(X)).
no_natural(X):-cneg(natural(X)).
no_parent(X,Y):-cneg(parent(X,Y)).
no_grandparent(X,Y):-cneg(grandparent(X,Y)).
 
%no_:-cneg().
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




% %%Esto luego lo quito pq lo genera la expansion
% stored_clause(odd(s(0)),[]).
% stored_clause(odd(s(s(X))),[odd(X)]).

% stored_clause(not_odd_number(X),[cneg(odd(X))]).

% stored_clause(parent(d,r),[]).
% stored_clause(parent(r,s),[]).
% stored_clause(grandparent(X,Y),[parent(X,Z),parent(Z,Y)]).

% stored_clause(boole(0),[]).
% stored_clause(boole(1),[]).

% stored_clause(binary_list([]),[]).
% stored_clause(binary_list([Y|L]),[boole(Y),binary_list(L)]).

% stored_clause(dist_list([]),[]).
% stored_clause(dist_list([(X,Y)|L]),[dist(X,Y),dist_list(L)]).

% stored_clause(positive(0),[]).
% stored_clause(positive(s(X)),[positive(X)]).

% stored_clause(natural(X),[dist(X,0),positive(X)]).

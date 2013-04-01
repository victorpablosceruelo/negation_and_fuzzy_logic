 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               EXAMPLES  FOR PACKAGE CNEG                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(proof_cnegf,_,[.(cnegf)]).
  
%:- module(proof_cneg,[odd/1,not_odd_number/1],[cneg]).

%:- use_module(dist,[dist/2]). 

%:- use_package(.(cneg)). 
%:-include(cneg). 
   

odd(s(0)).
odd(s(s(X))):-
	odd(X).
  

parent(a1,p). 
parent(a2,p). 
parent(p,h1).
parent(p,h2).

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

p1(X):- 
	dist(X,3),
	dist(X,Z),
	q(Z).
 
p2(X):- 
	dist(X,3),
	dist(X,_Z).
p3(X):- 
	dist(X,3),
	dist(X,5).

p4(X,Y):- 
	dist(X,3),
	dist(Y,5).
 
q(Z):- dist(Z,0).
q(Z):- r(Z,_W). 

r(8,9). 

positive(0). 
positive(s(X)):-
	positive(X).  

natural(X):-
	dist(X,0), 
	positive(X). 
 
pred2(7,_). 
pred2(9,Y):- 
	dist(Y,5),  
	pred1(_X).  
pred1(2).

iguales(X,X).

r1(1,1).
r1(2,2).

r2(X,Y):-
	r(X,Y).
r3(1,2).
r3(2,3).

r4(X,Y):-
	r3(X,Z),
	r3(Z,Y).

r5(X,Y,Z):-
	r3(X,Y),
	r3(Y,Z).
  
no_odd_number(X):- cnegf(odd(X)). 
no_boole(X):- cnegf(boole(X)).
no_binary_list(X):-cnegf(binary_list(X)). 
no_dist_list(X):-cnegf(dist_list(X)).
no_p(V1,V2,VX,VY):-cnegf(p(V1,V2,VX,VY)). 
no_q(Z):-cnegf(q(Z)).
no_r(X,Y):-cnegf(r(X,Y)). 
no_p1(X):-cnegf(p1(X)). 
no_p2(X):-cnegf(p2(X)).
no_p3(X):-cnegf(p3(X)).
no_p4(X,Y):-cnegf(p4(X,Y)).
no_positive(X):-cnegf(positive(X)).
no_natural(X):-cnegf(natural(X)).
no_parent(X,Y):-cnegf(parent(X,Y)). 
no_grandparent(X,Y):-cnegf(grandparent(X,Y)).
no_pred2(X,Y):-cnegf(pred2(X,Y)).
no_pred1(X):-cnegf(pred1(X)).
no_iguales(X,Y):-cnegf(iguales(X,Y)).  
no_r1(X,Y):-cnegf(r1(X,Y)).
no_r2(X,Y):-cnegf(r2(X,Y)).
no_r3(X,Y):-cnegf(r3(X,Y)).
no_r4(X,Y):-cnegf(r4(X,Y)).
no_r5(X,Y,Z):-cnegf(r5(X,Y,Z)).


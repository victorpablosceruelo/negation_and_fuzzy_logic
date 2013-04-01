 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               EXAMPLES  FOR PACKAGE CNEG                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(proof_cneg,_,[.(cneg)]).
  
%:- module(proof_cneg,[odd/1,not_odd_number/1],[cneg]).

%:- use_module(dist,[dist/2]). 

%:- use_package(.(cneg)). 
%:-include(cneg). 
   

odd(s(0)).
odd(s(s(X))):-
	odd(X).
  
not_odd_number(X):- 
	cneg(odd(X)).   

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
 
no_boole(X):- cneg(boole(X)).
no_binary_list(X):-cneg(binary_list(X)). 
no_dist_list(X):-cneg(dist_list(X)).
no_p(V1,V2,VX,VY):-cneg(p(V1,V2,VX,VY)). 
no_q(Z):-cneg(q(Z)).
no_r(X,Y):-cneg(r(X,Y)). 
no_p1(X):-cneg(p1(X)). 
no_p2(X):-cneg(p2(X)).
no_p3(X):-cneg(p3(X)).
no_p4(X,Y):-cneg(p4(X,Y)).
no_positive(X):-cneg(positive(X)).
no_natural(X):-cneg(natural(X)).
no_parent(X,Y):-cneg(parent(X,Y)). 
no_grandparent(X,Y):-cneg(grandparent(X,Y)).
no_pred2(X,Y):-cneg(pred2(X,Y)).
no_pred1(X):-cneg(pred1(X)).
  




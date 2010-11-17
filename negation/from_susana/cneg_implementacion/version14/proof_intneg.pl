 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               EXAMPLES  FOR PACKAGE INTNEG                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(proof_intneg,_,[.(intneg)]).
  
%:- use_module(dist,[dist/2]). 

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
 
no_boole(X):- intneg(boole(X)).
no_binary_list(X):-intneg(binary_list(X)). 
no_dist_list(X):-intneg(dist_list(X)).
no_p(V1,V2,VX,VY):-intneg(p(V1,V2,VX,VY)). 
no_q(Z):-intneg(q(Z)).
no_r(X,Y):-intneg(r(X,Y)). 
no_p1(X):-intneg(p1(X)). 
no_p2(X):-intneg(p2(X)).
no_p3(X):-intneg(p3(X)).
no_p4(X,Y):-intneg(p4(X,Y)).
no_positive(X):-intneg(positive(X)).
no_natural(X):-intneg(natural(X)).
no_parent(X,Y):-intneg(parent(X,Y)). 
no_grandparent(X,Y):-intneg(grandparent(X,Y)).
no_pred2(X,Y):-intneg(pred2(X,Y)).
no_pred1(X):-intneg(pred1(X)).
  




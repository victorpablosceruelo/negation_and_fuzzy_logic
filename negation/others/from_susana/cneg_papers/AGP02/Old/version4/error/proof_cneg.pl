:- module(proof_cneg,_,[.(cneg)]).
  
%:- use_package(.(cneg)).
 
%:- use_module(dist,[dist/2]). 

q(Z):- dist(Z,0).
q(Z):- r(Z,_W). 

r(8,9).
 
no_q(Z):- cneg(q(Z)).
no_r(X,Y):- cneg(r(X,Y)).

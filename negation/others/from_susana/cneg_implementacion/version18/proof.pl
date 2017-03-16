:- module(proof,_,[.(pack)]).
  
%:- use_module(dist,[dist/2]). 


p(X):- 
	dist(X,3),
	dist(X,Z),
	q(Z).
 
q(Z):- pack(Z).
  
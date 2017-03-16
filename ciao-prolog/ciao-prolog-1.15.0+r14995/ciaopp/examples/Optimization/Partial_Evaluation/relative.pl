:- module( _, [query/1] , [assertions] ).


relative(X,Y) :- ancestor(Z,X), ancestor(Z,Y).
 
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z),ancestor(Z,Y).
 
parent(X,Y) :- father(X,Y).
parent(X,Y) :- mother(X,Y).
 
father(jap,carol).
father(jap,jonas).
father(jonas,maria).
mother(carol,paulina).
mother(carol,albertina).
mother(albertina,peter).
mother(maria,mary).
mother(maria,jose).
mother(mary,anna).
mother(mary,john).

query( X ) :- relative(john,X).

% rrtq(relative(john,peter))



% relative__1(anna).
% relative__1(john).
% relative__1(carol).
% relative__1(jonas).
% relative__1(paulina).
% relative__1(albertina).
% relative__1(peter).
% relative__1(maria).
% relative__1(mary).
% relative__1(jose).

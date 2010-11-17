:- module(ex_intneg_01, _, [.(intneg)]). 
% use [.(intneg)] for local calls.
% :- module('ex_intneg_01',[p/1,q/1,r/1,s/2,z/1]).

p(0).
p(s(X)) :- p(X).

q(X) :- p(X) ; p(s(X)).
r(X) :- p(X) , q(s(X)).

s(X,Y) :- p(X), q(Y).

% Exists -> Universal
t(X) :- s(X,Z). 

% Overlapping clauses.
u(0, X) :- p(X).
u(X, 0) :- p(X).
u(X, Y) :- p(X), p(Y).
 
% Special overlapping clauses.
v(0, X) :- p(X).
v(Y, X) :- p(X), p(Y).
v(1, X) :- p(X).

% Predicados problematicos ...
%p(s(X)) :- a(X,Y), b(Y,X).
%p(f(X)) :- a(X,f(Y)), p(Y).

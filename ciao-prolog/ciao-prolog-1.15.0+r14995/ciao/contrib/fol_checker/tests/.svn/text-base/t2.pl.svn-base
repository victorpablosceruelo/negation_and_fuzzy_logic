% Example 2 (predicate Horn clauses with constants)
:- module(t2, [parent/2,ancestor/2], [fol_checker]).

person(donald).
person(marshall).
person(oystein).
person(thoralf).

animal(dog).
animal(bird).

% :- check inmodel parent(X,Y) :: person(X), person(Y).

:- prove parent(X,Y) => person(X), person(Y).
parent(donald, marshall).
parent(marshall, oystein).
parent(oystein, thoralf).

:- prove ancestor(X,Y) => person(X), person(Y).
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Z) :- parent(X,Y), ancestor(Y,Z).

% This one could not be proved (it is false)
% TODO: prover times out; can we check that this is false?
:- prove ancestor(X,Y) => animal(X), person(Y).


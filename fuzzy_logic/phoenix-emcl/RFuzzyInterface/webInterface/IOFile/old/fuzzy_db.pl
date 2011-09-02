% Fuzzy concept generated from database 
expensive(FLAT_NUMBER,V) :- house(PRICE,_,_),expensive_func(PRICE,V).
cheap(FLAT_NUMBER,V) :- house(PRICE,_,_),cheap_func(PRICE,V).
small(FLAT_NUMBER,V) :- house(_,_,SIZE),small_func(SIZE,V).
% Fuzzy Concept Functions 
expensive_func(X,Y) :- X.>.500, X.=<.600, Y.=.X/600.
expensive_func(X,Y) :- X.>=.300, X.=<.400, Y.=.0.1.
% Neagtion Functions 
not_func(X,Y) :- X.>=.0, X.=<.1, Y.=.1-X.
something_func(X,Y) :- X.>=.0, X.=<.1, Y.=.1-X.
% Quantification Functions 
very_func(X,Y) :- X.>=.0, X.=<.0.5, Y.=.X.
very_func(X,Y) :- X.>=.0, X.=<.0.5, Y.=.X.
very_func(X,Y) :- X.>.0.5, X.=<.1, Y.=.1-X.

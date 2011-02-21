% Fuzzy concept generated from database 
expensive(HOUSE_CODE,V) :- house(HOUSE_CODE,_,_,_,PRICE,_,_),expensive_func(PRICE,V).
cheap(HOUSE_CODE,V) :- house(HOUSE_CODE,_,_,_,PRICE,_,_),cheap_func(PRICE,V).
% Fuzzy Concept Functions 
expensive_func(X,Y) :- X .>=. 50000, X .=<. 100000, Y .=. 0.000002*X-0.100000.
expensive_func(X,Y) :- X .>. 100000, X .=<. 250000, Y .=. 0.000001*X+0.033333.
expensive_func(X,Y) :- X .>. 250000, X .=<. 350000, Y .=. 0.000001*X-0.05.
expensive_func(X,Y) :- X .>. 350000, X .=<. 450000, Y .=. 0.000002*X-0.4.
expensive_func(X,Y) :- X .>. 450000, X .=<. 550000, Y .=. 0.000001*X+0.05.
expensive_func(X,Y) :- X .>. 550000, X .=<. 800000, Y .=. 0.38.
expensive_func(X,Y) :- X .>. 800000, X .=<. 1000000, Y .=. 0.000001*X+0.3.
expensive_func(X,Y) :- X .>. 1000000, X .=<. 1500000, Y .=. 0.6.
expensive_func(X,Y) :- X .>. 1500000, X .=<. 2500000, Y .=. 0.75.
cheap_func(X,Y) :- X .>. 0, X .=<. 30000, Y .=. 1.
cheap_func(X,Y) :- X .>. 30000, X .=<. 50000, Y .=. -0.000010*X+1.3.
cheap_func(X,Y) :- X .>. 50000, X .=<. 100000, Y .=. -0.000002*X+0.9.
% Neagtion Functions 
not_func(X,Y) :- X .>=. 0, X .=<. 1, Y .=. 1-X.
% Quantification Functions 
very_func(X,Y) :- X .>=. 0, X .=<. 1, Y .=. X*0.8.

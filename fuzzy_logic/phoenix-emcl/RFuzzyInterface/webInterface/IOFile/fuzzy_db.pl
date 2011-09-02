% Fuzzy concept generated from database 
expensive(HOUSE_CODE,V) :- house(HOUSE_CODE,_,_,_,PRICE,_,_),expensive_func(PRICE,V).
cheap(HOUSE_CODE,V) :- house(HOUSE_CODE,_,_,_,PRICE,_,_),cheap_func(PRICE,V).
big(HOUSE_CODE,V) :- house(HOUSE_CODE,_,SIZE,_,_,_,_),big_func(SIZE,V).
small(HOUSE_CODE,V) :- house(HOUSE_CODE,_,SIZE,_,_,_,_),small_func(SIZE,V).
% Fuzzy Concept Functions 
expensive_func(X,Y) :- X .>=. 50000, X .<. 100000, Y .=. 0.000002*X-0.1.
expensive_func(X,Y) :- X .>=. 100000, X .<. 250000, Y .=. 0.000001*X+0.033333.
expensive_func(X,Y) :- X .>=. 250000, X .<. 350000, Y .=. 0.000001*X-0.05.
expensive_func(X,Y) :- X .>=. 350000, X .<. 450000, Y .=. 0.000002*X-0.4.
expensive_func(X,Y) :- X .>=. 450000, X .<. 550000, Y .=. 0.000001*X+0.05.
expensive_func(X,Y) :- X .>=. 550000, X .<. 800000, Y .=. 0.38.
expensive_func(X,Y) :- X .>=. 800000, X .<. 1000000, Y .=. 0.000001*X.
expensive_func(X,Y) :- X .>=. 1000000, X .<. 1500000, Y .=. 0.6.
expensive_func(X,Y) :- X .>=. 1500000, X .=<. 2500000, Y .=. 0.75.
cheap_func(X,Y) :- X .>. 0, X .=<. 30000, Y .=. 1.
cheap_func(X,Y) :- X .>. 30000, X .=<. 50000, Y .=. -0.00001*X+1.3.
cheap_func(X,Y) :- X .>. 50000, X .=<. 100000, Y .=. -0.000002*X+0.9.
cheap_func(X,Y) :- X .>. 100000, X .=<. 250000, Y .=. -0.000001*X+0.83333.
cheap_func(X,Y) :- X .>. 250000, X .=<. 350000, Y .=. -0.000002*X+1.0.
cheap_func(X,Y) :- X .>. 350000, X .=<. 450000, Y .=. -0.000002*X+1.
cheap_func(X,Y) :- X .>. 450000, X .=<. 550000, Y .=. -0.000001*X+0.55.
big_func(X,Y) :- X .>. 0, X .=<. 50, Y .=. 0.002*X.
big_func(X,Y) :- X .>. 50, X .=<. 80, Y .=. 0.003333*X-0.066667.
big_func(X,Y) :- X .>. 80, X .=<. 120, Y .=. 0.0025*X.
big_func(X,Y) :- X .>. 120, X .=<. 200, Y .=. 0.00125*X+0.15.
big_func(X,Y) :- X .>. 200, X .=<. 300, Y .=. 0.001*X+0.2.
big_func(X,Y) :- X .>. 300, X .=<. 500, Y .=. 0.001*X+0.2.
big_func(X,Y) :- X .>. 500, X .=<. 1500, Y .=. 0.0002*X+0.6.
big_func(X,Y) :- X .>. 1500, X .=<. 2500, Y .=. 0.0001*X+0.75.
small_func(X,Y) :- X .>. 0, X .=<. 50, Y .=. 1.
small_func(X,Y) :- X .>. 50, X .=<. 80, Y .=. -0.00333*X+1.1665.
small_func(X,Y) :- X .>. 80, X .=<. 100, Y .=. -0.005*X+1.3.
small_func(X,Y) :- X .>. 100, X .=<. 150, Y .=. -0.002*X+1.
small_func(X,Y) :- X .>. 150, X .=<. 200, Y .=. -0.004*X+1.3.
small_func(X,Y) :- X .>. 200, X .=<. 300, Y .=. -0.003*X+1.1.
small_func(X,Y) :- X .>. 300, X .=<. 500, Y .=. -0.001*X+0.5.
% Neagtion Functions 
not_func(X,Y) :- X .>=. 0, X .=<. 1, Y .=. 1-X.
% Quantification Functions 
very_func(X,Y) :- X .>=. 0, X .=<. 1, Y .=. 0.8*X.

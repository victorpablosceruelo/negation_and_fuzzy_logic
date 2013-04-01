:-module(_, [par/1, no_par/1], []).

:- use_package(.(cnegf)).

par(2).
par(4).

no_par(4) :- par(2).

multi(0,22).
multi(_,Y) :- multi(0,Y).

finite(par,1).

prueba(A) :- neg(par(A)).

n_prueba(B) :- neg(no_par(B)).


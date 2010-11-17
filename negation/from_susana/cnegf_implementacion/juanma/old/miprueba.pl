:-module(miprueba, [par/1, no_par/1], []).

% AHORA
%:- use_package(.(cnegf)).

%cneg(_) :- true. 

par(2).
par(4).

no_par(4) :- par(2).

multi(0,22).
multi(_,Y) :- multi(0,Y).

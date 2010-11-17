
:-module(miprueba, [par/1, no_par/1], []).

% ANTES
%:- use_package(.(rmdel)).

% AHORA
:- use_package(.(cnegf)).

%cneg(_) :- true. 

par(2).
par(4).

no_par(4):- neg(s(0)).


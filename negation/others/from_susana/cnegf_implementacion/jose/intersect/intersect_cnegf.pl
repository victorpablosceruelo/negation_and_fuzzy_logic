:- module(_intersect,[not_intersect/3,intersect/3],[assertions]).
:- use_module(cnegf).

:- entry mimember(X,Y) : (num(X), ground(X), list(Y,num), ground(Y)).
mimember(H,[H|_]):-!.
mimember(H,[_|T]):- mimember(H,T).

% El resultado es la interseccion de las 2 listas que se le pasan
:- entry intersect(X,Y,Z) : (list(X,num), ground(X), list(Y,num), ground(Y), var(Z)).
intersect([],_,[]):-!.
intersect([X|T],L,[X|R]):- mimember(X,L),intersect(T,L,R),!.
intersect([_|T],L,R):- intersect(T,L,R).

not_intersect(X,Y,Z) :- cnegf(intersect(X,Y,Z)).


:- module(pruebas, _,[]).


:- use_module(dist,[dist/2]).
:- use_module(cnegf,[cnegf/1]).

member1(X,[X|_]).
member1(X,[_Y|L]):- member1(X,L).

no_member(X,L):- cnegf(member1(X,L)).

abuelo(X,Y):- padre(X,Z),padre(Z,Y).

padre(abu,papa).
padre(papa,bebe).

natural(s(_X)).


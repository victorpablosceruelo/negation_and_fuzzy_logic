:- module(_factorial, _ , [assertions]).
:- use_module(cnegf,[cnegf/1]).

:- entry multiplica(X,Y,Z) : (num(X), ground(X), num(Y), ground(Y)).
multiplica(X,1,X) :- !.
multiplica(1,Y,Y) :- !.
multiplica(X,Y,Z) :- Y1 is Y - 1, multiplica(X,Y1,Z1),Z is X + Z1.


:- entry factorial(X,Y) : (num(X), ground(X)).
factorial(0,1) :- !.
factorial(X,Y) :- X1 is X -1, factorial(X1,Y1), multiplica(X,Y1,Y).

:- entry lista_factorial(X,Y) : (num(X), ground(X), var(Y)).
lista_factorial(1,[1]) :- !.
lista_factorial(N,[X|Xs]) :- factorial(N,X), N1 is N -1, lista_factorial(N1,Xs).

%:- entry numero_combinatorio(X,Y,Z) : (number(X), ground(X), number(Y), ground(Y), var(Z)).
numero_combinatorio(Superior,Inferior,Resultado) :- D is Superior - Inferior, factorial(Superior,FS), factorial(Inferior,FI), factorial(D,FD), Resultado is FS * FD / FI.

not_multiplica(X,Y,Z) :- cnegf(multiplica(X,Y,Z)).

not_factorial(X,Y) :- cnegf(factorial(X,Y)).

not_lista_factorial(X,Y) :- cnegf(lista_factorial(X,Y)).

not_numero_combinatorio(X,Y,Z) :-  cnegf(numero_combinatorio(X,Y,Z)).



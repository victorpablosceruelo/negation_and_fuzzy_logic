:- module(_,[main/1],[]).

:- use_package(assertions).

%:- entry main(,L,[X,Y]).

main(L1) :- mymember(X,L1), p(Y), q(X,Y).


mymember(X,[X|_T]).
mymember(X,[_Y|T]) :- mymember(X,T).

p(a).
p(b).
p(c).

q(X,X).

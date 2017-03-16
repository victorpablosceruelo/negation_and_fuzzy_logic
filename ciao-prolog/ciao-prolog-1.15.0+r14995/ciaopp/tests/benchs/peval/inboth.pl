:- module(_,[inboth/3],[]).

:- use_package(assertions).

:- entry inboth(a,L,[X,Y]).

inboth(X,L1,L2) :- mymember(X,L1),
                   mymember(X,L2).

mymember(X,[X|_T]).
mymember(X,[_Y|T]) :- mymember(X,T).

% To run in Ciao:
:- use_package(fd).

% EQ(X, Y) (X = Y, X == Y)
eq(X, Y):- X .=. Y.

% NEQ  (X \= Y)
neq(X, Y):- X .<>. Y.

% LEQ (X =< Y)
leq(X, Y):- X .=<. Y.

% LESS (X < Y)
less(X, Y):- X .<. Y.

% PLUS (X + Y = Z) 
plus(X, Y, Z):- Z .=. X + Y.

% MINUS (X - Y = Z) 
minus(X, Y, Z):- Z .=. X - Y.

% MULTIPLICATION (X * Y = Z) 
mult(X, Y, Z):- Z .=. X * Y.

% IN 
:- push_prolog_flag(multi_arity_warnings,off).
in(E,DL,DU):- E in DL..DU.
:- pop_prolog_flag(multi_arity_warnings).

% LABELING
do_label(L):- labeling(L).

:- impl_defined([xor/3, reif_eq/3, equiv/3]).

reif_leq(X, Y, leq(X,Y)).
or(A,B,1):- A ; B.

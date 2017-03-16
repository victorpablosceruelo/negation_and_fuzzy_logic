:- module(family, [grandfather/2], [assertions]).

:- entry grandfather(A,B): var * var.

grandfather(C,G):- father(C,P), father(P,G).
grandfather(C,G):- father(C,P), mother(P,G).

father(george,rose).

mother(rose,charles).
mother(mary,rose).

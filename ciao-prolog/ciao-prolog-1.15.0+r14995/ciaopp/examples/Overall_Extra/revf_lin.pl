:- module(revf_lin, [rev/2], [assertions,fsyntax,regtypes]).

:- entry rev/2 : {list, ground} * var.

rev(X) := ~rev_(X,[]).

:- entry rev_/3 : {ground, list} * list  * var.

rev_([],N)     := N.
rev_([H|T],NT) := ~rev_(T,[H|NT]).

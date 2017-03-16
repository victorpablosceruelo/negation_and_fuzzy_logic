:- module(revf_lin_assrt, [rev/2], [assertions,fsyntax]).
:- use_module(library(assertions(native_props))).

:- entry rev/2 : {ground, list} * var.

rev(X) := ~rev_(X,[]).

:- entry rev_/3 : {ground, list} * {ground, list} * var.

:- check comp rev_(A,_,_) + steps(length(A)+1).  

rev_([],N)     := N.
rev_([H|T],NT) := ~rev_(T,[H|NT]).

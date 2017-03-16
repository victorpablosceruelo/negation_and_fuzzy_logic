:- module(gecode_tr, [gecode_tr/2], []).
:- include(library(gecode(gecode_syntax))).
:- include(library(gecode(gecode_translation))).

gecode_tr((A .=. B),  parseGECODE(A .=. B)).
gecode_tr((A .>. B),  parseGECODE(A .>. B)).
gecode_tr((A .<. B),  parseGECODE(A .<. B)).
gecode_tr((A .=<. B), parseGECODE(A .=<. B)).
gecode_tr((A .>=. B), parseGECODE(A .>=. B)).
gecode_tr((A .<>. B), parseGECODE(A .<>. B)).

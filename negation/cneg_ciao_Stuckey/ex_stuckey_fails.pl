:- module(ex_stuckey_fails,_,[.(cneg), debugger_pkg]).


p(X) :- disequality(X, Z, []), equality(Z, 3, []). 
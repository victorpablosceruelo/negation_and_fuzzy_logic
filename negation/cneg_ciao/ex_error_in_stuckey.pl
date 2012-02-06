:- module(ex_error_in_stuckey,_,[.(cneg)]).
%:- module(ex_error_in_stuckey,_,[.(cneg), .(debugger_pkg)]).


p(X) :- disequality(X, Z, []), equality(Z, 3, []). 
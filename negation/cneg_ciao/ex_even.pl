:- module(ex_even,_,[.(cneg), .(debugger_pkg)]).
%:- module(even,_,[.(cneg)]).
%:- use_module(dist). 

even(0).
even(s(s(X))):- even(X).

%neg_even(X):- cneg([], even(X)).


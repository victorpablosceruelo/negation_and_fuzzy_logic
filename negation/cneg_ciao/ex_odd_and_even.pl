:- module(ex_odd_and_even,_,[.(cneg)]).
%:- module(ex_odd_and_even,_,[.(cneg), .(debugger_pkg)]).

even(0).
even(s(s(X))):- even(X).

odd(X):- cneg_rt([], even(X)).


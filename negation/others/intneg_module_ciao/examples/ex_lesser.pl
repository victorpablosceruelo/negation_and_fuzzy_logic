:- module(ex_lesser,_,[intneg]).

lesser(0,s(_Y)).
lesser(s(X),s(Y)):-lesser(X,Y).


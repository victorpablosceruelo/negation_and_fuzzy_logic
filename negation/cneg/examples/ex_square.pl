:- module(ex_square,_,[.(cneg)]).
%:- module(ex_square,_,[.(cneg), .(debugger_pkg)]).

square(X) :- nat(Y), prod(Y, Y, X).

nat(0).
nat(s(X)) :- nat(X).

prod(0, _Y, 0).
prod(s(0), Y, Y).
prod(s(s(X)), Y, Z) :- prod(s(X), Y, W), add(W, Y, Z).

add(0, Y, Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).
:- module(app,[append/3],[assertions]).

:- entry append(A,[B],C) : list(A).

append([], L, L).
append([E|Es], L, [E|R]) :- append(Es, L, R).
